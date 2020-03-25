gc()

#libraries ####
library(NiekPhD)
library(parallel)
library(tidyverse)

# General Document options ####
exchange <- "bitstamp"
currency <- "usd"
Freq <- "1hour"

dir <- paste0("C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Data/",
              exchange, "/", exchange, currency, "_", Freq, ".csv")
wdir <- paste0("C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Data/",
               exchange, "/")
# dir <- paste0("D:/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Data/", 
#               exchange, "/", exchange, currency, "_", Freq, ".csv")
# wdir <- paste0("D:/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Data/", 
#                exchange, "/")


# Import Data ####
data <- na.locf_bitcoincharts(data.table::fread(dir), F, F, F, F)
P <- data$Price
rm(data)

x <- c(0.005,0.01,0.5,1,5,10,20)
y <- c(0.005,0.01,0.5,1,5,10,20)
d <- c(0:5)
dx <- 0:5
dy <- 0:4
j <- c(1,2,5,10,20)
k <- c(0, 5,10,15,20,25)

filter_rules2 <- expand.grid(x = x, d = d, j = j, k = k, KEEP.OUT.ATTRS = F)


filter_rules <- expand.grid(x = x, y = y, dx = dx, dy =dy, j = j, KEEP.OUT.ATTRS = F)
filter_rules <- filter_rules[filter_rules$y < filter_rules$x, ]
filter_rules <- filter_rules[filter_rules$dy < filter_rules$dx, ]
# filter_rules <- filter_rules[(filter_rules$dy == 0 & filter_rules$d != 0) | (filter_rules$d == 0 & filter_rules$dy != 0), ]

rownames(filter_rules) <- NULL
rownames(filter_rules2) <- NULL

Low <- sapply(j, function(l){zoo::rollapply(P, l, 
                                            function(x) min(x, na.rm = T), 
                                            fill = NA, align = "right")})

Low <- apply(Low, 2 , dplyr::lag, 1)

High <- sapply(j, function(l){zoo::rollapply(P, l, 
                                             function(x) max(x, na.rm = T), 
                                             fill = NA, align = "right")})
High <- apply(High, 2 , dplyr::lag, 1)

colnames(Low) <- as.character(j)
colnames(High) <- as.character(j)

calculate_filter2 <- function(Low, High, x, d, k, P){
  x <- x/100
  N <- length(Low)
  
  
  #decrease k with 1 if larger than 0 in order to have the right holding period.
  #i:i + k is actually 1 period longer than k (for example: k = 1, i = 5 => 5:6 
  # is a holding period of 2 instead of 1)
  k <- ifelse(k > 0, k - 1, k)
  
  #Determine where P is below Low and above High
  Index1 <- which(P <= Low | P >= High)
  
  Low <- (1 + x) * Low
  High <- (1 - x) * High
  
  if(is.na(Low[Index1[1]-1])){
    Index1 <- Index1[-1]
  }
  
  
  #Predefine Rule for quickness
  Rule <- rep(NA, N)
  
  #A check whether the current trade point is overridden by a previous trade.
  #for example if k = 5 but another trade signal is given after 3 days, this 
  # signal can be ignored.
  test1 <- 0
  for(i in Index1){#loop over possible trading points
    i_1 <- i + 1
    if(i_1 + d > test1){ #actual check
      if( (i_1 + d + k) <= N){
        
        if(P[i] < Low[i] & all(P[i_1:(i_1+d)] >= Low[i_1])){
          Rule[(i_1+d):(i_1+d+k)] <- 1
          test1 <- i_1 + d + k
          
        }else if(P[i] > High[i] & all(P[i_1:(i_1+d)] <= High[i_1])){
          Rule[(i_1+d):(i_1+d+k)] <- -1
          test1 <- i_1 + d + k
        }
        
        
      }else if( (i_1 + d)<= N){
        
        if(P[i] < Low[i] & all(P[i_1:(i_1+d)] >= Low[i_1])){
          Rule[(i_1+d):N] <- 1
          test1 <- i_1 + d + k
          
        }else if(P[i] > High[i] & all(P[i_1:(i_1+d)] <= High[i_1])){
          Rule[(i_1+d):N] <- -1
          test1 <- i_1 + d + k
        }
        
      }
    }
  }
  
  
  if(!k){ 
    Rule <- zoo::na.locf0(Rule) #fill in the blanks
  }
  
  # cbind(Low, P, High, Rule)[1:100,]
  
  
  Rule <- dplyr::lag(Rule, 1) #adjust for advanced knowledge bias
  Rule[is.na(Rule)] <- 0
  
  return(Rule)
  
}


calculate_filter2_2 <- function(Low, High, x, d, k, P){
  x <- x/100
  N <- length(Low)
  # lag_P <- dplyr::lag(P, 1)
  #Determine where P is below Low and above High
  Rule2 <- ifelse(P <= ((1 + x) * Low), 1, 
                  ifelse( P >= ((1 - x) * High), -1, 0))
  
  Rule3 <- dplyr::lag(Rule2, 1)
  Index <- which((Rule3 == 1 | Rule3 == -1) & (Rule3 != Rule2)) #determine which periods are just after below/above periods
  rle_rule <- rle(Rule2)
  Index2 <- cumsum(rle_rule$lengths) + 1 #indices of periods after a change in value
  
  if(d > 1){
    #filter out false signals and add the length of the false signals to the previous run
    #example if d = 2 and Pcomes above low, nex period P goes below high for 1 period and then 
    #the next period above high again then high + the 1 period below high + the next period of
    #high should count for a buy signal
    
    #look for 0s which do not count as trading signal
    Ind_0 <- which(rle_rule$values == 0 & rle_rule$lengths < d)
    if(length(Ind_0)> 0){
      #for which of these false signals the previous and the next signal is the same?
      Ind_0_2 <- rle_rule$values[Ind_0 - 1] == rle_rule$values[Ind_0 + 1]
      Ind_0_2[is.na(Ind_0_2)] <- F
      
      #Add the length of the false signals to the length of the previous periods
      if(tail(Ind_0, 1) == length(rle_rule$lengths)){
        rle_rule$lengths[Ind_0 - 1] <- rle_rule$lengths[Ind_0 - 1] + rle_rule$lengths[Ind_0]
        rle_rule$lengths[Ind_0 - 1][-length(Ind_0)] <- rle_rule$lengths[Ind_0 - 1][-length(Ind_0)] +  
          rle_rule$lengths[Ind_0 + 1][-length(Ind_0)]*Ind_0_2[-length(Ind_0)]
        
      }else{
        rle_rule$lengths[Ind_0 - 1] <- rle_rule$lengths[Ind_0 - 1] + rle_rule$lengths[Ind_0] + 
          rle_rule$lengths[Ind_0 + 1]*Ind_0_2
      }
      
      rle_rule$lengths[Ind_0] <- 0
      rle_rule$lengths[Ind_0[Ind_0_2] + 1] <- 0
    }
    
    
  }
  
  
  Index3 <- (Index2 %in% Index) & (dplyr::lead(rle_rule$lengths, 1, F) >= d) #determine which overbought/sold periods went on longer then d
  Index4 <- Index2[Index3] + d - 1
  rle_value <- rle_rule$values[Index3] 
  Rule <- rep(NA, N)
  Rule[Index4] <- rle_value
  
  # cbind(Low, P, High, Rule, Rule2)[6:26,]
  
  if(!k){
    Rule <- zoo::na.locf0(Rule) #fill in the blanks
  }else{
    Index5 <- (dplyr::lag(Index4, 1, -k) + k - 1) >= Index4 #which trade signals are within a period where we already have a position
    Index5_2 <- (Index5 == dplyr::lag(Index5, 1, F)) & (Index5 == T) #do not delete rules which are within the period of a deleted rule 
    count <- sum(Index5_2)
    Index5[Index5_2]  <- F
    
    
    Index6 <- Index4[!Index5]
    Rule[Index4[Index5]] <- NA #delete trades that are overlapped by previous trades
    rle_value <- rle_value[!Index5] 
    
    #Iterative scheme to keep the right rules
    while(count > 0){
      Index7 <- (dplyr::lag(Index6, 1, -k) + k - 1) >= Index6 #which trade signals are within a period where we already have a position
      Index7_2 <- (Index7 == dplyr::lag(Index7, 1, F)) & (Index7 == T)
      count <- sum(Index7_2)
      Index7[Index7_2]  <- F
      
      
      Rule[Index6[Index7]] <- NA #delete trades that are overlapped by previous trades
      Index6 <- Index6[!Index7]
      rle_value <- rle_value[!Index7] 
    }
    
    
    for (i in seq_along(Index6)) {
      ind_i <- Index6[i]
      if(Rule[ind_i] == rle_value[i]){
        
        if((ind_i + k - 1) <= N){
          Rule[ind_i:(ind_i + k - 1)] <- rle_value[i]
        }else{
          Rule[ind_i:N] <- rle_value[i]
        }
        
      }
    }
  }
  Rule <- dplyr::lag(Rule, 1)
  Rule[is.na(Rule)] <- 0
  return(Rule)
  
}


Rule_list <- lapply(seq_len(nrow(filter_rules2)), function(l)filter_rules2[l, ])


no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterExport(cl, list("High", "Low", "calculate_filter2", "P"))
clusterEvalQ(cl, c(library(zoo), library(dplyr)))
print(paste("Calculating filter Rules at", Sys.time()))

Rules <- parLapply(cl, Rule_list, function(l)calculate_filter2(Low[ ,as.character(l[3])], 
                                                               High[ ,as.character(l[3])], 
                                                               as.numeric(l[1]), 
                                                               as.numeric(l[2]), 
                                                               as.numeric(l[4]),
                                                               P))

print(paste("filter Rules Calculated at", Sys.time()))
stopCluster(cl)

print(paste("Saving filter Rules at", Sys.time()))
readr::write_csv(as.data.frame(do.call(cbind, Rules)), 
                 path = paste0(wdir, exchange, currency,"_", Freq, "filter_Rules.csv"))
print(paste("filter Rules Saved at", Sys.time()))

