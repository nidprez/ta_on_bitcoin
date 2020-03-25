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

x <- c(0.05,0.1,0.5,1,5)
d <- c(1:3)
j <- c(5,10,15,20,25,50,100,200)
c <- c(0.1,0.5,1,5,10)
k <- c(0,1,5,10,25)

CB_rules <- expand.grid(x = x, d = d, j = j, k = k, c = c, KEEP.OUT.ATTRS = F)

rownames(CB_rules) <- NULL

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


calculate_CB <- function(Low, High, x, d, k, c, P){
  x <- x/100
  c <- c/100
  
  
  N <- length(Low)
  #Determine where P is below Low and above High
  Rule2 <- ifelse(High <= ((1+c)*Low), 
                  ifelse(P <= ((1 - x) * Low), -1, 
                         ifelse( P >= ((1 + x) * High), 1, 0)),
                  0)
  
  Rule3 <- dplyr::lag(Rule2, 1)
  Index <- which((Rule3 == 1 | Rule3 == -1) & (Rule3 != Rule2)) #determine which periods are just after below/above periods
  rle_rule <- rle(Rule2)
  Index2 <- cumsum(rle_rule$lengths) + 1 #indices of periods after a change in value
  Index3 <- (Index2 %in% Index) & (rle_rule$lengths >= d) #determine which overbought/sold periods went on longer then d
  Index4 <- Index2[Index3] - (rle_rule$lengths[Index3] - d) - 1
  if(length(Index4) >0 ){
    
  
  rle_value <- rle_rule$values[Index3] 
  
  
  if(!!k){
    #check whether sum runs go longer then multiples of k, in that case the position doesn't need to be closed after k periods
    Index3_2 <- (Index2 %in% Index) & (rle_rule$lengths >= d) & (rle_rule$lengths >= (d + k))
    sum_ind3 <- sum(Index3_2)
    # Index3_3 <- (Index2 %in% Index) & (rle_rule$lengths >= d) & (rle_rule$lengths >= (d + 2*k))
    if(sum_ind3>0){
      
      i_2 <- 1
      while(sum_ind3[i_2]>0){
        i_2 <- i_2 + 1
        Index3_2 <- cbind(Index3_2, 
                          (Index2 %in% Index) & (rle_rule$lengths >= d) & 
                            (rle_rule$lengths >= (d + (i_2*k))))
        sum_ind3[i_2] <- sum(Index3_2[,i_2])
      }
      Index3_2 <- Index3_2[, - i_2]
      sum_ind3 <- sum_ind3[-i_2]
      if(length(sum_ind3)==1){
        Index3_2 <- matrix(Index3_2, ncol = 1)
      }
      for(i in 1:length(sum_ind3)){
        Index4 <- c(Index4, Index2[Index3_2[,i]] - (rle_rule$lengths[Index3_2[,i]] - d - (i*k)) - 1)
      }
      Index4 <- sort(Index4, decreasing = F)
      rle_value <- integer(sum(sum(sum_ind3), sum(Index3)))
      j <- 1
      for (i in seq_along(Index3)) {
        check <- F
        for (i_2 in length(sum_ind3):1) {
          if(Index3_2[i,i_2]){
            rle_value[j:(j+i_2)] <- rle_rule$values[i]
            j <- j + 1 + i_2
            check <- T
            break
          }
        }
        if(Index3[i] & !check){
          rle_value[j] <- rle_rule$values[i]
          j <- j + 1
        }
      }
    }
    
  }
  
  Rule <- rep(NA, N)
  Rule[Index4] <- rle_value
  
  cbind(Low, P, High, Rule, Rule2)[6:26,]
  
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
}else{
  Rule <- rep(0, N)
}
  return(Rule)
  
}

Rule_list <- lapply(seq_len(nrow(CB_rules)), function(l)CB_rules[l, ])


no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterExport(cl, list("High", "Low", "calculate_CB", "P"))
clusterEvalQ(cl, c(library(zoo), library(dplyr)))
print(paste("Calculating CB Rules at", Sys.time()))

Rules <- parLapply(cl, Rule_list, function(l)calculate_CB(Low[ ,as.character(l[3])], 
                                                          High[ ,as.character(l[3])], 
                                                          as.numeric(l[1]), 
                                                          as.numeric(l[2]), 
                                                          as.numeric(l[4]),
                                                          as.numeric(l[5]),
                                                          P))

print(paste("CB Rules Calculated at", Sys.time()))
stopCluster(cl)

print(paste("Saving CB Rules at", Sys.time()))
readr::write_csv(as.data.frame(do.call(cbind, Rules)), 
                 path = paste0(wdir, exchange, currency,"_", Freq, "CB_Rules.csv"))
print(paste("CB Rules Saved at", Sys.time()))

test <- sapply(1:8, function(l) ifelse(High[,l] <= 1.01*Low[,l], T, F))
