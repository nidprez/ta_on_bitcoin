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

data <- na.locf_bitcoincharts(data.table::fread(dir), F, F, F, F)
P <- data$Price
rm(data)

h <- c(5,10,15,20,25,50,100,150,200,250)
v <- c(10,15,20,25)
d <- c(1,2,5)
k <- c(0, 1,5,10,25)

RSI_rules <- expand.grid(h = h, v = v, d = d, k = k, KEEP.OUT.ATTRS = F)
rownames(RSI_rules) <- NULL


calculate_RSI <- function(RSI, v, d, k){
  
  N <- length(RSI)
  Rule <- rep(NA,N)
  
  Rule2 <- ifelse(RSI <= (50 - v) , 1, ifelse(RSI >= (50 + v), -1, 0)) #determine overbought/sold areas
  Rule3 <- dplyr::lag(Rule2, 1)
  Index <- which((Rule3 == 1 | Rule3 == -1) & (Rule3 != Rule2)) #determine periods which just after overbought/sold areas
  rle_rule <- rle(Rule2)
  Index2 <- cumsum(rle_rule$lengths) + 1 #indices of periods after a change in value
  Index3 <- (Index2 %in% Index) & (rle_rule$lengths >= d) #determine which overbought/sold periods went on longer then d
  Index4 <- Index2[Index3]
  rle_value <- rle_rule$values[Index3] 
  Rule[Index4] <- rle_value
  
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


#calculate all the possible RSI
RSI <- sapply(h, function(l){TTR::RSI(P, n = l)})
colnames(RSI) <- as.character(h)

# ** RSI Rules ####
Rule_list <- lapply(seq_len(nrow(RSI_rules)), function(l)RSI_rules[l, ])

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterExport(cl, list("RSI", "calculate_RSI"))
clusterEvalQ(cl, c(library(zoo), library(dplyr)))

#calculate the rules
print(paste("Calculating RSI at", Sys.time()))
Rules <- parLapply(cl, Rule_list , 
                   function(l){calculate_RSI(RSI[ , as.character(l[1])], 
                                             as.numeric(l[2]), 
                                             as.numeric(l[3]), 
                                             as.numeric(l[4]))})
stopCluster(cl)

print(paste("RSI calculated at", Sys.time()))
#save the rules in csv
print(paste("Saving RSI at", Sys.time()))
write_csv(as.data.frame(do.call(cbind, Rules)), 
          path = paste0(wdir, exchange, currency,"_", Freq, "RSI_Rules.csv"))
print(paste("RSI Rules Saved at", Sys.time()))

