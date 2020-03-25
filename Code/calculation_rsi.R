######################
# Calculate RSI rules
######################
gc()

#libraries ####
library(NiekPhD)
library(parallel)
library(tidyverse)

# General Document options ####
# exchange <- "bitstamp"
# currency <- "usd"
# Freq <- "day"

wdir <- paste0(getwd(), "/Data/", exchange, "/")
filename <- paste0(exchange, currency, "_", Freq, ".csv")
if((filename %in% list.files(wdir))){ #test if the directory is right
  dir <- paste0(wdir, filename)
  
  
  data <- na.locf_bitcoincharts(data.table::fread(dir), F, F, F, F)
  P <- data$Price
  rm(data)
  
  h <- c(5,10,14,20,25,50,100,150,200,250)
  v <- c(10,15,20,25,30)
  d <- c(1,2,5)
  k <- c(0, 1,5,10,25)
  
  RSI_rules <- expand.grid(h = h, v = v, d = d, k = k, KEEP.OUT.ATTRS = F)
  rownames(RSI_rules) <- NULL
  
  calculate_RSI <- function(RSI, v, d, k){
    
    N <- length(RSI)
    Rule <- rep(NA,N)
    
    ub <- 50 + v
    lb <- 50 - v
    
    #decrease k with 1 if larger than 0 in order to have the right holding period.
    #i:i + k is actually 1 period longer than k (for example: k = 1, i = 5 => 5:6 
    # is a holding period of 2 instead of 1)
    k <- ifelse(k > 0, k - 1, k)
    
    #overbought and oversold periods
    Index1 <- which(RSI >= ub | RSI <= lb)
    
    while (is.na(RSI[Index1[1]-d])) {
      Index1 <- Index1[-1]
    }
    
    
    #Predefine Rule for quickness
    Rule <- rep(NA, N)
    
    #A check whether the current trade point is overridden by a previous trade.
    #for example if k = 5 but another trade signal is given after 3 days, this 
    # signal can be ignored.
    test1 <- 0
    for(i in Index1){#loop over possible trading points
      
      if(i > test1){ #actual check
        if( (i + 1 + k) <= N){
          
          if(all(RSI[(i - d):i] >= ub) & RSI[i + 1] < ub){
            Rule[(i + 1):(i + 1 + k)] <- -1
            test1 <- i + 1 + k
            
          }else if(all(RSI[(i - d):i] <= lb) & RSI[i + 1] > lb){
            Rule[(i + 1):(i + 1 + k)] <- 1
            test1 <- i + 1 + k
          }
          
          
        }else if((i + 1) <= N){
          
          if(all(RSI[(i - d):i] >= ub) & RSI[i + 1] < ub){
            Rule[(i + 1):N] <- -1
            test1 <- i + 1 + k
            
          }else if(all(RSI[(i - d):i] <= lb) & RSI[i + 1] > lb){
            Rule[(i + 1):N] <- 1
            test1 <- i + 1 + k
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
  
}else{
  print("ERROR: Data was not found in the working directory")
  print(dir)
  print(paste("calculation unsuccesful for CB Rules at", Sys.time()))
}