########################################
# Calculate Channel breakout trading rules
########################################

gc()

#libraries ####
library(NiekPhD)
library(parallel)
library(tidyverse)

# General Document options ####
# exchange <- "bitstamp"
# currency <- "usd"
# Freq <- "day"

wdir <- paste0(getwd(), "/Data/", exchange, "/", Freq, "/")
filename <- paste0(exchange, currency, "_", Freq, ".csv")
if((filename %in% list.files(wdir))){ #test if the directory is right
  dir <- paste0(wdir, filename)
  
  
  # Import Data ####
  data <- na.locf_bitcoincharts(data.table::fread(dir), F, F, F, F)
  P <- data$Price
  rm(data)
  
  x <- c(0.05,0.1,0.5,1,5)
  d <- c(0:2)
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
    
    #decrease k with 1 if larger than 0 in order to have the right holding period.
    #i:i + k is actually 1 period longer than k (for example: k = 1, i = 5 => 5:6 
    # is a holding period of 2 instead of 1)
    k2 <- k
    k <- ifelse(k > 0, k - 1, k)
    
    ub <- (1 + x) * (1 + c) * Low
    lb <- (1 - x) * (1 - c) * High
    
    # Index1 <- which((High <= ((1 + c) * Low)))
    
    #Determine where P is below Low and above High
    Index1 <- which((High <= ((1 + c) * Low)) & (P >= ub | P <= lb))
    
    if(length(Index1) == 0){
      Rule <- rep(0, N)
      return(Rule)
    }
    
    # test <- cbind(Low, P, High, ub, lb, Rule)
    
    if(is.na(lb[Index1[1]-1])){
      Index1 <- Index1[-1]
    }
    
    
    #Predefine Rule for quickness
    Rule <- rep(NA, N)
    
    #A check whether the current trade point is overridden by a previous trade.
    #for example if k = 5 but another trade signal is given after 3 days, this 
    # signal can be ignored.
    test1 <- 0
    for(i in Index1){#loop over possible trading points
      
      if(i + d > test1){ #actual check
        if( (i + d + k) <= N){
          
          if(P[i - 1] >= lb[i - 1] & all(P[i:(i+d)] <= lb[i])){
            Rule[(i+d):(i+d+k)] <- -1
            test1 <- i + d + k
            
          }else if(P[i - 1] <= ub[i - 1] & all(P[i:(i+d)] >= ub[i])){
            Rule[(i+d):(i+d+k)] <- 1
            test1 <- i + d + k
          }
          
          
        }else if( (i + d)<= N){
          
          if(P[i - 1] >= lb[i - 1] & all(P[i:(i+d)] <= lb[i])){
            Rule[(i+d):N] <- -1
            test1 <- i + d + k
            
          }else if(P[i - 1] <= ub[i - 1] & all(P[i:(i+d)] >= ub[i])){
            Rule[(i+d):N] <- 1
            test1 <- i + d + k
          }
        }
      }
    }
    
    
    if(!k2){ 
      Rule <- zoo::na.locf0(Rule) #fill in the blanks
    }
    
    
    
    
    Rule <- dplyr::lag(Rule, 1) #adjust for advanced knowledge bias
    Rule[is.na(Rule)] <- 0
    
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
                   path = paste0(wdir, exchange, currency,"_", Freq, "_cbrules.csv"))
  print(paste("CB Rules Saved at", Sys.time()))
}else{
  print("ERROR: Data was not found in the working directory")
  print(dir)
  print(paste("calculation unsuccesful for CB Rules at", Sys.time()))
}

