#####################################
# Calculate support and resistance rules
#####################################

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
  data <- na.locf_bitcoincharts(data.table::fread(dir), F, F, T, F)
  P <- data$Price
  rm(data)
  
  x <- c(0.05,0.1,0.5,1,2.5,5,10)
  d <- c(0:5)
  j <- c(2,5,10,15,20,25,50,100,250)
  k <- c(0,1,5,10,25)
  
  SR_rules <- expand.grid(x = x, d = d, j = j, k = k, KEEP.OUT.ATTRS = F)
  
  rownames(SR_rules) <- NULL
  
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
  
  
  calculate_SR <- function(Low, High, x, d, k, P){
    x <- x/100
    N <- length(Low)
    Low <- (1 - x) * Low
    High <- (1 + x) * High
    
    #decrease k with 1 if larger than 0 in order to have the right holding period.
    #i:i + k is actually 1 period longer than k (for example: k = 1, i = 5 => 5:6 
    # is a holding period of 2 instead of 1)
    k2 <- k
    k <- ifelse(k > 0, k - 1, k)
    
    #Determine where P is below Low and above High
    Index1 <- which(P <= Low | P >= High)
    
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
      
      if(i + d > test1){ #actual check
        if( (i + d + k) <= N){
          
          if(P[i - 1] >= Low[i - 1] & all(P[i:(i+d)] <= Low[i])){
            Rule[(i+d):(i+d+k)] <- -1
            test1 <- i + d + k
            
          }else if(P[i - 1] <= High[i - 1] & all(P[i:(i+d)] >= High[i])){
            Rule[(i+d):(i+d+k)] <- 1
            test1 <- i + d + k
          }
          
          
        }else if( (i + d)<= N){
          
          if(P[i - 1] >= Low[i - 1] & all(P[i:(i+d)] <= Low[i])){
            Rule[(i+d):N] <- -1
            test1 <- i + d + k
            
          }else if(P[i - 1] <= High[i - 1] & all(P[i:(i+d)] >= High[i])){
            Rule[(i+d):N] <- 1
            test1 <- i + d + k
          }
        }
      }
    }
    
    
    if(!k2){ 
      Rule <- zoo::na.locf0(Rule) #fill in the blanks
    }
    
    # cbind(Low, P, High, Rule)[1:100,]
    
    
    Rule <- dplyr::lag(Rule, 1) #adjust for advanced knowledge bias
    Rule[is.na(Rule)] <- 0
    
    return(Rule)
    
  }
  
  
  
  Rule_list <- lapply(seq_len(nrow(SR_rules)), function(l)SR_rules[l, ])
  
  
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("High", "Low", "calculate_SR", "P"))
  clusterEvalQ(cl, c(library(zoo), library(dplyr)))
  print(paste("Calculating SR Rules at", Sys.time()))
  
  Rules <- parLapply(cl, Rule_list, function(l)calculate_SR(Low[ ,as.character(l[3])], 
                                                            High[ ,as.character(l[3])], 
                                                            as.numeric(l[1]), 
                                                            as.numeric(l[2]), 
                                                            as.numeric(l[4]),
                                                            P))
  
  print(paste("SR Rules Calculated at", Sys.time()))
  stopCluster(cl)
  
  print(paste("Saving SR Rules at", Sys.time()))
  readr::write_csv(as.data.frame(do.call(cbind, Rules)), 
                   path = paste0(wdir, exchange, currency,"_", Freq, "_supresrules.csv"))
  print(paste("SR Rules Saved at", Sys.time()))
  
}else{
  print("ERROR: Data was not found in the working directory")
  print(dir)
  print(paste("calculation unsuccesful for S&R Rules at", Sys.time()))
}