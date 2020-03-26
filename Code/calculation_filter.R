###############################
# calculate all the filter rules
###############################


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
if((filename %in% list.files(wdir))){
  dir <- paste0(wdir, filename)
  
  
  # Import Data ####
  data <- na.locf_bitcoincharts(data.table::fread(dir), F, F, F, F)
  P <- data$Price
  rm(data)
  
  x <- c(0.05,0.1,0.5,1,5,10,20)
  y <- c(0.05,0.1,0.5,1,5,10,20)
  d <- c(0:5)
  dx <- 0:5
  dy <- 0:4
  j <- c(1,2,5,10,20)
  k <- c(0, 5,10,15,20,25)
  
  filter_rules2 <- expand.grid(x = x, d = d, j = j, k = k, KEEP.OUT.ATTRS = F)
  
  
  filter_rules <- expand.grid(x = x, dx = dx, y = y, dy =dy, j = j, KEEP.OUT.ATTRS = F)
  filter_rules <- filter_rules[filter_rules$y < filter_rules$x, ]
  filter_rules <- filter_rules[filter_rules$dy < filter_rules$dx, ]
  # filter_rules <- filter_rules[(filter_rules$dy == 0 & filter_rules$d != 0) | (filter_rules$d == 0 & filter_rules$dy != 0), ]
  
  rownames(filter_rules) <- NULL
  rownames(filter_rules2) <- NULL
  
  calculate_high <- function(P, j){
    max <- zoo::rollapply(P, j, 
                          function(x) max(x, na.rm = T), 
                          fill = NA, align = "right")
    max <- dplyr::lag(max, 1)
    Index <- which(P >= max)
    
    N <- length(P)
    High <- rep(NA,N)
    High[Index] <- P[Index]
    High <- zoo::na.locf0(High)
    High <- dplyr::lag(High, 1)
    
    return(High)
    
  }
  
  calculate_low <- function(P, j){
    min <- zoo::rollapply(P, j, 
                          function(x) min(x, na.rm = T), 
                          fill = NA, align = "right")
    min <- dplyr::lag(min, 1)
    Index <- which(P <= min)
    
    N <- length(P)
    Low <- rep(NA,N)
    Low[Index] <- P[Index]
    Low <- zoo::na.locf0(Low)
    Low <- dplyr::lag(Low, 1)
    
    return(Low)
    
  }
  
  Low <- sapply(j, function(l){calculate_low(P, l)})
  High <- sapply(j, function(l){calculate_high(P, l)})
  
  colnames(Low) <- as.character(j)
  colnames(High) <- as.character(j)
  
  
  
  calculate_filter <- function(Low, High, x, dx, y, dy, P){
    x <- x/100
    y <- y/100
    N <- length(Low)
    
    #Determine where P is below Low and above High
    Index1 <- which(P <= Low | P >= High)
    
    Low1 <- (1 + x) * Low
    High1 <- (1 - x) * High
    
    Low2 <- (1 + y) * Low
    High2 <- (1 - y) * High
    
    NA_high <- which(is.na(High))
    NA_low <- which(is.na(Low))
    
    Index1 <- Index1[!(Index1 %in% c(NA_high,NA_low))]
    
    
    #Predefine Rule for quickness
    Rule <- rep(NA, N)
    
    #A check whether the current trade point is overridden by a previous trade.
    #for example if k = 5 but another trade signal is given after 3 days, this 
    # signal can be ignored.
    test1 <- 0
    trade <- F
    for(i in Index1){#loop over possible trading points
      i_1 <- i + 1
      
      
      if(!trade){ #actual check
        
        if( (i_1 + dx) <= N){
          if(P[i] < Low1[i] & all(P[i_1:(i_1+dx)] >= Low1[i_1])){
            Rule[(i_1+dx)] <- 1
            trade <- T
            position <- 1
            Index2 <- i_1 + dx + 1
            
          }else if(P[i] > High1[i] & all(P[i_1:(i_1+dx)] <= High1[i_1])){
            Rule[(i_1+dx)] <- -1
            trade <- T
            position <- -1
            Index2 <- i_1 + dx + 1
            
          }
        }
        
      }else if( (i_1 + dy) <= N){
        if(position == -1){
          if(P[i] < Low2[i] & all(P[i_1:(i_1+dy)] >= Low2[i_1])){
            Rule[(i_1+dy)] <- 0
            Rule[Index2:(i+dy)] <- position
            trade <- F
            position <- 0
            
          }
        }else if(position == 1){
          if(P[i] > High2[i] & all(P[i_1:(i_1+dy)] <= High2[i_1])){
            Rule[(i_1+dy)] <- 0
            Rule[Index2:(i+dy)] <- position
            trade <- F
            position <- 0
            
          }
        }
      }
      
    }
    
    
    if(trade & (Index2 <= N)){
      Rule[Index2:N] <- position
    }
    
    # cbind(Low, P, High, Rule)[1:100,]
    
    
    Rule <- dplyr::lag(Rule, 1) #adjust for advanced knowledge bias
    Rule[is.na(Rule)] <- 0
    
    return(Rule)
    
  }
  
  
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
    
    NA_high <- which(is.na(High))
    NA_low <- which(is.na(Low))
    
    Index1 <- Index1[!(Index1 %in% c(NA_high,NA_low))]
    
    
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
  
  
  #Calculation first set of filter Rules ####
  Rule_list <- lapply(seq_len(nrow(filter_rules2)), function(l)filter_rules2[l, ])
  
  
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("High", "Low", "calculate_filter2", "P"))
  clusterEvalQ(cl, c(library(zoo), library(dplyr)))
  print(paste("Calculating filter2 Rules at", Sys.time()))
  
  Rules <- parLapply(cl, Rule_list, function(l)calculate_filter2(Low[ ,as.character(l[3])], 
                                                                 High[ ,as.character(l[3])], 
                                                                 as.numeric(l[1]), 
                                                                 as.numeric(l[2]), 
                                                                 as.numeric(l[4]),
                                                                 P))
  
  
  print(paste("filter2 Rules Calculated at", Sys.time()))
  stopCluster(cl)
  
  print(paste("Saving filter2 Rules at", Sys.time()))
  readr::write_csv(as.data.frame(do.call(cbind, Rules)), 
                   path = paste0(wdir, exchange, currency,"_", Freq, "_filter2rules.csv"))
  print(paste("filter2 Rules Saved at", Sys.time()))
  
  
  #Second set of filter Rules ####
  Rule_list <- lapply(seq_len(nrow(filter_rules)), function(l)filter_rules[l, ])
  
  
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("High", "Low", "calculate_filter", "P"))
  clusterEvalQ(cl, c(library(zoo), library(dplyr)))
  print(paste("Calculating filter Rules at", Sys.time()))
  
  Rules <- parLapply(cl, Rule_list, function(l)calculate_filter(Low[ ,as.character(l[5])], 
                                                                High[ ,as.character(l[5])], 
                                                                as.numeric(l[1]), 
                                                                as.integer(l[2]),
                                                                as.numeric(l[3]),
                                                                as.integer(l[4]),
                                                                P))
  
  print(paste("filter Rules Calculated at", Sys.time()))
  stopCluster(cl)
  
  print(paste("Saving filter Rules at", Sys.time()))
  readr::write_csv(as.data.frame(do.call(cbind, Rules)), 
                   path = paste0(wdir, exchange, currency,"_", Freq, "_filterrules.csv"))
  print(paste("filter Rules Saved at", Sys.time()))
  
}else{
  print("ERROR: Data was not found in the working directory")
  print(dir)
  print(paste("calculation unsuccesful for filter Rules at", Sys.time()))
}