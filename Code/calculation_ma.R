###########################
# Calculate the moving average rules
###########################

gc()

#libraries ####
library(NiekPhD)
library(parallel)
library(tidyverse)

# General Document options ####
# exchange <- "bitstamp"
# currency <- "usd"
# Freq <- "1hour"

wdir <- paste0(getwd(), "/Data/", exchange, "/")
filename <- paste0(exchange, currency, "_", Freq, ".csv")
if((filename %in% list.files(wdir))){ #test if the directory is right
  dir <- paste0(wdir, filename)
  
  
  # Import Data ####
  data <- na.locf_bitcoincharts(data.table::fread(dir), F, F, F, F)
  P <- data$Price
  rm(data)
  
  # specs of the Moving Average Rules
  p <- c(1,2,5,10,15,20,25,50,100,150,200) #lags in the short MA
  q <- c(2,5,10,15,20,25,50,100,150,200,250) #lags in the long MA
  # n <- c(2,5,10,15,20,25,50,100,150)
  x <- c(0,0.0005,0.001,0.005,0.01,0.05) #percentage of the filter
  d <- c(0,2,3,4,5) #number of days that the MA should cross eachother
  k <- c(0, 5,10,25) #duration of holding the position (0 being as long as possible)
  
  MA_lags <- unique(c(p,q)) #get the individual MA lags
  MA_rules <- expand.grid(p = p, q=q, x=x, d=d, k=k, KEEP.OUT.ATTRS = F)
  MA_rules <- MA_rules[!(MA_rules$p >= MA_rules$q),] #delete all instances where the short MA is equal or longer than the long MA
  rownames(MA_rules) <- NULL
  
  #calculate all the possible moving averages
  MAs <- sapply(MA_lags, function(l){zoo::rollapply(P, l, 
                                                    function(x) mean(x, na.rm = T), 
                                                    fill = NA, align = "right")})
  
  colnames(MAs) <- as.character(MA_lags)
  rm(MA_lags)
  
  
  
  calculate_crossingMA <- function(MAS, MAL, x, d, k){
    N <- length(MAS)
    Rule <- integer(N)
    if(!x && !d && !k){
      Rule <- ifelse(MAS >= MAL, 1, -1)
    }else if(!d && !k){
      Rule <- ifelse(MAS >= ((1 + x) * MAL), 1, ifelse(MAS <= ((1 - x) * MAL), -1, NA))
      Rule <- zoo::na.locf0(Rule)
    }else if(!k){
      Rule2 <- ifelse(MAS >= ((1 + x) * MAL), 1, ifelse(MAS <= ((1 - x) * MAL), -1, NA))
      rle_rule <- rle(Rule2)
      index <- cumsum(rle_rule[["lengths"]]) #determine indices where a change takes place
      index2 <- rle_rule[["lengths"]] >= d #runs larger than d periods
      index3 <- index[index2] - rle_rule[["lengths"]][index2] + d # indices where a run has been going for at least d periods
      Rule <- rep(NA, N) #empty vector
      Rule[index3] <- rle_rule[["values"]][index2] #put value of the run on the right indices
      Rule <- zoo::na.locf0(Rule)
    }else if(!d){
      Rule2 <- ifelse(MAS >= ((1 + x) * MAL), 1, ifelse(MAS <= ((1 - x) * MAL), -1, NA))
      Rule <- rep(NA, N) #empty vector
      i <- 1
      while (i <= N) {
        if(is.na(Rule2[i])){
          #skip period if the rule is NA
          i <- i + 1
        }else{
          #if it is not NA hold the position for k periods or until the end of the sample
          if((i + k) <= N){
            Rule[i:(i+k-1)] <- Rule2[i]
          }else{
            Rule[i:N] <- Rule2[i]
          }
          
          i <- i + k
        }
      }
    }else{
      Rule2 <- ifelse(MAS >= ((1 + x) * MAL), 1, ifelse(MAS <= ((1 - x) * MAL), -1, NA))
      Rule <- rep(NA, N) #empty vector
      i <- d
      while (i <= N) {
        if(is.na(Rule2[i])){
          i <- i + 1
        }else{
          #check whether MAS is under or above MAL for the d previous periods 
          # e.g. either they previous periods are all 1 or -1
          test <- abs(max(Rule2[(i - d + 1): i]) - min(Rule2[(i- d + 1): i])) < .Machine$double.eps ^ 0.5
          if(test && !is.na(test)){
            if((i + k) <= N){
              Rule[i:(i+k-1)] <- Rule2[i]
            }else{
              Rule[i:N] <- Rule2[i]
            }
            i <- i + k
          }else{
            i <- i + 1
          }
        }
      }
    }
    
    Rule <- dplyr::lag(Rule, 1)
    Rule[is.na(Rule)] <- 0
    
    return(Rule)
  }
  
  Rule_list <- lapply(seq_len(nrow(MA_rules)), function(l)MA_rules[l, ])
  
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("MAs", "calculate_crossingMA"))
  clusterEvalQ(cl, library(zoo))
  print(paste("Calculating Moving average Rules at", Sys.time()))
  
  Rules <- parLapply(cl, Rule_list, function(l)calculate_crossingMA(MAs[ ,as.character(l[1])], 
                                                                    MAs[ ,as.character(l[2])], 
                                                                    as.numeric(l[3]), 
                                                                    as.numeric(l[4]), 
                                                                    as.numeric(l[5])))
  
  print(paste("Moving average Rules Calculated at", Sys.time()))
  stopCluster(cl)
  
  print(paste("Saving Moving average Rules at", Sys.time()))
  readr::write_csv(as.data.frame(do.call(cbind, Rules)), 
                   path = paste0(wdir, exchange, currency,"_", Freq, "MA_Rules.csv"))
  print(paste("Moving average Rules Saved at", Sys.time()))
  
}else{
  print("ERROR: Data was not found in the working directory")
  print(dir)
  print(paste("calculation unsuccesful for CB Rules at", Sys.time()))
}

