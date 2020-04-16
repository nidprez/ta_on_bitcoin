##########################################
# Calculate triple moving average rules
##########################################


gc()

#libraries ####
library(NiekPhD)
library(parallel)
library(tidyverse)

# General Document options ####
# exchange <- "bitstamp"
# currency <- "usd"
# Freq <- "1hour"

wdir <- paste0(getwd(), "/Data/", exchange, "/", Freq, "/")
filename <- paste0(exchange, currency, "_", Freq, ".csv")
if((filename %in% list.files(wdir))){ #test if the directory is right
  dir <- paste0(wdir, filename)
  
  
  
  # Import Data ####
  data <- na.locf_bitcoincharts(data.table::fread(dir), Year = T)
  
  # specs of the Moving Average Rules
  p <- c(2,5,10,15,20,25,50,100,150,200) #lags in the short MA
  q <- c(2,5,10,15,20,25,50,100,150,200,250) #lags in the long MA
  n <- c(1,2,5,10,15,20,25,50,100,150)
  x <- c(0,0.0005,0.001,0.005,0.01,0.05) #percentage of the filter
  d <- c(0,2,3,4,5) #number of days that the MA should cross eachother
  
  MA_lags <- unique(c(n,p,q)) #get the individual MA lags
  MA_rules <- expand.grid(n=n, p = p, q=q, x=x, d=d, KEEP.OUT.ATTRS = F)
  MA_rules <- MA_rules[!(MA_rules$p >= MA_rules$q),] #delete all instances where the short MA is equal or longer than the long MA
  MA_rules <- MA_rules[!(MA_rules$n >= MA_rules$p),] #delete all instances where the short MA is equal or longer than the long MA
  MA_rules <- MA_rules[!(MA_rules$n == 1),] #delete all instances where the short MA is equal or longer than the long MA
  rownames(MA_rules) <- NULL
  
  #calculate all the possible moving averages
  MAs <- sapply(MA_lags, function(l){zoo::rollapply(data$Price, l, 
                                                    function(x) mean(x, na.rm = T), 
                                                    fill = NA, align = "right")})
  
  colnames(MAs) <- as.character(MA_lags)
  rm(MA_lags)
  
  
  
  calculate_tripleMA <- function(MA1, MA2, MA3, x, d, P){
    N <- length(MA1)
    if(!x && !d){
      Rule <- numeric(N)
      Rule <- ifelse((P >= MA1 & P >= MA2) | (P >= MA1 & P >= MA3) | 
                       (P >= MA2 & P >= MA3), 0.33, 
                     ifelse((P < MA1 & P < MA2) | (P < MA1 & P < MA3) | 
                              (P < MA2 & P < MA3), -0.33, NA))
      Rule <- ifelse( (P >= MA1 & P >= MA2 & P >= MA3), 1, 
                      ifelse((P < MA1 & P < MA2 & P < MA3), -1, Rule))
    }else if(!d){
      Rule <- numeric(N)
      Rule <- ifelse((P >= ((1 + x) * MA1) & P >= ((1 + x) * MA2)) | 
                       (P >= ((1 + x) * MA1) & P >= ((1 + x) * MA3)) | 
                       (P >= ((1 + x) * MA2) & P >= ((1 + x) * MA3)), 0.33, 
                     ifelse((P < ((1 - x) * MA1) & P < ((1 - x) * MA2)) | 
                              (P < ((1 - x) * MA1) & P < ((1 - x) * MA3)) | 
                              (P < ((1 - x) * MA2) & P < ((1 - x) * MA3)), -0.33, NA))
      
      Rule <- ifelse((P >= ((1 + x) * MA1) & P >= ((1 + x) * MA2) & P >= ((1 + x) * MA3)), 1, 
                     ifelse((P < ((1 - x) * MA1) & P < ((1 - x) * MA2) & P < ((1 - x) * MA3)), -1, Rule))
      
    }else{
      Rule2 <- ifelse((P >= ((1 + x) * MA1) & P >= ((1 + x) * MA2)) | 
                        (P >= ((1 + x) * MA1) & P >= ((1 + x) * MA3)) | 
                        (P >= ((1 + x) * MA2) & P >= ((1 + x) * MA3)), 0.33, 
                      ifelse((P < ((1 - x) * MA1) & P < ((1 - x) * MA2)) | 
                               (P < ((1 - x) * MA1) & P < ((1 - x) * MA3)) | 
                               (P < ((1 - x) * MA2) & P < ((1 - x) * MA3)), -0.33, NA))
      Rule2 <- ifelse((P >= ((1 + x) * MA1) & P >= ((1 + x) * MA2) & P >= ((1 + x) * MA3)), 1, 
                      ifelse((P < ((1 - x) * MA1) & P < ((1 - x) * MA2) & P < ((1 - x) * MA3)), -1, Rule2))
      rle_rule <- rle(Rule2)
      index <- cumsum(rle_rule[["lengths"]]) #determine indices where a change takes place
      index2 <- rle_rule[["lengths"]] >= d #runs larger than d periods
      index3 <- index[index2] - rle_rule[["lengths"]][index2] + d # indices where a run has been going for at least d periods
      Rule <- rep(NA, N) #empty vector
      Rule[index3] <- rle_rule[["values"]][index2] #put value of the run on the right indices
    }
    Rule <- zoo::na.locf0(Rule)
    Rule <- dplyr::lag(Rule, 1)
    Rule[is.na(Rule)] <- 0
    
    return(Rule)
  }
  
  Rule_list <- lapply(seq_len(nrow(MA_rules)), function(l)MA_rules[l, ])
  
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("MAs", "calculate_tripleMA"))
  clusterEvalQ(cl, library(zoo))
  print(paste("Calculating Moving average Rules at", Sys.time()))
  
  Rules <- parLapply(cl, Rule_list, function(l)calculate_tripleMA(MAs[ ,as.character(l[1])], 
                                                                  MAs[ ,as.character(l[2])], 
                                                                  MAs[ ,as.character(l[3])], 
                                                                  as.numeric(l[4]), 
                                                                  as.numeric(l[5]),
                                                                  MAs[,1]))
  
  print(paste("Moving average Rules Calculated at", Sys.time()))
  stopCluster(cl)
  
  print(paste("Saving Moving average Rules at", Sys.time()))
  readr::write_csv(as.data.frame(do.call(cbind, Rules)), 
                   path = paste0(wdir, exchange, currency,"_", Freq, "_triple_marules.csv"))
  print(paste("Moving average Rules Saved at", Sys.time()))
  
}else{
  print("ERROR: Data was not found in the working directory")
  print(dir)
  print(paste("calculation unsuccesful for CB Rules at", Sys.time()))
}