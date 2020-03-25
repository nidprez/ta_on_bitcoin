gc()

#libraries ####
library(NiekPhD)
library(parallel)
library(tidyverse)

#General Document options ####
# exchange <- "bitstamp"
# currency <- "usd"
# Freq <- "day"
# q <- 0.2

dir <- paste0("C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Data/", 
              exchange, "/", exchange, currency, "_", Freq, ".csv")
wdir <- paste0("C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Data/", 
               exchange, "/")

dir1 <- paste0(wdir, exchange, currency, "_", Freq, "MA_filter=0_Rules.csv")
dir2 <- paste0(wdir, exchange, currency, "_", Freq, "MA_filter=0.01_Rules.csv")
dir3 <- paste0(wdir, exchange, currency, "_", Freq, "RSI_Rules.csv")


# Import Data ####
data <- NiekPhD::na.locf_bitcoincharts(data.table::fread(dir), Year = T)


Years <- unique(data$Year)
Years <- Years[Years > 2012 & Years < 2019]


start = rep(NA, length(Years))
stop = rep(NA, length(Years))
for(i in seq_along(Years)){
  x <- which(data$Year == Years[i])
  start[i] <- x[1]
  stop[i] <- tail(x, 1)
}

calc_return <- function(rule, benchmark, type){
  ret <- ((rule)- type)*benchmark #type = 0 if you compare against the no trade benchmark and 1 against the only long benchmark
  return(ret)
}

calc_cost <- function(rule){
  #Define empty vector
  cost <- rep(0,length(rule))
  
  # Use the rle function in order to get the changes in value (lengths gives the length of each run and value the value of the run)
  rle_output <- rle(rule)
  # Get the indices where a trade takes place.
  trades <- cumsum(rle_output$lengths) + 1
  trades <- trades[-length(trades)]
  
  if(rle_output$values[1] == 0){
   trades <- trades[-1] #There is no trade on the first day so no transaction cost either
  }

  cost[trades] <- 1
  
  cost
}

for(i in seq_along(Years)){

  #+ Long only strategy ####
  #.Load in data, skip = starti
  Rules1 <- data.table::fread(dir1, skip = (start[i]), nrows = stop[i] - start[i] + 1)
  Rules3 <- data.table::fread(dir3, skip = (start[i]), nrows = stop[i] - start[i] + 1)
  
  Rules <- cbind(Rules1, 
                 #Rules2, 
                 Rules3)
  rm(Rules1,
     #Rules2,
     Rules3)
  
  #Long only strategy
  Rules[is.na(Rules)] <- 0
  Rules[Rules == -1] <- 0
  Rules <- as.matrix(Rules)
  
  #+++ benchmark = long ####
  benchmark <- data$Ret[start[i]:stop[i]]
  benchmark[is.na(benchmark)] <- 0
  
  
  print(paste("split Rules into list at", Sys.time()))
  Rules <- split(Rules, col(Rules))
  
  # ++++ Returns ####
  print(paste("calculate f_hat at", Sys.time()))
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("calc_return", "calc_cost", "benchmark"))
  f_hat <- parLapply(cl, Rules, function(x)calc_return(x, benchmark, 1))
  f_hat2 <- parLapply(cl, Rules, function(x)calc_return(x, benchmark, 0))
  costs <- parLapply(cl, Rules, function(x)calc_cost(x))
  stopCluster(cl)
  
  f_hat <- do.call(cbind, f_hat)
  f_hat2 <- do.call(cbind, f_hat2)
  costs <- do.call(cbind, costs)
  
  
  write.table(f_hat, paste0("Data/f_hat_bh_long_", exchange, currency, "_", Freq, "_" , Years[i], ".csv"), sep = ",", row.names = F, col.names = F)
  write.table(f_hat2, paste0("Data/f_hat_zb_long_", exchange, currency, "_", Freq, "_" , Years[i], ".csv"), sep = ",", row.names = F, col.names = F)
  write.table(f_hat2, paste0("Data/costs_", exchange, currency, "_", Freq, "_" , Years[i], ".csv"), sep = ",", row.names = F, col.names = F)
  
  Rules1 <- data.table::fread(dir1, skip = (start[i]), nrows = stop[i] - start[i] + 1)
  Rules3 <- data.table::fread(dir3, skip = (start[i]), nrows = stop[i] - start[i] + 1)
  
  Rules <- cbind(Rules1, 
                 #Rules2, 
                 Rules3)
  rm(Rules1,
     #Rules2,
     Rules3)
  
  #Long only strategy
  Rules[is.na(Rules)] <- 0
  Rules <- as.matrix(Rules)
  
  
  print(paste("split Rules into list at", Sys.time()))
  Rules <- split(Rules, col(Rules))
  
  # ++++ Returns ####
  print(paste("calculate f_hat at", Sys.time()))
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("calc_return", "calc_cost", "benchmark"))
  f_hat <- parLapply(cl, Rules, function(x)calc_return(x, benchmark, 1))
  f_hat2 <- parLapply(cl, Rules, function(x)calc_return(x, benchmark, 0))
  stopCluster(cl)
  
  f_hat <- do.call(cbind, f_hat)
  f_hat2 <- do.call(cbind, f_hat2)
  
  write.table(f_hat, paste0("Data/f_hat_bh_both_", exchange, currency, "_", Freq, "_" , Years[i], ".csv"), sep = ",", row.names = F, col.names = F)
  write.table(f_hat2, paste0("Data/f_hat_zb_both_", exchange, currency, "_", Freq, "_" , Years[i], ".csv"), sep = ",", row.names = F, col.names = F)
}

# for (k in c("both","long") {
#   for(i in Years){
#     f_hat <- data.table::fread(paste0("Data/f_hat_bh_", k, "_", exchange, currency, "_", Freq, "_" , i, ".csv"))
#     
#     
#   }
# }




