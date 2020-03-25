gc()

#libraries ####
library(NiekPhD)
library(parallel)
library(tidyverse)

# General Document options ####
exchange <- "bitstamp"
currency <- "usd"
Freq <- "1hour"
q <- 0.2

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
Years <- Years[Years >= 2013 & Years <= 2018]


start = rep(NA, length(Years))
stop = rep(NA, length(Years))
for(i in seq_along(Years)){
  x <- which(data$Year == Years[i])
  start[i] <- x[1]
  stop[i] <- tail(x, 1)
}


# Hansen Spa ####
# Prepare empty data frames
result  <- data.frame(Year = rep(Years, each = 2), Type = "Long", Benchmark = "Buy and hold", 
                      Transactions = NA, Return = NA, cost = NA, T_value = NA, 
                      p_l = NA, p_c = NA, p_u = NA, best = NA)

result2  <- data.frame(Year = rep(Years, each = 2), Type = "Long", Benchmark = "Not in market", 
                      Transactions = NA, Return = NA, cost = NA, T_value = NA, 
                      p_l = NA, p_c = NA, p_u = NA, best = NA)

result3  <- data.frame(Year = rep(Years, each = 2), Type = "Both", Benchmark = "Buy and hold", 
                      Transactions = NA, Return = NA, cost = NA, T_value = NA, 
                      p_l = NA, p_c = NA, p_u = NA, best = NA)

result4  <- data.frame(Year = rep(Years, each = 2), Type = "Both", Benchmark = "Not in market", 
                       Transactions = NA, Return = NA, cost = NA, T_value = NA, 
                       p_l = NA, p_c = NA, p_u = NA, best = NA)





calc_return <- function(rule, benchmark, type){
  ret <- ((rule)- type)*benchmark #type = 0 if you compare against the no trade benchmark and 1 against the only long benchmark
  return(ret)
}

calc_cost <- function(rule, costs, transactions = F){
  
  trades <- rle(rule)$values
  
  if(trades[1] != 0){ #if the first position = 0, that means there is no transaction and no trading cost
    num_trades = length(trades)
  }else{
    num_trades = length(trades) - 1
  }
  if(transactions){
    return(num_trades)
  }
  
  N <- length(rule)
  
  #subtract the average cost from the logreturns
  cost_hat <- ((num_trades/N)*log(1 - costs))
  return(cost_hat)
}


calc_return <- function(rule, benchmark, type, TC){
  ret <- ((rule)- type)*benchmark
  
  if(TC > 0){
    #Define empty vector
    cost <- rep(0,length(rule))
    
    # Use the rle function in order to get the changes in value (lengths gives the length of each run and value the value of the run)
    rle_output <- rle(rule)
    # Get the indices where a trade takes place.
    trades <- cumsum(rle_output$lengths) + 1
    trades <- trades[-length(trades)]
    
    if(rle_output$values[1] != 0){
      trades <- c(1, trades) #There is no trade on the first day so no transaction cost either
    }
    
    cost[trades] <- log(1-TC)
    
    
    ret <- ret + cost
  }
return(ret)
}

# Results loop####
for(i in seq_along(Years)){
  k <- (i*2) - 1
  
  #+ Long only strategy ####
  #.Load in data, skip = starti
  Rules1 <- data.table::fread(dir1, skip = (start[i]), nrows = stop[i] - start[i] + 1)
  #Rules2 <- data.table::fread(dir2, skip = start[i], nrows = stop[i] - start[i] + 1)
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
  
  print(paste("calculate mean_cost at", Sys.time()))
  cost_hat <- apply(Rules, 2, calc_cost, costs = 0.0025)
  names(cost_hat) <- NULL
  print(paste("Mean_cost calculated at", Sys.time()))
  
  
  print(paste("split Rules into list at", Sys.time()))
  Rules <- split(Rules, col(Rules))
  
  # ++++ Returns ####
  print(paste("calculate f_hat at", Sys.time()))
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("calc_return", "calc_cost", "benchmark"))
  f_hat <- parLapply(cl, Rules, function(x)calc_return(x, benchmark, 1, 0.001))
  
  # cost_hat <- parLapply(cl, Rules, function(x)calc_cost(x, 0.0025))
  # print(paste("Mean_cost calculated at", Sys.time()))
  # 
  f_hat <- do.call(cbind, f_hat)
  # cost_hat <- do.call(c, cost_hat)
  # names(cost_hat) <- NULL
  stopCluster(cl)
  print(paste("f_hat calculated at", Sys.time()))
  
  
  result[k:(k+1), -1:-5] <- hansen_spa1(f_hat, N = 1000, q = 0.2)
  result$Transactions[k] <- calc_cost(Rules[[result$best[k]]], 0, T)
  result$Transactions[k + 1] <- calc_cost(Rules[[result$best[k + 1]]], 0, T)
  result$Return[k] <- sum(Rules[[result$best[k]]]*benchmark)
  result$Return[k + 1] <- sum(Rules[[result$best[k + 1]]]*benchmark) + 
    length(benchmark) * cost_hat[result$best[k + 1]]

  
  print(result)
  rm(f_hat)
  
  # +++ benchmark is not in market ####
  print(paste("calculate f_hat at", Sys.time()))
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("calc_return", "calc_cost", "benchmark"))
  f_hat <- parLapply(cl, Rules, function(x)calc_return(x, benchmark, 0))
  
  # cost_hat <- parLapply(cl, Rules, function(x)calc_cost(x, 0.0025))
  # print(paste("Mean_cost calculated at", Sys.time()))
  # 
  f_hat <- do.call(cbind, f_hat)
  # cost_hat <- do.call(c, cost_hat)
  # names(cost_hat) <- NULL
  stopCluster(cl)
  print(paste("f_hat at calculated", Sys.time()))
  
  
  result2[k:(k+1), -1:-5] <- hansen_spa1(f_hat, N = 1000, q = q, cost_hat)
  result2$Transactions[k] <- calc_cost(Rules[[result2$best[k]]], 0, T)
  result2$Transactions[k + 1] <- calc_cost(Rules[[result2$best[k + 1]]], 0, T)
  result2$Return[k] <- sum(Rules[[result2$best[k]]]*benchmark)
  result2$Return[k + 1] <- sum(Rules[[result2$best[k + 1]]]*benchmark) + 
    length(benchmark) * cost_hat[result2$best[k + 1]]
  
  print(result2)
  rm(f_hat)
  
  #+ short and long ####
  #.Load in data, skip = starti
  Rules1 <- data.table::fread(dir1, skip = (start[i]), nrows = stop[i] - start[i] + 1)
  #Rules2 <- data.table::fread(dir2, skip = start[i], nrows = stop[i] - start[i] + 1)
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
  
  #+++ benchmark = long ####
  benchmark <- data$Ret[start[i]:stop[i]]
  benchmark[is.na(benchmark)] <- 0
  
  print(paste("calculate mean_cost at", Sys.time()))
  cost_hat <- apply(Rules, 2, calc_cost, costs = 0.0025)
  names(cost_hat) <- NULL
  print(paste("Mean_cost calculated at", Sys.time()))
  
  
  print(paste("split Rules into list at", Sys.time()))
  Rules <- split(Rules, col(Rules))
  
  # ++++ Returns ####
  print(paste("calculate f_hat at", Sys.time()))
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("calc_return", "calc_cost", "benchmark"))
  f_hat <- parLapply(cl, Rules, function(x)calc_return(x, benchmark, 1))
  
  # cost_hat <- parLapply(cl, Rules, function(x)calc_cost(x, 0.0025))
  # print(paste("Mean_cost calculated at", Sys.time()))
  # 
  f_hat <- do.call(cbind, f_hat)
  # cost_hat <- do.call(c, cost_hat)
  # names(cost_hat) <- NULL
  stopCluster(cl)
  print(paste("f_hat at calculated", Sys.time()))
  
  
  result3[k:(k+1), -1:-5] <- hansen_spa1(f_hat, N = 1000, q = q, cost_hat)
  result3$Transactions[k] <- calc_cost(Rules[[result3$best[k]]], 0, T)
  result3$Transactions[k + 1] <- calc_cost(Rules[[result3$best[k + 1]]], 0, T)
  result3$Return[k] <- sum(Rules[[result3$best[k]]]*benchmark)
  result3$Return[k + 1] <- sum(Rules[[result3$best[k + 1]]]*benchmark) + 
    length(benchmark) * cost_hat[result3$best[k + 1]]
  
  
  print(result3)
  rm(f_hat)
  
  # +++ benchmark is not in market ####
  print(paste("calculate f_hat at", Sys.time()))
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("calc_return", "calc_cost", "benchmark"))
  f_hat <- parLapply(cl, Rules, function(x)calc_return(x, benchmark, 0))
  
  # cost_hat <- parLapply(cl, Rules, function(x)calc_cost(x, 0.0025))
  # print(paste("Mean_cost calculated at", Sys.time()))
  # 
  f_hat <- do.call(cbind, f_hat)
  # cost_hat <- do.call(c, cost_hat)
  # names(cost_hat) <- NULL
  stopCluster(cl)
  print(paste("f_hat at calculated", Sys.time()))
  
  
  result4[k:(k+1), -1:-5] <- hansen_spa1(f_hat, N = 1000, q = q, cost_hat)
  result4$Transactions[k] <- calc_cost(Rules[[result4$best[k]]], 0, T)
  result4$Transactions[k + 1] <- calc_cost(Rules[[result4$best[k + 1]]], 0, T)
  result4$Return[k] <- sum(Rules[[result4$best[k]]]*benchmark)
  result4$Return[k + 1] <- sum(Rules[[result4$best[k + 1]]]*benchmark) + 
    length(benchmark) * cost_hat[result4$best[k + 1]]
  
  print(result4)
  rm(f_hat)
}

# Save Results ####

write_csv(rbind(result, result2, result3, result4), 
          paste0("hansen_q=", q,"_", exchange, currency, "_", Freq, ".csv"))