gc()

#libraries ####
library(NiekPhD)
library(parallel)
library(tidyverse)

# General Document options ####
exchange <- "mtgox"
currency <- "usd"
Freq <- "day"
q <- 0.2

dir <- paste0("C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Data/", 
              exchange, "/", exchange, currency, "_", Freq, ".csv")
wdir <- paste0("C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Data/", 
               exchange, "/")

dir1 <- paste0(wdir, exchange, currency, "_", Freq, "MA_filter=0_Rules.csv")
dir2 <- paste0(wdir, exchange, currency, "_", Freq, "MA_filter=0.01_Rules.csv")
dir3 <- paste0(wdir, exchange, currency, "_", Freq, "RSI_Rules.csv")


# Import Data ####
data <- data.table::fread(dir)
data$V1 <- data$V1 + 300
data$V1 <- anytime::utctime(data$V1)
data <- bitcoincharts_tidy(data, Freq)
data$Price <- zoo::na.locf(data$Price)
# data$High <- zoo::na.locf(data$High)
# data$Low <- zoo::na.locf(data$Low)
data$VolumeCurr[is.na(data$VolumeCurr)] <- 0 
data <- data %>% 
  mutate(Year = lubridate::year(data$Time), Ret = mydiff(log(Price))) #add factor containing the year

Years <- unique(data$Year)
Years <- Years[-1]
for (i in seq_along(Years)) {
  if(Years[i] == 2019){
    Years <- Years[-i]
  }
}
start = rep(NA, length(Years))
stop = rep(NA, length(Years))
for(i in seq_along(Years)){
  x <- which(data$Year == Years[i])
  start[i] <- x[1]
  stop[i] <- tail(x, 1)
}


# Hansen Spa ####
# Prepare empty data frames
result  <- data.frame(Year = Years, Type = "Long", Benchmark = "Buy and hold", cost = 0, 
                      T_value = NA, p_l = NA, p_c = NA, p_u = NA, best = NA)

result2  <- data.frame(Year = Years, Type = "Long", Benchmark = "Buy and hold", cost = 0.0025, 
                       T_value = NA, p_l = NA, p_c = NA, p_u = NA, best = NA)

result3  <- data.frame(Year = Years, Type = "Long", Benchmark = "Not in market", cost = 0, 
                      T_value = NA, p_l = NA, p_c = NA, p_u = NA, best = NA)

result4  <- data.frame(Year = Years, Type = "Long", Benchmark = "Not in market", cost = 0.0025, 
                       T_value = NA, p_l = NA, p_c = NA, p_u = NA, best = NA)

result5  <- data.frame(Year = Years, Type = "Both", Benchmark = "Buy and hold", cost = 0, 
                      T_value = NA, p_l = NA, p_c = NA, p_u = NA, best = NA)

result6  <- data.frame(Year = Years, Type = "Both", Benchmark = "Buy and hold", cost = 0.0025, 
                       T_value = NA, p_l = NA, p_c = NA, p_u = NA, best = NA)

result7  <- data.frame(Year = Years, Type = "Both", Benchmark = "Not in market", cost = 0, 
                       T_value = NA, p_l = NA, p_c = NA, p_u = NA, best = NA)

result8  <- data.frame(Year = Years, Type = "Both", Benchmark = "Not in market", cost = 0.0025, 
                       T_value = NA, p_l = NA, p_c = NA, p_u = NA, best = NA)

returns <- list()
returns2 <- list()
returns3 <- list()
returns4 <- list()
returns5 <- list()
returns6 <- list()
returns7 <- list()
returns8 <- list()

# Function to Prepare the returns
calc_return <- function(rule, benchmark, costs, type){
  ret <- (dplyr::lag(rule)- type)*benchmark #type = 0 if you compare against the no trade benchmark and 1 against the only long benchmark
  ret[1] <- 0
  trades <- rle(rule)$values
  if(trades[1] != 0){
    num_trades = length(trades)
  }else{
    num_trades = length(trades) - 1
  }
  N <- length(ret)
  
  #subtract the average cost from the logreturns
  ret <- ret + ((num_trades/N)*log(1 - costs))
  
  ret
}

# Long only strategy ####
for(i in seq_along(Years)){
  #.Load in data, skip = starti - 1 because we don't want a foresight bias
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
  
  #benchmark = long
  benchmark <- data$Ret[start[i]:stop[i]]
  benchmark[is.na(benchmark)] <- 0
  
  Rules <- split(Rules, col(Rules))
  
  # ++ Returns ####
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("calc_return", "benchmark"))
  f_hat <- parLapply(cl, Rules, function(x)calc_return(x, benchmark, 0, 1))
  f_hat <- do.call(cbind, f_hat)
  stopCluster(cl)
  hansen_spa1(f_hat, N = 500, q = q)
  
  result[i, -1:-4] <- hansen_spa1(f_hat, N = 500, q = q)
  returns[[i]] <- rbind(summarize_returns(data[start[i]:stop[i],], Name = "best rule", 
                                          rule = as.integer(Rules[[result$best[i]]])), 
                        summarize_returns(data[start[i]:stop[i],], Name = "Benchmark", 
                                          rule = rep(1,length(benchmark))))
  
  print(result)
  rm(f_hat)
  
  # ++ Returns 2 ####
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("calc_return", "benchmark"))
  f_hat <- parLapply(cl, Rules, function(x)calc_return(x, benchmark, 0.0025, 1))
  f_hat <- do.call(cbind, f_hat)
  stopCluster(cl)
  
  result2[i, -1:-4] <- hansen_spa1(f_hat, N = 500, q = q)
  returns2[[i]] <- rbind(summarize_returns(data[start[i]:stop[i],], Name = "best rule", TC = 0.0025,
                                          rule = as.integer(Rules[[result$best[i]]])), 
                        summarize_returns(data[start[i]:stop[i],], Name = "Benchmark", 
                                          rule = rep(1,length(benchmark))))
  print(result2)
  rm(f_hat)
  
  # ++ Returns 3 ####
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("calc_return", "benchmark"))
  f_hat <- parLapply(cl, Rules, function(x)calc_return(x, benchmark, 0, 0))
  f_hat <- do.call(cbind, f_hat)
  stopCluster(cl)
  
  result3[i, -1:-4] <- hansen_spa1(f_hat, N = 500, q = q)
  returns3[[i]] <- rbind(summarize_returns(data[start[i]:stop[i],], Name = "best rule", TC = 0.0025,
                                           rule = as.integer(Rules[[result$best[i]]])), 
                         summarize_returns(data[start[i]:stop[i],], Name = "Benchmark", 
                                           rule = rep(1,length(benchmark))))
  print(result3)
  rm(f_hat)
  
  # ++ Returns 4 ####
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("calc_return", "benchmark"))
  f_hat <- parLapply(cl, Rules, function(x)calc_return(x, benchmark, 0.0025, 0))
  f_hat <- do.call(cbind, f_hat)
  stopCluster(cl)
  
  result4[i, -1:-4] <- hansen_spa1(f_hat, N = 500, q = q)
  returns4[[i]] <- rbind(summarize_returns(data[start[i]:stop[i],], Name = "best rule", TC = 0.0025,
                                           rule = as.integer(Rules[[result$best[i]]])), 
                         summarize_returns(data[start[i]:stop[i],], Name = "Benchmark", 
                                           rule = rep(1,length(benchmark))))
  print(result4)
  rm(f_hat)
  
}

gc()

# Long and Short strategy ####
for(i in seq_along(Years)){
  #.Load in data, skip = starti - 1 because we don't want a foresight bias
  Rules1 <- data.table::fread(dir1, skip = (start[i] - 1), nrows = stop[i] - start[i] + 1)
  #Rules2 <- data.table::fread(dir2, skip = start[i], nrows = stop[i] - start[i] + 1)
  Rules3 <- data.table::fread(dir3, skip = (start[i] - 1), nrows = stop[i] - start[i] + 1)
  
  Rules <- cbind(Rules1, 
                 #Rules2, 
                 Rules3)
  rm(Rules1,
     #Rules2,
     Rules3)
  Rules[is.na(Rules)] <- 0
  Rules <- as.matrix(Rules)
  
  
  #benchmark = long
  benchmark <- data$Ret[start[i]:stop[i]]
  benchmark[is.na(benchmark)] <- 0
  
  Rules <- split(Rules, col(Rules))
  
  # ++ Returns 5 ####
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("calc_return", "benchmark"))
  f_hat <- parLapply(cl, Rules, function(x)calc_return(x, benchmark, 0, 1))
  f_hat <- do.call(cbind, f_hat)
  stopCluster(cl)
  
  
  result5[i, -1:-4] <- hansen_spa1(f_hat, N = 500, q = q)
  returns5[[i]] <- rbind(summarize_returns(data[start[i]:stop[i],], Name = "best rule", 
                                          rule = as.integer(Rules[[result$best[i]]])), 
                        summarize_returns(data[start[i]:stop[i],], Name = "Benchmark", 
                                          rule = rep(1,length(benchmark))))
  
  print(result5)
  rm(f_hat)
  
  # ++ Returns 6 ####
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("calc_return", "benchmark"))
  f_hat <- parLapply(cl, Rules, function(x)calc_return(x, benchmark, 0.0025, 1))
  f_hat <- do.call(cbind, f_hat)
  stopCluster(cl)
  
  result6[i, -1:-4] <- hansen_spa1(f_hat, N = 500, q = q)
  returns6[[i]] <- rbind(summarize_returns(data[start[i]:stop[i],], Name = "best rule", TC = 0.0025,
                                           rule = as.integer(Rules[[result$best[i]]])), 
                         summarize_returns(data[start[i]:stop[i],], Name = "Benchmark", 
                                           rule = rep(1,length(benchmark))))
  print(result6)
  rm(f_hat)
  
  # ++ Returns 7 ####
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("calc_return", "benchmark"))
  f_hat <- parLapply(cl, Rules, function(x)calc_return(x, benchmark, 0, 0))
  f_hat <- do.call(cbind, f_hat)
  stopCluster(cl)
  
  result7[i, -1:-4] <- hansen_spa1(f_hat, N = 500, q = q)
  returns7[[i]] <- rbind(summarize_returns(data[start[i]:stop[i],], Name = "best rule", TC = 0.0025,
                                           rule = as.integer(Rules[[result$best[i]]])), 
                         summarize_returns(data[start[i]:stop[i],], Name = "Benchmark", 
                                           rule = rep(1,length(benchmark))))
  print(result7)
  rm(f_hat)
  
  # ++ Returns 8 ####
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("calc_return", "benchmark"))
  f_hat <- parLapply(cl, Rules, function(x)calc_return(x, benchmark, 0.0025, 0))
  f_hat <- do.call(cbind, f_hat)
  stopCluster(cl)
  
  result8[i, -1:-4] <- hansen_spa1(f_hat, N = 500, q = q)
  returns8[[i]] <- rbind(summarize_returns(data[start[i]:stop[i],], Name = "best rule", TC = 0.0025,
                                           rule = as.integer(Rules[[result$best[i]]])), 
                         summarize_returns(data[start[i]:stop[i],], Name = "Benchmark", 
                                           rule = rep(1,length(benchmark))))
  print(result8)
  rm(f_hat)
  
}

# Save Results ####

write_csv(rbind(result, result2, result3, result4, result5, result6, result7, result8), 
          paste0("hansen_q=", q,"_", exchange, currency, "_", Freq, ".csv"))

write_csv(rbind(do.call(rbind, returns), do.call(rbind, returns2), do.call(rbind, returns3), 
                do.call(rbind, returns4), do.call(rbind, returns5), do.call(rbind, returns6), 
                do.call(rbind, returns7), do.call(rbind, returns8)), 
          paste0("hansen_q=", q,"_", exchange, currency, "_", Freq, "_returns.csv"))
