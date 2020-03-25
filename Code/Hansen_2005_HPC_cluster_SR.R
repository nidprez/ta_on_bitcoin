#!/usr/bin/env Rscript

###############################################
#HANSENS METHODOLOGY WITH THE SORTINO RATIO
###############################################

#libraries ####
library(parallel)
library(tidyverse)
library(PerformanceAnalytics)

# functions ####
mydiff <- function(x, lag = 1, differences = 1){
  a = diff(x, lag, differences)
  return(c(rep(NA, lag), a))
}

na.locf_bitcoincharts <- function(data, Anytime = TRUE, HighLow = TRUE, LogRet = TRUE, Year = FALSE){
  
  data$Price <- zoo::na.locf(data$Price)
  data$VolumeCurr[is.na(data$VolumeCurr)] <- 0
  
  if(Anytime){
    data$Time <- anytime::anytime(data$Time)
  }
  
  if(HighLow){
    data$High <- zoo::na.locf(data$High)
    data$Low <- zoo::na.locf(data$Low)
  }
  
  if(LogRet){
    data <- data %>%
      mutate(Ret = mydiff(log(Price))) #add factor containing the year
  }
  
  if(Year){
    data <- data %>%
      mutate(Year = lubridate::year(data$Time)) #add factor containing the year
  }
  
  
  return(data)
}


#General Document options ####
# exchange <- Sys.getenv("exchange")
# currency <- Sys.getenv("currency")
# Freq <- Sys.getenv("Freq")
# q <- as.numeric(Sys.getenv("q"))
# year <- as.numeric(Sys.getenv("year"))
# long <- as.logical(Sys.getenv("long"))
# TC <- as.numeric(Sys.getenv("cost"))
# hodl <- as.logical(Sys.getenv("hodl"))
# N <- as.numeric(Sys.getenv("N"))
# 
# no_cores <- as.numeric(Sys.getenv("ncpus"))

# no_cores=7
# exchange="bitstamp"
# currency="usd"
# Freq="day"
# q=0.2
# year=2013
# long=T
# TC=0.0025
# hodl=T
# N=1000


print(parallel::detectCores())

wdir <- paste0(getwd(),"/Data/", exchange)
dir <- paste0(wdir, "/", exchange, currency, "_", Freq, ".csv")

dir1 <- paste0(wdir, "/", exchange, currency, "_", Freq, "MA_Rules.csv")
dir2 <- paste0(wdir, "/", exchange, currency, "_", Freq, "triple_MA_Rules.csv")
dir3 <- paste0(wdir, "/", exchange, currency, "_", Freq, "RSI_Rules.csv")
dir4 <- paste0(wdir, "/", exchange, currency, "_", Freq, "SR_Rules.csv")
dir5 <- paste0(wdir, "/", exchange, currency, "_", Freq, "filter_Rules.csv")
dir6 <- paste0(wdir, "/", exchange, currency, "_", Freq, "filter2_Rules.csv")
dir7 <- paste0(wdir, "/", exchange, currency, "_", Freq, "CB_Rules.csv")

# Import Data ####
data <- na.locf_bitcoincharts(data.table::fread(dir), Year = T)



x <- which(data$Year == year)
start <- x[1]
stop <- tail(x, 1)
data <- data[start:stop, ]

rm(x)


calc_return <- function(rule, benchmark, type, TC){
  ret <- rule*benchmark
  
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
  
  SR_benchmark <- benchmark/DownsideDeviation(benchmark, method = "full", na.rm = T)
  sd_ret <- DownsideDeviation(ret, method = "full", na.rm = T)
  SR_ret <- ifelse(sd_ret == 0, 0, ret/sd_ret)
  SR <- SR_ret - SR_benchmark
  
  return(SR)
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




# Prepare Rules strategy ####
#.Load in data, skip = starti
Rules <- cbind(data.table::fread(dir1, skip = start, nrows = stop - start + 1), 
               data.table::fread(dir2, skip = start, nrows = stop - start + 1),
               data.table::fread(dir3, skip = start, nrows = stop - start + 1),
               data.table::fread(dir4, skip = start, nrows = stop - start + 1),
               data.table::fread(dir5, skip = start, nrows = stop - start + 1),
               data.table::fread(dir6, skip = start, nrows = stop - start + 1),
               data.table::fread(dir7, skip = start, nrows = stop - start + 1))
Rules[is.na(Rules)] <- 0


#Long only strategy
if(long){
  Rules[Rules < 0] <- 0
}

Rules <- as.matrix(Rules)

print(paste("Rules is", format(object.size(Rules), "Gb")))
n = nrow(Rules)
s = ncol(Rules)

#+++ benchmark ####
benchmark <- data$Ret
benchmark[is.na(benchmark)] <- 0



print(paste("split Rules into list at", Sys.time()))
Rules <- split(Rules, col(Rules))

# ++++ Returns ####
print(paste("calculate f_hat at", Sys.time()))
cl <- parallel::makeCluster(no_cores)
print(paste("made cluster at", Sys.time()))
parallel::clusterExport(cl, list("calc_return", "benchmark", "hodl", "TC"))
parallel::clusterEvalQ(cl, library("PerformanceAnalytics"))
print(paste("export cluster at", Sys.time()))
f_hat <- parallel::parLapply(cl, Rules, function(x)calc_return(x, benchmark, hodl, TC))
print(paste("parlapplied at", Sys.time()))
stopCluster(cl)
# rm(Rules)


print(paste("f_hat calculated at", Sys.time()))
print(paste("f_hat is", format(object.size(f_hat), "Gb")))

#calculate hansen SPA ####
print(paste("start calculating hansen SPA at", Sys.time()))

#first calculate omega because f_hat is in list form
# print(paste("calculate omega at", Sys.time()))
# cl <- parallel::makeCluster(no_cores)
# parallel::clusterExport(cl, c("n"))
# auto.cov<- do.call(cbind, parLapply(cl, f_hat, function(x) acf(x, type = "covariance", plot = F, lag.max = n, demean = T)$acf))
# parallel::stopCluster(cl)
# i<-as.matrix(seq(1,n-1))
# kernel <- c(0,apply(i,1,function(x) ((n-x)/n)*((1-q)^x) + (x/n)*((1-q)^(n-x)) ))
# 
# omega <-auto.cov*kernel #kernel has equal length as auto.cov rows, so this is a columnwise product.
# omega <- sqrt(auto.cov[1,] + (colSums(omega)*2))
# rm(auto.cov, kernel, i)

# mean returns of f_hat
f_bar <- vapply(f_hat, mean, na.rm = T, FUN.VALUE = numeric(1), USE.NAMES = F)

#calculate number of transactions and total excess return of best rule
best <- which(f_bar == max(f_bar, na.rm = T))[1]
print(paste("best is", best))
print(paste("best return is", f_bar[best]))
Transactions_best <- calc_cost(Rules[[best[1]]], 0, T)
ts <- Rules[[best[1]]]*benchmark
Return_best <- mean(ts)/DownsideDeviation(ts, method = "full", na.rm = T)
outperforming = sum(f_bar > 0)
rm(Rules)

print(paste("calculate f_mean_boot at", Sys.time()))

set.seed(120707)
array <- boot::boot.array(boot::tsboot(f_hat[[1]], mean, R = N, l = (1/q), sim ="geom", orig.t = F))

my_fun <- function(f_hat, array){
  apply(array, 1, function(x) mean(f_hat[x]))
}


cl <- parallel::makeCluster(no_cores)
parallel::clusterExport(cl, c("my_fun", "array"))
f_mean_boot <- do.call(rbind, parLapply(cl, f_hat, function(l) my_fun(l, array)))
parallel::stopCluster(cl)


rm(f_hat, my_fun, array)
print(paste("calculate spa at", Sys.time()))

print(paste("calculate omega at", Sys.time()))
omega <- apply(as.matrix(1:s), 1,function(x) sqrt(sum(((sqrt(n) * f_mean_boot[x,]) - (sqrt(n) * f_bar[x]))^2)/N))


#test statistic ####
test1 <- (sqrt(n) * f_bar) / omega
test1[is.na(test1)] <- 0
T_spa <- max(test1, 0)

g_l <- ifelse(f_bar > 0, f_bar, 0) #same as max(f_bar, 0) but 5 times as quick
Z_l <- f_mean_boot - g_l

#T_SPA_b,n
colMax <- function(x){
  apply(x, 2, function(y) max(c(y,0), na.rm = T))
}

T_spa_boot1 <- colMax(Z_l*sqrt(n) / omega)
p_l <- sum(T_spa_boot1 >= T_spa)/N


g_c <- ifelse(f_bar >= -sqrt(( (omega^2) / n) * 2* log( log(n) )), f_bar, 0)
Z_c <- f_mean_boot - g_c
T_spa_boot2 <- colMax((Z_c*sqrt(n)) / omega)
p_c <- sum(T_spa_boot2 >= T_spa)/N

Z_u <-  f_mean_boot - f_bar
T_spa_boot3 <- colMax((Z_u * sqrt(n)) / omega)
p_u <- sum(T_spa_boot3 >= T_spa)/N


p = p_c
best2 <- best[1]
num = 1
while(p <= 0.1){
  f_bar <- f_bar[-best2]
  omega <- omega[-best2]
  # f_mean_boot <- f_mean_boot[-best2, ]
  test1 <- test1[-best2]
  T_spa2 <- max(test1, 0)
  
  # g_c <- ifelse(f_bar >= -sqrt(( (omega^2) / n) * 2* log( log(n) )), f_bar, 0)
  Z_c <- Z_c[-best2, ]
  T_spa_boot2 <- colMax((Z_c*sqrt(n)) / omega)
  p <- sum(T_spa_boot2 > T_spa2)/N
  
  best2 <- which(f_bar == max(f_bar, na.rm = T))[1]
  print(p)
  if(p<=0.1){
    num = num + 1
  }
  print(num)
}

result  <- data.frame(Year = year, Type = ifelse(long, "Long", "Both"), 
                      Benchmark = ifelse(hodl, "Buy and hold", "Not in market"), 
                      Transactions = Transactions_best, SR = Return_best, 
                      cost = TC, T_value = T_spa,  p_l = p_l, p_c = p_c, 
                      p_u = p_u, best = best, outperforming = outperforming, num = num)
print(result)

write.csv(result, paste0(getwd(), "/Results/year=", year, "_cost=", TC, "_long=", long, "_hodl=", hodl, "_N=", N, "_hansen_q=",q , "_", "sortino", "_",exchange, currency, "_", Freq, ".csv"), row.names = F)




