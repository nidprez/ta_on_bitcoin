######################################
# Check if a lot of double rules exist (with same trading signals)
######################################

#all rules for day,1h,30m,15m takes +-30min to run
# not necessary to run all the results

#libraries ####
library(parallel)
library(tidyverse)

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

calc_return <- function(rule, b_ret){
  ret <- (-rule)*b_ret
  return(ret)
}

# variables ####
freq <- c("day",
          # "12hours",
          # "6hours",
          # "4hours",
          #  # "2hours",
          "1hour",
          "30min",
          "15min"
          # "10min"
          # "5min"
          #"3min", 
          #"1min"
)

exchange <- "bitstamp"
currency <- "usd"
ttr <- c("ma", "triple_ma", "filter", "filter2", "rsi", "cb", "supres", "obv")

no_cores <- detectCores() - 1

# calculation ####
equal_rules <- list()
result_mean <- list()

for(i in freq){
  print(i)
  dir <- paste0(getwd(), "/Data/", exchange, "/", exchange, currency, "_", i, ".csv")
  data <- na.locf_bitcoincharts(data.table::fread(file = dir), Year = T)
  index <- which(data$Year >=2012 & data$Year <= 2019)
  start <- index[1]
  stop <- tail(index, 1)
  data <- data[index, ]
  benchmark <- data$Ret
  benchmark[is.na(benchmark)] <- 0
  
  for (j in ttr) {
    print(j)
    rule_dir <- paste0(getwd(), "/Data/", exchange, "/", exchange, currency, "_", i, "_", j, "rules.csv")
    rule <- data.table::fread(rule_dir, skip = start, nrows = stop - start + 1)
    
    print(paste("split Rules into list at", Sys.time()))
    rule <- as.list(rule)
      # split(rule, col(rule))
    
    # ++++ Returns ####
    print(paste("calculate f_hat at", Sys.time()))
    cl <- parallel::makeCluster(no_cores)
    print(paste("made cluster at", Sys.time()))
    parallel::clusterExport(cl, list("calc_return", "benchmark"))
    print(paste("export cluster at", Sys.time()))
    rule_return <- parallel::parLapply(cl, rule, function(x)calc_return(x, benchmark))
    print(paste("parlapplied at", Sys.time()))
    stopCluster(cl)
    
    mean_ret <- sapply(rule_return, mean, na.rm = T)
    
    num_eq <- rep(NA, length(mean_ret))
    
    for(k in seq_along(mean_ret)){
      num_eq[k] <- sum(mean_ret == mean_ret[k]) - 1
    }
    
    
    equal_rules[[i]][[j]] <- num_eq
    result_mean[[i]][[j]] <- mean_ret
  }
}

# results ####

#number of double rules
lapply(equal_rules, function(l)sapply(l, function(x)round(median(x), 2)))

#check percentage of outperforming rules
lapply(result_mean, function(l)sapply(l, function(x)round(sum(x>0)/length(x), 2)))

# annualized mean return
sapply(result_mean$day,     function(x)round(mean(x*365), 2))
sapply(result_mean$`1hour`, function(x)round(mean(x*365*24), 2))
sapply(result_mean$`30min`, function(x)round(mean(x*365*24*2), 2))
sapply(result_mean$`15min`, function(x)round(mean(x*365*24*2*2), 2))


