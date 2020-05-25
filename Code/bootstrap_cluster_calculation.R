#!/usr/bin/env Rscript

#libraries ####
library(parallel)
library(tidyverse)
library(data.table)

# functions ####
mydiff <- function(x, lag = 1, differences = 1){
  a = diff(x, lag, differences)
  return(c(rep(NA, lag), a))
}

na.locf_bitcoincharts <- function(data, Anytime = TRUE, HighLow = F, LogRet = TRUE, Year = FALSE, Semester = FALSE){
  
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
    data$Ret <- mydiff(log(data$Price)) #add factor containing the year
  }
  
  if(Year){
    data$Period <- lubridate::year(data$Time) #add factor containing the year
  }
  
  if(Semester){
      data$Period <- lubridate::semester(data$Time, T) #add factor containing the year
  }
  return(data)
}

calc_return <- function(rule, benchmark, TC, RF){
  ret <- rule*benchmark
  
  
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
  
  cost[trades] <- log(1-TC[trades])
  
  
  my_ret <- ret + cost - (rule*RF)
  
  return(my_ret)
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

no_cores=3
exchange="bitstamp"
currency="usd"
Freq="day"
q=0.1
year=2015
N=1000
semester = 1

print(parallel::detectCores())

wdir <- paste0(getwd(),"/Data/", exchange, "/", Freq)
dir <- paste0(wdir, "/", exchange, currency, "_", Freq, ".csv")

dir1 <- paste0(wdir, "/", exchange, currency, "_", Freq, "_marules.csv")
dir2 <- paste0(wdir, "/", exchange, currency, "_", Freq, "_rsirules.csv")
dir3 <- paste0(wdir, "/", exchange, currency, "_", Freq, "_filterrules.csv")
dir4 <- paste0(wdir, "/", exchange, currency, "_", Freq, "_filter2rules.csv")
dir5 <- paste0(wdir, "/", exchange, currency, "_", Freq, "_supresrules.csv")
dir6 <- paste0(wdir, "/", exchange, currency, "_", Freq, "_cbrules.csv")
dir7 <- paste0(wdir, "/", exchange, currency, "_", Freq, "_obvrules.csv")


# Import Data ####
data <- na.locf_bitcoincharts(data.table::fread(dir), Year = F, Semester = T)


if(!semester){ #if data is not subdivided in semesters take data of chosen year
  x <- which(data$Period == year)
}else{ #otherwise take data of chosen year in particular semester
  x <- which(data$Period == (year+(semester / 10)))
} #Period variable should be in numeric format year.semester e.g. 2018.1

start <- x[1]
stop <- tail(x, 1)
data <- data[start:stop, ]

# set transaction costs
if(tail(data$Time, 1) <= as.POSIXct("2/3/2015", tz="UTC", format = "%d/%m/%Y")){
  TC <- rep(0.002, nrow(data))
}else if(data$Time[1] > as.POSIXct("2/3/2015", tz="UTC", format = "%d/%m/%Y")){
  TC <- rep(0.001, nrow(data))
}else{
  tmpsum <- sum(data$Time < as.POSIXct("2/3/2015", tz="UTC", format = "%d/%m/%Y"))
  TC <- c(rep(0.002, tmpsum), rep(0.001, nrow(data)-tmpsum))
}

rm(x, tmpsum)




# Prepare Rules strategy ####
#.Load in data

matchname <- paste0(Freq,"_(.*?)rules.csv")

my_fread <- function(x, start, stop, y){
  data.table::fread(x, skip = start, nrows = stop - start +1) %>% 
    set_names(paste0(stringr::str_match(x, y)[2],  c(1:ncol(.))))
    
}


Rules <- cbind(my_fread(dir1, start, stop, matchname), 
               my_fread(dir2, start, stop, matchname),
               my_fread(dir3, start, stop, matchname),
               my_fread(dir4, start, stop, matchname),
               my_fread(dir5, start, stop, matchname),
               my_fread(dir6, start, stop, matchname),
               my_fread(dir7, start, stop, matchname))
Rules[is.na(Rules)] <- 0

Rule_names <- names(Rules)


Rules <- as.matrix(Rules) %>% 
  unname()

nonzero <- apply(Rules, 2, function(x)any(x != 0))  #which rules are equal to the buy and hold strategy


print(paste("Rules is", format(object.size(Rules), "Gb")))
n = nrow(Rules)
s = ncol(Rules)

#+++ benchmark ####
benchmark <- data$Ret
benchmark[is.na(benchmark)] <- 0
RF <- data$RF


# ++++ Returns ####
print(paste("calculate f_hat at", Sys.time()))
cl <- parallel::makeCluster(no_cores)
print(paste("made cluster at", Sys.time()))
parallel::clusterExport(cl, list("calc_return", "benchmark", "TC", "nonzero", "RF"))
print(paste("export cluster at", Sys.time()))
f_hat <- matrix(0,n,s) #f_hat is (n x s)-matrix
dim_test <- dim(f_hat)
f_hat[, nonzero] <- parallel::parApply(cl, Rules[,nonzero], 2, function(x)calc_return(x, benchmark, TC, RF))
print(paste("parlapplied at", Sys.time()))
stopCluster(cl)

if(!all.equal(dim_test, dim(f_hat))){
  print("ERROR not equal dimensions")
}
# rm(Rules)


print(paste("f_hat calculated at", Sys.time()))
print(paste("f_hat is", format(object.size(f_hat), "Gb")))


#first calculate omega ####
print(paste("calculate omega at", Sys.time()))
cl <- parallel::makeCluster(no_cores)
parallel::clusterExport(cl, c("n","nonzero"))
auto.cov <- matrix(0,n,s)
dim_test <- dim(auto.cov)

auto.cov[, nonzero]<- parApply(cl, f_hat[, nonzero], 2, function(x) acf(x, type = "covariance", 
                                                  plot = F, lag.max = n, 
                                                  demean = T)$acf)
#auto.cov is (n x s) matrix-
parallel::stopCluster(cl)

if(!all.equal(dim_test, dim(auto.cov))){
  print("ERROR not equal dimensions")
}
i<-as.matrix(seq(1,n-1))
kernel <- c(0,apply(i,1,function(x) ((n-x)/n)*((1-q)^x) + (x/n)*((1-q)^(n-x)) ))

omega <-auto.cov*kernel #kernel has equal length as auto.cov rows, so this is a columnwise product.
omega <- sqrt(auto.cov[1,] + (colSums(omega)*2))
rm(auto.cov, kernel, i)

# mean returns of f_hat
f_bar <- colMeans(f_hat, na.rm = T)

res_df <- cbind(omega, f_bar, nonzero)
row.names(res_df) <- Rule_names


write.csv(res_df, file = paste0(wdir, "/fbar_meanret_year=",year, "_", semester, "_", 
                               exchange, currency, "_", Freq, ".csv"))
rm(Rules)

print(paste("calculate f_mean_boot at", Sys.time()))

set.seed(120707)
array <- boot::boot.array(boot::tsboot(f_hat[,1], mean, R = N, l = (1/q), sim ="geom", orig.t = F))

my_fun <- function(f_hat, array){
  apply(array, 1, function(x) mean(f_hat[x]))
}


cl <- parallel::makeCluster(no_cores)
parallel::clusterExport(cl, c("my_fun", "array", "nonzero"))
f_mean_boot <- matrix(0,N,s)
f_mean_boot[, nonzero] <- parApply(cl, f_hat[,nonzero], 2, function(l) my_fun(l, array)) #(N x s) matrix with mean returns 
parallel::stopCluster(cl)

write.csv(f_mean_boot, file = paste0(wdir, "/bootres_meanret_year=",year, "_", semester, "_", 
                                exchange, currency, "_", Freq, ".csv"),
          row.names = F)

