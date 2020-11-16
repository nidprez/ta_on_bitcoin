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
year=2017
long=T
TC=0.001
hodl=T
N=1000
semester = 2

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

if(tail(data$Time, 1) <= as.POSIXct("2/3/2015", tz="UTC", format = "%d/%m/%Y")){
  TC <- 0.002
}else if(data$Time[1] > as.POSIXct("2/3/2015", tz="UTC", format = "%d/%m/%Y")){
  TC <- 0.001
}else{
  tmpsum <- sum(data$Time <= as.POSIXct("2/3/2015", tz="UTC", format = "%d/%m/%Y"))
  TC <- c(rep(0.002, tmpsum), rep(0.001, nrow(data)-tmpsum))
}

rm(x)


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
Rule_names_unnum <- str_replace_all(Rule_names, "[:digit:]", "")
# Rules is a (n x s)-matrix


#Long only strategy
# if(long){
#   Rules[Rules == -1] <- 0
# }

Rules <- as.matrix(Rules) %>% 
  unname()

nonzero <- apply(Rules, 2, function(x)any(x != 0))  #which rules are equal to the buy and hold strategy

data.frame(Rule_names_unnum, nonzero) %>% 
  group_by(Rule_names_unnum) %>% 
  summarise(nonzero_p = sum(nonzero)/n(), N = n())

print(paste("Rules is", format(object.size(Rules), "Gb")))
n = nrow(Rules)
s = ncol(Rules)

#+++ benchmark ####
benchmark <- data$Ret
benchmark[is.na(benchmark)] <- 0


print(paste("split Rules into list at", Sys.time()))
# Rules <- split(Rules, col(Rules))

# ++++ Returns ####
print(paste("calculate f_hat at", Sys.time()))
cl <- parallel::makeCluster(no_cores)
print(paste("made cluster at", Sys.time()))
parallel::clusterExport(cl, list("calc_return", "benchmark", "hodl", "TC", "nonzero"))
print(paste("export cluster at", Sys.time()))
f_hat <- matrix(0,n,s)
dim_test <- dim(f_hat)
f_hat[, nonzero] <- parallel::parApply(cl, Rules[,nonzero], 2, function(x)calc_return(x, benchmark, 0, TC))
print(paste("parlapplied at", Sys.time()))
stopCluster(cl)

if(!all.equal(dim_test, dim(f_hat))){
  print("ERROR not equal dimensions")
}
# rm(Rules)
# f_hat <- do.call(cbind, f_hat)

# cbind(f_hat[,1:2], Rules[,1:2], benchmark)

print(paste("f_hat calculated at", Sys.time()))
print(paste("f_hat is", format(object.size(f_hat), "Gb")))

#calculate hansen SPA ####
print(paste("start calculating hansen SPA at", Sys.time()))

#first calculate omega because f_hat is in list form
print(paste("calculate omega at", Sys.time()))
cl <- parallel::makeCluster(no_cores)
parallel::clusterExport(cl, c("n","nonzero"))
auto.cov <- matrix(0,n,s)
dim_test <- dim(auto.cov)

# auto.cov<- do.call(cbind, parLapply(cl, f_hat, function(x) acf(x, type = "covariance", plot = F, lag.max = n, demean = T)$acf))
auto.cov[, nonzero]<- parApply(cl, f_hat[, nonzero], 2, function(x) acf(x, type = "covariance", 
                                                  plot = F, lag.max = n, 
                    v                              demean = T)$acf)
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
  # apply(f_hat, mean, na.rm =T, FUN.VALUE = numeric(1), USE.NAMES = F)

#calculate number of transactions and total excess return of best rule
best <- which(f_bar == max(f_bar, na.rm = T))[1]
print(paste("best is", best))
print(paste("best return is", f_bar[best]))
Transactions_best <- calc_cost(Rules[[best[1]]], 0, T)
outperforming = sum(f_bar > 0)

rm(Rules)
Return_best <- sum(f_hat[[best[1]]])

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
f_2 <- parApply(cl, f_hat[,nonzero], 2, function(l) my_fun(l, array))
# f_mean_boot <- do.call(rbind, parLapply(cl, f_hat, function(l) my_fun(l, array)))
parallel::stopCluster(cl)


rm(f_hat, my_fun, array)
print(paste("calculate spa at", Sys.time()))


#test statistic ####
test1 <- (sqrt(n) * f_bar) / omega
test1[is.na(test1)] <- 0
T_spa <- max(test1, 0)

# Individual bootstrap p.values, white(2000) p.1109
ttest <- apply(as.matrix(1:s), 1, 
               function(x) mean((sqrt(n)*f_bar[x])<= sqrt(n)*(f_mean_boot[, x]-f_bar[x]))) 
summary(p.adjust(ttest, method = "holm"))

white_boot <- apply(as.matrix(1:s), 1, 
                    function(x) sqrt(n)*(f_mean_boot[, x]-f_bar[x]))
V_i <- apply(white_boot, 1, max)
mean(max(sqrt(n)*f_bar) < V_i)


tmp1V <- sqrt(n)*f_bar[1]
tmp1Vi <- sqrt(n)*(f_mean_boot[, 1]-f_bar[1])
wtest <- rep(NA,s)
wtest[1] <- mean(tmp1Vi>tmp1V)
for (i in 2:s) {
  tmp2V <- max(sqrt(n)*f_bar[i], tmp1V)
  tmp2Vi <- pmax(sqrt(n)*(f_mean_boot[, i]-f_bar[i]), tmp1Vi)
  wtest[i] <- mean(tmp2Vi>tmp2V)
  tmp1V <- tmp2V
  tmp1Vi <- tmp2Vi
}
sample1 <- sample(1:s, round(s*0.95))

sum(pt(-abs(f_bar/(omega/sqrt(nrow(data)))), df = rep(nrow(data), length(f_bar))) < 0.1, na.rm = T)

summary(ttest[-sample1])

g_l <- ifelse(f_bar > 0, f_bar, 0) #same as max(f_bar, 0) but 5 times as quick (1 x s)
Z_l <- apply(f_mean_boot, 1, function(x){x - g_l}) #(s X N) matrix

#T_SPA_b,n
colMax <- function(x){
  apply(x, 2, function(y) max(c(y,0), na.rm = T))
}

T_spa_boot1 <- colMax(Z_l*sqrt(n) / omega)
p_l <- sum(T_spa_boot1 > T_spa)/N


g_c <- ifelse(f_bar >= -sqrt(( (omega^2) / n) * 2* log( log(n) )), f_bar, 0)
Z_c <- apply(f_mean_boot, 1, function(x){x - g_c}) #(s X N) matrix
T_spa_boot2 <- colMax((Z_c*sqrt(n)) / omega)
p_c <- sum(T_spa_boot2 > T_spa)/N

Z_u <- apply(f_mean_boot, 1, function(x){x - f_bar}) #(s X N) matrix
T_spa_boot3 <- colMax((Z_u * sqrt(n)) / omega)
p_u <- sum(T_spa_boot3 > T_spa)/N

# p = p_c
# best2 <- best[1]
# num = 1
# while(p <= 0.1){
#   f_bar <- f_bar[-best2]
#   omega <- omega[-best2]
#   # f_mean_boot <- f_mean_boot[-best2, ]
#   test1 <- test1[-best2]
#   T_spa2 <- max(test1, 0)
#   
#   # g_c <- ifelse(f_bar >= -sqrt(( (omega^2) / n) * 2* log( log(n) )), f_bar, 0)
#   Z_c <- Z_c[-best2, ]
#   T_spa_boot2 <- colMax((Z_c*sqrt(n)) / omega)
#   p <- sum(T_spa_boot2 > T_spa2)/N
#   
#   best2 <- which(f_bar == max(f_bar, na.rm = T))[1]
#   print(p)
#   if(p<=0.1){
#     num = num + 1
#   }
#   print(num)
# }

result  <- data.frame(Year = year, Type = ifelse(long, "Long", "Both"), 
                      Benchmark = ifelse(hodl, "Buy and hold", "Not in market"), 
                      Transactions = Transactions_best, Return = Return_best, 
                      cost = TC, T_value = T_spa,  p_l = p_l, p_c = p_c, 
                      p_u = p_u, best = best, outperforming = outperforming)
result



write.csv(result, paste0(getwd(), "/Results/year=", year, "_cost=", TC, "_long=", long, "_hodl=", hodl, "_N=", N, "_hansen_q=",q , "_", exchange, currency, "_", Freq, ".csv"), row.names = F)




