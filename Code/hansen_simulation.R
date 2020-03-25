library(ttrbitcoin)
library(parallel)
benchmark1 <- rnorm(365+1250,0.0005,0.1)

lags <- c(1:50, seq(50,200,10))

result  <- data.frame(MAlag = rep(NA, length(lags)),Type = NA, 
                               Benchmark = NA, 
                               Transactions = NA, Return = NA, 
                               cost = NA, T_value = NA,  p_l = NA, p_c = NA, 
                               p_u = NA, best = NA, outperforming = NA)

count = 1
for (currentlag in lags) {
  print(currentlag)
  
  


benchmark <- TTR::SMA(benchmark1, currentlag)[-1:-currentlag]

P <- cumprod(exp(benchmark))

plot(tail(P, 365), type = "l", main = paste0("Price with SMA(", currentlag,")"))
# lines(TTR::SMA(tail(P,365), 10), col = "red")

p <- c(1,2,5,10,15,20,25,50,100,150,200) #lags in the short MA
q <- c(2,5,10,15,20,25,50,100,150,200,250) #lags in the long MA
n <- c(2,5,10,15,20,25,50,100,150)
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


colnames(MAs) <- as.character(MA_lags)
rm(MA_lags)

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

Rules <- do.call(cbind, Rules)

index <- benchmark %in% tail(benchmark, 365)

Rules <- Rules[index, ]
benchmark <- benchmark[index]


no_cores=7
q=0.2
long=F
TC=0
hodl=T
N=1000

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
Rules[is.na(Rules)] <- 0


#Long only strategy
if(long){
  Rules[Rules == -1] <- 0
}

Rules <- as.matrix(Rules)

print(paste("Rules is", format(object.size(Rules), "Gb")))
n = nrow(Rules)
s = ncol(Rules)

#+++ benchmark ####
benchmark[is.na(benchmark)] <- 0


print(paste("split Rules into list at", Sys.time()))
Rules <- split(Rules, col(Rules))

# ++++ Returns ####
print(paste("calculate f_hat at", Sys.time()))
cl <- parallel::makeCluster(no_cores)
print(paste("made cluster at", Sys.time()))
parallel::clusterExport(cl, list("calc_return", "benchmark", "hodl", "TC"))
print(paste("export cluster at", Sys.time()))
f_hat <- parallel::parLapply(cl, Rules, function(x)calc_return(x, benchmark, hodl, TC))
print(paste("parlapplied at", Sys.time()))
stopCluster(cl)
# rm(Rules)
# f_hat <- do.call(cbind, f_hat)

print(paste("f_hat calculated at", Sys.time()))
print(paste("f_hat is", format(object.size(f_hat), "Gb")))

#calculate hansen SPA ####
print(paste("start calculating hansen SPA at", Sys.time()))

#first calculate omega because f_hat is in list form
print(paste("calculate omega at", Sys.time()))
cl <- parallel::makeCluster(no_cores)
parallel::clusterExport(cl, c("n"))
auto.cov<- do.call(cbind, parLapply(cl, f_hat, function(x) acf(x, type = "covariance", plot = F, lag.max = n, demean = T)$acf))
parallel::stopCluster(cl)
i<-as.matrix(seq(1,n-1))
kernel <- c(0,apply(i,1,function(x) ((n-x)/n)*((1-q)^x) + (x/n)*((1-q)^(n-x)) ))

omega <-auto.cov*kernel #kernel has equal length as auto.cov rows, so this is a columnwise product.
omega <- sqrt(auto.cov[1,] + (colSums(omega)*2))
rm(auto.cov, kernel, i)

# mean returns of f_hat
f_bar <- vapply(f_hat, mean, na.rm =T, FUN.VALUE = numeric(1), USE.NAMES = F)

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
p_l <- sum(T_spa_boot1 > T_spa)/N


g_c <- ifelse(f_bar >= -sqrt(( (omega^2) / n) * 2* log( log(n) )), f_bar, 0)
Z_c <- f_mean_boot - g_c
T_spa_boot2 <- colMax((Z_c*sqrt(n)) / omega)
p_c <- sum(T_spa_boot2 > T_spa)/N

Z_u <-  f_mean_boot - f_bar
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

result[count, ]  <- data.frame(MAlag = currentlag,Type = ifelse(long, "Long", "Both"), 
                      Benchmark = ifelse(hodl, "Buy and hold", "Not in market"), 
                      Transactions = Transactions_best, Return = Return_best, 
                      cost = TC, T_value = T_spa,  p_l = p_l, p_c = p_c, 
                      p_u = p_u, best = best, outperforming = outperforming)
result
print(p_c)
count = count +1
}
