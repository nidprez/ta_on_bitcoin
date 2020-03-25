library(NiekPhD)
library(boot)
# Data preparation ####
rm(list = ls())

library(readxl)
data <- read_excel("C:/Users/nidprez/OneDrive - UGent/Teaching/Investment analysis/2018-2019/Assignment1_2018-2019.xlsx",
                                    sheet = "Factors")

data2 <- read_excel("C:/Users/nidprez/OneDrive - UGent/Teaching/Investment analysis/2018-2019/Assignment1_2018-2019.xlsx",
                    sheet = "Portfolios")

models <- as.matrix(cbind(data[, -1:-2], data2[,-1]))
models[is.na(models)] <- 0
benchmark <- as.matrix(data[, 2])

# Ret <- rnorm(120, 0, 0.05)
# Ret <- round(Ret, 4)
# plot(Ret, type = "l")
# n = length(Ret)
# P <- numeric(n+1)
# P[1] <- 100
# for (i in 2:(n+1)) {
#   P[i] = P[i-1]*(1 + Ret[i-1])
# }
#
# plot(P, type = "l")
#
# lags <- create_MA_lags(1,20,5)
# Rules <- lapply(lags, function(x){calculate_crossingMA2(x[1],x[2], P)})
# Rules <- do.call(cbind, Rules)
#
# RSI <- create_RSI_specs(c(1,15),c(25,35),c(65,75), 5)
# TTR::RSI()
# Rules <- calculate_RSI()
#
# Rules2 <- Rules[-1:-21, ]
# benchmark <- Ret[-1:-20]
#
# models <- apply(Rules2, 2, function(x)x*benchmark)

# White RC http://www.cristiandima.com/white-s-reality-check-for-data-snooping-in-r/ ####

n = length(benchmark)
s = ncol(models)
N = 500
q = 0.1
f_hat <- apply(models, 2 , function(x)x-benchmark)
f_bar <- Rfast::colmeans(f_hat)


#get the bootstrapped means f_star_bar_t
f_mean_boot <- boot::tsboot(f_hat, Rfast::colmeans, R = N, l = (1/q), sim ="geom", orig.t = F)$t

#alternative way to get the same
# theta <- boot.array(tsboot(f_hat, colMeans, R = 500, l = 10, sim ="geom", orig.t = F))
#
#
# f <- t(f_hat) #matrix holding n daily log returns
# #for each strategy x
# f.mean.boot <- matrix(nrow=s, ncol=N) #N bootstrapped resample means
# #for each x strategy
# for(x in 1:s){ #go through each model
#   for(i in 1:N){ #resample N times
#     f.mean.boot[x,i] = mean(f[x,theta[i,]]) #mean of resampled f[x,]
#     #using theta[i,]
#   }
# }
#

V <- sqrt(n)*f_bar
V_boot <- sqrt(n) * (t(f_mean_boot) - f_bar)
final_V_boot <- Rfast::colMaxs(V_boot, value = T)
p <- sum(final_V_boot > max(V))/N
p

prev.V <- sqrt(n)*f_bar[1] #V1
prev.V.boot <- sqrt(n)*(f_mean_boot[ ,1] - f_bar[1])
current.V.boot <- numeric(N)
for(x in 2:s){ #start from V2 and go on till Vs
  current.V <- max((sqrt(n)*f_bar[x]),prev.V)
  for(i in 1:N){
    current.V.boot[i] <- max(sqrt(n)*(f_mean_boot[i,x] - f_bar[x]),prev.V.boot[i])

  }
  prev.V.boot <- current.V.boot
  prev.V <- current.V
  print(length(which(current.V.boot > current.V))/N)
}
p <- length(which(current.V.boot > current.V))/N #returns p-value for best model
p

# Hansen ####

omega2 <- apply(as.matrix(1:s), 1, function(x) mean(((sqrt(n)*f_mean_boot[, x]) - (sqrt(n)*f_bar[x]))^2))

auto.cov<-apply(f_hat,2, function(x) acf(x, type = "covariance", plot = F, lag.max = n)$acf)
i<-as.matrix(seq(1,nrow(f_hat)-1))
kernel <- c(0,apply(i,1,function(x) ((n-x)/n)*((1-q)^x) + (x/n)*((1-q)^(n-x)) ))
omega <-auto.cov*kernel #kernel has equal length as auto.cov rows, so this is a columnwise product.
omega <- sqrt(auto.cov[1,] + colSums(omega)*2)

# gammai <- numeric(n)
# for (i in 0:(n-1)) {
#   test <- numeric(n-i)
#   for (j in 1:length(test)) {
#     test[j] <- (f_hat[j,1]-f_bar[1]) * (f_hat[j+i,1]-f_bar[1])
#   }
#   gammai[i+1] <- sum(test)/n
# }


  T_spa <- max(sqrt(n)*f_bar/omega, 0)

  # g_l <- apply(as.matrix(f_bar), 1, function(x) max(x,0))
  g_l <- ifelse(f_bar > 0, f_bar, 0) #same as max(f_bar, 0) but 5 times as quick
  
  Z_l <- t(f_mean_boot) - g_l
  
  #T_SPA_b,n
  T_spa_boot1 <- Rfast::colMaxs(Z_l*sqrt(n) / omega, value = T)
  #T_spa_boot_l <- ifelse(T_spa_boot1 > 0, T_spa_boot1, 0)
  p_l <- sum(T_spa_boot1 > T_spa)/N

  
  
  # g_c <- rep(NA,length(f_bar) )
  # for(i in 1:length(f_bar)){
  #   g_c[i] <- f_bar[i]*ifelse(f_bar[i]>=-sqrt(((omega[i])/n)*2*log(log(n))),1,0)
  # }

  
  g_c <- ifelse(f_bar >= -sqrt(( (omega^2) / n) * 2* log( log(n) )), f_bar, 0)
  Z_c <- t(f_mean_boot) - g_c
  T_spa_boot2 <- Rfast::colMaxs((Z_c*sqrt(n)) / omega, value = T)
  #T_spa_boot_c <- ifelse(T_spa_boot2 > 0, T_spa_boot2, 0)
  p_c <- sum(T_spa_boot2 > T_spa)/N
  
  Z_u <-  t(f_mean_boot) - f_bar
  T_spa_boot3 <- Rfast::colMaxs((Z_u * sqrt(n)) / omega, value = T)
  #bT_spa_boot_u <- ifelse(T_spa_boot3 > 0, T_spa_boot3, 0)
  p_u <- sum(T_spa_boot3 > T_spa)/N
  
  hansen_spa <- function(f_hat, N, q){
    n = nrow(f_hat)
    s = ncol(models)
    f_bar <- Rfast::colmeans(f_hat) #calculate the means
    
    #bootstrap
    f_mean_boot <- boot::tsboot(f_hat, Rfast::colmeans, R = N, l = (1/q), sim ="geom", orig.t = F, parallel = "snow")$t
    
    #estimate omega with kernel weights
    auto.cov<-apply(f_hat,2, function(x) acf(x, type = "covariance", plot = F, lag.max = n, demean = T)$acf)
    i<-as.matrix(seq(1,nrow(f_hat)-1))
    kernel <- c(0,apply(i,1,function(x) ((n-x)/n)*((1-q)^x) + (x/n)*((1-q)^(n-x)) ))

    omega <-auto.cov*kernel #kernel has equal length as auto.cov rows, so this is a columnwise product.
    omega <- sqrt(auto.cov[1,] + colSums(omega)*2)
    
    #test statistic
    T_spa <- max(sqrt(n)*f_bar/omega, 0)
    
    g_l <- ifelse(f_bar > 0, f_bar, 0) #same as max(f_bar, 0) but 5 times as quick
    Z_l <- t(f_mean_boot) - g_l
    
    #T_SPA_b,n
    T_spa_boot1 <- Rfast::colMaxs(Z_l*sqrt(n) / omega, value = T)
    p_l <- sum(T_spa_boot1 > T_spa)/N
    
    
    g_c <- ifelse(f_bar >= -sqrt(( (omega^2) / n) * 2* log( log(n) )), f_bar, 0)
    Z_c <- t(f_mean_boot) - g_c
    T_spa_boot2 <- Rfast::colMaxs((Z_c*sqrt(n)) / omega, value = T)
    p_c <- sum(T_spa_boot2 > T_spa)/N
    
    Z_u <-  t(f_mean_boot) - f_bar
    T_spa_boot3 <- Rfast::colMaxs((Z_u * sqrt(n)) / omega, value = T)
    p_u <- sum(T_spa_boot3 > T_spa)/N
    
    return(c(p_l,p_c,p_u))
  }

  testh <-hansen_spa(f_hat, 500, 0.2)
