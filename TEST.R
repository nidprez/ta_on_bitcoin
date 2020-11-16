
library(parallel)
library(tidyverse)

no_cores=7
exchange="bitstamp"
currency="usd"
Freq="1hour"
q=0.1
year=2017
long=T
TC=0.001
hodl=T
N=1000
semester = 2


path <- paste0(getwd(), "/Results/")
bootres_path <- paste0(path, "bootres_meanret_year=", year, "_", semester, "_bitstampusd_", Freq, ".csv")
clusterres_path <- paste0(path, "fbar_meanret_year=", year, "_", semester, "_bitstampusd_", Freq, ".csv")

bootres <- data.table::fread(bootres_path)
clusterres <- data.table::fread(clusterres_path)

f_bar <- clusterres$f_bar
omega <- clusterres$omega
omega[omega == 0] = 1


wdir <- paste0(getwd(),"/Data/", exchange)
dir <- paste0(wdir, "/", exchange, currency, "_", Freq, ".csv")


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
data <- na.locf_bitcoincharts(data.table::fread(dir), Semester = T)

n <- as.numeric(data[Period == (year + (semester/10)), .(count = .N)])
s <- length(f_bar)

test1 <- (sqrt(n) * f_bar) / omega
test1[is.na(test1)] <- 0
T_spa <- max(test1, 0)

colMax <- function(x){
  apply(x, 2, function(y) max(c(y,0), na.rm = T))
}

g_c <- ifelse(f_bar >= -sqrt(( (omega^2) / n) * 2* log( log(n) )), f_bar, 0)
Z_c <- sqrt(n) * apply(bootres, 1, function(x){x - g_c}) / omega#(s X N) matrix
T_spa_boot <- colMax(Z_c)
p_c <- sum(T_spa_boot > T_spa)/N

colnames(bootres) <- as.character(1:s)

bootres <- as.matrix(bootres)

ttest <- apply(as.matrix(1:s), 1, 
               function(x) mean((sqrt(n)*f_bar[x])<= sqrt(n)*(bootres[, x]-f_bar[x]))) 
summary(p.adjust(ttest, method = "holm"))

white_boot <- apply(as.matrix(1:s), 1, 
                    function(x) sqrt(n)*(bootres[, x]-f_bar[x]))
V_i <- apply(white_boot, 1, max)
mean(max(sqrt(n)*f_bar) < V_i)

nonzero <- clusterres$nonzero

library(StepwiseTest)
fwerk <- FWERkControl(test1, t(bootres), 1, alpha = 0.1)
fwerk <- FWERkControl(ifelse(test1 < 0, 0, test1), ifelse(Z_c < 0, 0, Z_c), 100, alpha = 0.1)
fdp <- FDPControl(ifelse(test1 < 0, 0, test1), ifelse(Z_c < 0, 0, Z_c), 1,0.1)
sum(fwerk$Reject)
sum(fdp$Reject)

# hist(sqrt(n)*(bootres[, 10111]- f_bar[10111])/omega[10111])
# > abline(v=sqrt(n)*f_bar[10111]/omega[10111])
# > mean(sqrt(n)*f_bar[10111]/omega[10111] < sqrt(n)*(bootres[, 10111]- f_bar[10111])/omega[10111])

### FDR+ #####
pvals <- apply(as.matrix(1:s), 1, 
               function(x) mean(abs(f_bar[x]) <= abs(bootres[, x]-f_bar[x])))

lambda <- seq(0.3,0.7,0.05)

estimate_pi0_hat <- function(pvals, lambda){
  (sum(pvals > lambda) / length(pvals)) * (1 / (1 - lambda))
}

boot_pi0 <- function(pvals, i, lambda){
  sapply(lambda, function(x) estimate_pi0_hat(pvals[i], x))
}

library(boot)
boot <- boot(pvals, boot_pi0, 1000, lambda = lambda)

for(i in seq_along(lambda)){
  print(mean((boot$t[,i] - min(boot$t0))^2))
}

MSE_lambda <- apply(boot$t, 2, function(x) mean((x - min(boot$t0))^2))
lambda <- lambda[order(MSE_lambda)[1]]
pi_0 <- boot$t0[order(MSE_lambda)[1]]

gamma <- seq(0.3,0.5,0.05)

calculate_T <- function(pvals, f_bar, pos = T, pi_0, gamma){
  if(pos){
    (sum(pvals <= gamma & f_bar > 0)/length(pvals)) - (pi_0 * (gamma/2))
    
  }else{
    (sum(pvals <= gamma & f_bar < 0)/length(pvals)) - (pi_0 * (gamma/2))
    
  }
}

boot_T <- function(pvals, i, f_bar, pos = T, pi_0, gamma){
  sapply(gamma, function(x) calculate_T(pvals[i], f_bar[i], pos, pi_0, x))
}

T_boot_pos <- boot(pvals, boot_T, 1000, f_bar = f_bar, pi_0 = pi_0, gamma = gamma)
T_boot_neg <- boot(pvals, boot_T, 1000, f_bar = f_bar, pi_0 = pi_0, gamma = gamma, pos = F)

MSE_T_pos <- apply(T_boot_pos$t, 2, function(x) mean((x - max(T_boot_pos$t0))^2))
MSE_T_neg <- apply(T_boot_neg$t, 2, function(x) mean((x - max(T_boot_neg$t0))^2))

min_MSE_neg <- order(MSE_T_neg)[1]
min_MSE_pos <- order(MSE_T_pos)[1]

if(MSE_T_neg[min_MSE_neg] < MSE_T_pos[min_MSE_pos]){
  gamma <- gamma[min_MSE_neg]
  pi_A_pos <- T_boot_pos$t0[min_MSE_neg]
  pi_A_neg <- T_boot_neg$t0[min_MSE_neg]
}else{
  gamma <- gamma[min_MSE_pos]
  pi_A_pos <- T_boot_pos$t0[min_MSE_pos]
  pi_A_neg <- T_boot_neg$t0[min_MSE_pos]
}

pi_0 + pi_A_pos + pi_A_neg

FDR_pos <- ((1/2) * pi_0 * s * gamma) / max(sum(pvals <= gamma & f_bar > 0), 1)

calculate_FDR <- function(pvals, f_bar, pos = T, pi_0, gamma){
  ((1/2) * pi_0 * s * gamma) / max(sum(pvals <= gamma & f_bar > 0), 1)
}
ordered_p <- order(pvals)
ordered_p <- ordered_p[f_bar[ordered_p] > 0]


FDRS <- sapply(ordered_p, function(x) calculate_FDR(pvals, f_bar, pos = T, pi_0, pvals[x]))

FDRS <- ifelse(pvals[ordered_p] > lambda, 1, FDRS)
# pos_FDRS <- (FDRS[f_bar[ordered_p] > 0])

indices1 <- (which(FDRS <= 0.1))

if(sum(indices1) > 0){
  index2 <- tail(indices1,1)
}else{
  index2 <- tail(which(FDRS == min(FDRS)), 1)
}


FDR_10 <- FDRS[index2]
sig_rules <- rep(0, s)
sig_rules[ordered_p[1:index2]] <- 1
pvals[ordered_p[index2]]

for(i in order(pvals)){
  print(i)
  if(f_bar[i] > 0){
     print(((1/2) * pi_0 * s * pvals[i]) / max(sum(pvals <= pvals[i] & f_bar > 0), 1))
  }
}


0.5*pi_0*s*0.1/sum(pvals[pvals > 0] <= 0.1)
