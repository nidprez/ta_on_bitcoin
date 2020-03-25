gc()

#libraries ####
library(NiekPhD)
library(parallel)
library(tidyverse)

# General Document options ####
# exchange <- "bitstamp"
# currency <- "usd"
# Freq <- "day"

dir <- paste0("C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Data/", 
              exchange, "/", exchange, currency, "_", Freq, ".csv")
wdir <- paste0("C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Data/", 
               exchange, "/")


# Import Data ####
data <- data.table::fread(dir)
data$Price <- zoo::na.locf(data$Price)
data$High <- zoo::na.locf(data$High)
data$Low <- zoo::na.locf(data$Low)
data$VolumeCurr[is.na(data$VolumeCurr)] <- 0 
data <- data %>% 
  mutate(Year = lubridate::year(data$Time), Ret = mydiff(log(Price))) #add factor containing the year
Year <- as.list(c("all", unique(data$Year)))

#### Moving Averages ####
MA <- NiekPhD::create_MA_lags(1, 200, 5) #create lags

MA_lags <- unique(do.call("c", MA)) #get the individual MA lags
MAs_list <- split(MA_lags, seq_along(MA_lags)) #put them in a list

#parallel computing
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterExport(cl , "data")
clusterEvalQ(cl, library("zoo"))

#calculate all the possible moving averages
MAs <- parLapply(cl, MAs_list, 
                 function(l){zoo::rollapply(data$Price, l, 
                                            function(x) mean(x, na.rm = T), 
                                                          fill = NA, align = "right")})
stopCluster(cl)
#put in dataframe
MAs <- do.call(cbind, MAs)
colnames(MAs) <- as.character(MA_lags)
rm(MAs_list)


# ** MA rules with no filter ####
Result <- data.frame()

cl <- makeCluster(no_cores)
clusterExport(cl, list("data", "Year", "MAs", "Freq"))
clusterEvalQ(cl, c(library(PerformanceAnalytics), library(dplyr), library(NiekPhD)))

#calculate the rules
Rules <- parLapply(cl, MA , function(l){calculate_crossingMA(MAs[ ,as.character(l[1])], 
                                                             MAs[ ,as.character(l[2])], 
                                                             data$Price)})

#save the rules in csv
write_csv(as.data.frame(do.call(cbind, Rules)), 
          path = paste0(wdir, exchange, currency,"_", Freq, "MA_filter=0_Rules.csv"))

#make a list containing the rules and the specifics of the MA lags (for later use)
L = list()
for (m in 1:length(Rules)) {
  L[[m]] <- list(Rules[[m]], MA[[m]])
}


#summarize the returns of the rules accross years, no transaction costs
Result1 <- parLapply(cl , L, function(l){summarize_returns(data, rule = l[[1]], 
                                                           Name = Freq,
                                                           Spec = c("MA", l[[2]]), Year = Year)})
Result1 <- data.table::rbindlist(Result1)
Result <- rbind(Result, Result1)
rm(Result1)

#summarize the returns of the rules accross years, 0.25% transaction costs
Result2 <- parLapply(cl , L, function(l){summarize_returns(data, rule = l[[1]], 
                                                           Name = Freq,
                                                           Spec = c("MA", l[[2]]), TC = 0.0025, 
                                                           Year = Year)})
Result2 <- data.table::rbindlist(Result2)

Result <- rbind(Result, Result2)
rm(Result2)
stopCluster(cl)

#save results
write_csv(Result, paste0(wdir, exchange, currency,"_", Freq, "MA_filter=0_Results.csv"))

#clean up memory
rm(Result, L, Rules)
gc() 

# ** MA rules with 1 % filter #######
Result <- data.frame()

cl <- makeCluster(no_cores)
clusterExport(cl, list("data", "Year", "MAs", "Freq"))
clusterEvalQ(cl, c(library(PerformanceAnalytics), library(dplyr), library(NiekPhD)))

#calculate the rules
Rules <- parLapply(cl, MA , function(l){calculate_crossingMA(MAs[ ,as.character(l[1])], 
                                                             MAs[ ,as.character(l[2])], 
                                                             data$Price,
                                                             filter = 0.01)})
#save the rules in csv
write_csv(as.data.frame(do.call(cbind, Rules)), 
          path = paste0(wdir, exchange, currency,"_", Freq, "MA_filter=0.01_Rules.csv"))

#make a list containing the rules and the specifics of the MA lags (for later use)
L = list()
for (m in 1:length(Rules)) {
  L[[m]] <- list(Rules[[m]], MA[[m]], 0.01)
}


#summarize the returns of the rules accross years, no transaction costs
Result1 <- parLapply(cl , L, function(l){summarize_returns(data, rule = l[[1]], 
                                                           Name = Freq,
                                                           Spec = c("MA", l[[2]]), Year = Year)})
Result1 <- data.table::rbindlist(Result1)
Result <- rbind(Result, Result1)
rm(Result1)

#summarize the returns of the rules accross years, 0.25% transaction costs
Result2 <- parLapply(cl , L, function(l){summarize_returns(data, rule = l[[1]], 
                                                           Name = Freq,
                                                           Spec = c("MA", l[[2]]), TC = 0.0025, 
                                                           Year = Year)})
Result2 <- data.table::rbindlist(Result2)

Result <- rbind(Result, Result2)
rm(Result2)
stopCluster(cl)


#save results
write_csv(Result, paste0(wdir, exchange, currency,"_", Freq, "MA_filter=0.01_Results.csv"))
  
#clean up memory
rm(Result, L, Rules)
gc() 


rm(MA, MAs, MA_lags)

#### RSI ####

RSI_specs <- create_RSI_specs(lag = c(2,100), lb = c(10,40), ub = c(60,90), inter = 1)

RSI_lags <- unique(RSI_specs$lag)
RSI_list <- split(RSI_lags, seq_along(RSI_lags)) #put them in a list

#parallel computing
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterExport(cl , "data")
clusterEvalQ(cl, library("TTR"))

#calculate all the possible moving averages
RSI <- parLapply(cl, RSI_list, 
                 function(l){TTR::RSI(data$Price, l)})
stopCluster(cl)
#put in dataframe
RSI <- do.call(cbind, RSI)
colnames(RSI) <- as.character(RSI_lags)
rm(RSI_list)

# ** RSI Rules ####
RSI_specs <- split(RSI_specs, seq_along(RSI_specs$lag))
Result <- data.frame()

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterExport(cl, list("data", "Year", "RSI", "Freq"))
clusterEvalQ(cl, c(library(PerformanceAnalytics), library(dplyr), library(NiekPhD)))

#calculate the rules
Rules <- parLapply(cl, RSI_specs , 
                   function(l){calculate_RSI(RSI[, as.character(l$lag)], lb = l$lb, ub = l$ub)})

#save the rules in csv
write_csv(as.data.frame(do.call(cbind, Rules)), 
          path = paste0(wdir, exchange, currency,"_", Freq, "RSI_Rules.csv"))

#make a list containing the rules and the specifics of the MA lags (for later use)
L = list()
for (m in 1:length(Rules)) {
  L[[m]] <- list(Rules[[m]], RSI_specs[[m]])
}


#summarize the returns of the rules accross years, no transaction costs
Result1 <- parLapply(cl , L, function(l){summarize_returns(data, rule = l[[1]], 
                                                           Name = Freq,
                                                           Spec = c("RSI", l[[2]]), Year = Year)})
Result1 <- data.table::rbindlist(Result1)
Result <- rbind(Result, Result1)
rm(Result1)

#summarize the returns of the rules accross years, 0.25% transaction costs
Result2 <- parLapply(cl , L, function(l){summarize_returns(data, rule = l[[1]], 
                                                           Name = Freq,
                                                           Spec = c("RSI", l[[2]]), TC = 0.0025, 
                                                           Year = Year)})
Result2 <- data.table::rbindlist(Result2)

Result <- rbind(Result, Result2)
rm(Result2)
stopCluster(cl)

#save results
write_csv(Result, paste0(wdir, exchange, currency,"_", Freq, "RSI_Results.csv"))

#clean up memory
rm(Result, L, Rules, RSI_specs, RSI, RSI_lags)
gc() 

#### Kaufman ER ####
ER_specs <- create_ER_specs(lag = c(5,100), inter_lag = 5, bound = c(0.05,0.5), inter = 0.01)

ER_lags <- unique(ER_specs$lag)
ER_list <- split(ER_lags, seq_along(ER_lags)) #put them in a list

#parallel computing
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterExport(cl , "data")
clusterEvalQ(cl, library("NiekPhD"))

#calculate all the possible moving averages
ER <- parLapply(cl, ER_list, 
                 function(l){Kaufman_ER(data$Ret, l)})
stopCluster(cl)
#put in dataframe
ER <- do.call(cbind, ER)
colnames(ER) <- as.character(ER_lags)
rm(ER_list)

# ** ER Rules ####
ER_specs <- split(ER_specs, seq_along(ER_specs$lag))
Result <- data.frame()

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterExport(cl, list("data", "Year", "ER", "Freq"))
clusterEvalQ(cl, c(library(PerformanceAnalytics), library(dplyr), library(NiekPhD)))

#calculate the rules
Rules <- parLapply(cl, ER_specs , 
                   function(l){calculate_Kaufman_ER(ER[, as.character(l$lag)], bound = l$Bound)})

#save the rules in csv
write_csv(as.data.frame(do.call(cbind, Rules)), 
          path = paste0(wdir, exchange, currency,"_", Freq, "ER_Rules.csv"))

#make a list containing the rules and the specifics of the MA lags (for later use)
L = list()
for (m in 1:length(Rules)) {
  L[[m]] <- list(Rules[[m]], ER_specs[[m]])
}


#summarize the returns of the rules accross years, no transaction costs
Result1 <- parLapply(cl , L, function(l){summarize_returns(data, rule = l[[1]], 
                                                           Name = Freq,
                                                           Spec = c("ER", l[[2]]), Year = Year)})
Result1 <- data.table::rbindlist(Result1)
Result <- rbind(Result, Result1)
rm(Result1)

#summarize the returns of the rules accross years, 0.25% transaction costs
Result2 <- parLapply(cl , L, function(l){summarize_returns(data, rule = l[[1]], 
                                                           Name = Freq,
                                                           Spec = c("ER", l[[2]]), TC = 0.0025, 
                                                           Year = Year)})
Result2 <- data.table::rbindlist(Result2)

Result <- rbind(Result, Result2)
rm(Result2)
stopCluster(cl)

#save results
write_csv(Result, paste0(wdir, exchange, currency,"_", Freq, "ER_Results.csv"))

#clean up memory
rm(Result, L, Rules, ER, ER_specs, ER_lags)
gc() 

#### CMF ####
CMF_specs <- create_CMF_specs(lag = c(5,100), inter_lag = 5, bound = c(0.05,0.5), inter = 0.01)

CMF_lags <- unique(CMF_specs$lag)
CMF_list <- split(CMF_lags, seq_along(CMF_lags)) #put them in a list

MFV <- Money_flow_volume(data$Price, data$Low, data$High, data$VolumeCurr)

#parallel computing
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterExport(cl , c("data", "MFV"))
clusterEvalQ(cl, library("NiekPhD"))

#calculate all the possible moving averages
CMF <- parLapply(cl, CMF_list, 
                function(l){CMF(MFV, data$VolumeCurr, l)})
stopCluster(cl)
#put in dataframe
CMF <- do.call(cbind, CMF)
colnames(CMF) <- as.character(CMF_lags)
rm(CMF_list)

# ** CMF Rules ####
CMF_specs <- split(CMF_specs, seq_along(CMF_specs$lag))
Result <- data.frame()

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterExport(cl, list("data", "Year", "CMF", "Freq"))
clusterEvalQ(cl, c(library(PerformanceAnalytics), library(dplyr), library(NiekPhD)))

#calculate the rules
Rules <- parLapply(cl, CMF_specs , 
                   function(l){calculate_CMF(CMF[, as.character(l$lag)], bound = l$Bound)})

#save the rules in csv
write_csv(as.data.frame(do.call(cbind, Rules)), 
          path = paste0(wdir, exchange, currency,"_", Freq, "CMF_Rules.csv"))

#make a list containing the rules and the specifics of the MA lags (for later use)
L = list()
for (m in 1:length(Rules)) {
  L[[m]] <- list(Rules[[m]], CMF_specs[[m]])
}


#summarize the returns of the rules accross years, no transaction costs
Result1 <- parLapply(cl , L, function(l){summarize_returns(data, rule = l[[1]], 
                                                           Name = Freq,
                                                           Spec = c("CMF", l[[2]]), Year = Year)})
Result1 <- data.table::rbindlist(Result1)
Result <- rbind(Result, Result1)
rm(Result1)

#summarize the returns of the rules accross years, 0.25% transaction costs
Result2 <- parLapply(cl , L, function(l){summarize_returns(data, rule = l[[1]], 
                                                           Name = Freq,
                                                           Spec = c("CMF", l[[2]]), TC = 0.0025, 
                                                           Year = Year)})
Result2 <- data.table::rbindlist(Result2)

Result <- rbind(Result, Result2)
rm(Result2)
stopCluster(cl)

#save results
write_csv(Result, paste0(wdir, exchange, currency,"_", Freq, "CMF_Results.csv"))

#clean up memory
rm(Result, L, Rules, CMF, CMF_specs, CMF_lags, MFV)
gc() 

