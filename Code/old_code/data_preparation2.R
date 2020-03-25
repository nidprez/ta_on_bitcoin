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

#### Moving Averages ####
MA <- NiekPhD::create_MA_lags(1, 100, 1) #create lags

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

cl <- makeCluster(no_cores)
clusterExport(cl, list("MAs"))
clusterEvalQ(cl, c(library(PerformanceAnalytics), library(dplyr), library(NiekPhD)))

print(paste("Calculating Moving average Rules at", Sys.time()))
#calculate the rules
Rules <- lapply(1:length(MA), function(i) integer(nrow(data)))
Rules <- parLapply(cl, MA , function(l){calculate_crossingMA(MAs[ ,as.character(l[1])], 
                                                             MAs[ ,as.character(l[2])])})
print(paste("Moving average Rules Calculated at", Sys.time()))
stopCluster(cl)

#save the rules in csv
print(paste("Saving Moving average Rules at", Sys.time()))
write_csv(as.data.frame(do.call(cbind, Rules)), 
          path = paste0(wdir, exchange, currency,"_", Freq, "MA_filter=0_Rules.csv"))
print(paste("Moving average Rules Saved at", Sys.time()))


rm(Rules)
gc()

#** MA with filter #####
# #calculate the rules
# Rules <- lapply(1:length(MA), function(i) integer(nrow(data)))
# Rules <- parLapply(cl, MA , function(l){calculate_crossingMA(MAs[ ,as.character(l[1])], 
#                                                              MAs[ ,as.character(l[2])], 
#                                                              data$Price,
#                                                              filter = 0.01)})
# #save the rules in csv
# write_csv(as.data.frame(do.call(cbind, Rules)), 
#           path = paste0(wdir, exchange, currency,"_", Freq, "MA_filter=0.01_Rules.csv"))
# stopCluster(cl)

rm(MA, MAs, MA_lags)
#clean up memory

gc() 



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
                 function(l){TTR::RSI(data$Price, n = l)})
stopCluster(cl)
#put in dataframe
RSI <- do.call(cbind, RSI)
colnames(RSI) <- as.character(RSI_lags)
rm(RSI_list)

# ** RSI Rules ####
RSI_specs <- split(RSI_specs, seq_along(RSI_specs$lag))

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterExport(cl, list("data", "RSI"))
clusterEvalQ(cl, c(library(PerformanceAnalytics), library(dplyr), library(NiekPhD)))

#calculate the rules
print(paste("Calculating RSI at", Sys.time()))
Rules <- lapply(1:length(RSI_specs), function(i) integer(nrow(data)))
Rules <- parLapply(cl, RSI_specs , 
                   function(l){calculate_RSI(RSI[, as.character(l$lag)], lb = l$lb, ub = l$ub)})
stopCluster(cl)

print(paste("RSI calculated at", Sys.time()))
#save the rules in csv
print(paste("Saving RSI at", Sys.time()))
write_csv(as.data.frame(do.call(cbind, Rules)), 
          path = paste0(wdir, exchange, currency,"_", Freq, "RSI_Rules.csv"))
print(paste("RSI Rules Saved at", Sys.time()))



#clean up memory
rm(Rules, RSI_specs, RSI, RSI_lags)
gc() 

