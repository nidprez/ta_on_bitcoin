rm(list = ls())

library(NiekPhD)
library(parallel)
library(tidyverse)
library(readr)
library(asbio)

# Download Data ####
exchange <- "bitstamp"
currency <- "usd"

dir <- paste0(getwd(), "/Data/", exchange, "/")

# data <- read_csv("Data/bitstamp/bitstampusd.csv", 
#                  col_names = F)
# names(data) <- c("Time", "Price", "Volume")
# data <- data %>% 
#   mutate(Time = anytime::utctime(Time))

get_histdata_bitcoincharts <- function(exchange, currency, dir = NULL){
  
  slur <- paste0(tolower(exchange), toupper(currency), ".csv.gz")
  available <- list_bitcoincharts()
  # Check if exchangedata exists on bitcoincharts
  if(!(slur %in% available$Active | slur %in% available$Inactive)){
    stop("The exchange is not specified, check in list_bitcoincharts() function.")
  }
  
  if(slur %in% available$Active & !slur %in% available$Inactive){
    url <- paste0("http://api.bitcoincharts.com/v1/csv/", slur)
  }else{
    url <- paste0("http://api.bitcoincharts.com/v1/csv/inactive_exchanges/", slur)
  }
  
  
  if(is.null(dir)){
    temp <- tempfile()
    temp2 <- tempfile()
    download.file(url, temp)
    R.utils::gunzip(temp, temp2, temporary = TRUE)
    data <- data.table::fread(temp2)
    unlink(temp)
    unlink(temp2)
  }else{
    savefile <- paste0(dir, "/", exchange, currency, ".csv.gz")
    savefile2 <- paste0(dir, "/", exchange, currency, ".csv")
    if(file.exists(savefile2)){   #remove the csv if it already exists
      file.remove(savefile2)
    }
    
    download.file(url, savefile)
    R.utils::gunzip(savefile, savefile2)
    data <- data.table::fread(savefile2)
  }
  names(data) <- c("Time", "Price", "Volume")
  data$Time <- anytime::utctime(data$Time)
  
  return(data)
}

# data <- get_histdata_bitcoincharts(exchange, currency, dir)

#clean data - Brownlees & Gallo (2006) ####
data <- data[!(data$Volume <= 0), ]

#if trades happen at the same time, take median price
data <- data %>% 
  group_by(Time) %>% 
  summarise(Price = median(Price), Volume = sum(Volume)) %>% 
  mutate(Year = lubridate::year(Time))


#determine which data points could be outliers
p_list <- data %>% 
  select(Price, Year) %>% 
  group_split(Year, keep = F)

names(p_list) <- unique(data$Year)


clean_data <- function(data, delta, year){
  
  #predefine function for loop
  closest <- function(x, k, k2, n){
    if(x <= k2){
      c(1:(k+1))[-x]
    }else if((x + k2) > n){
      c((n-k):n)[-abs(n-x-1-k)]
    }else{
      c((x-k2):(x-1), (x+1):(x+k2))
    }
  }
  
  #variables used
  if(year <= 2013){
    gamma <- 0.5
    k <- 40
  }else if(year <= 2016){
    gamma <- 1.5
    k <- 60
  }else{
    gamma <- 10
    k <- 60
  }
  
  k2 <- k/2
  n <- length(data)
  
  outlier <- logical(n)
  
  
  for (i in 1:n){
    p <- asbio::trim.me(data[closest(i, k, k2, n)], delta)
    outlier[i] <- ifelse(abs(data[i] - mean(p)) < (3*(sd(p) + gamma)), F, T)
    # if(i%%10000){print(paste(round(i/n*100, 4), "% done"))}
  }
  return(outlier)
}

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterExport(cl, list("p_list", "clean_data"))
clusterEvalQ(cl, c(library(asbio)))
print(paste("Calculating outliers at", Sys.time()))

outlier <- parLapply(cl, seq_along(p_list), 
                   function(i, l, n){clean_data(l[[i]]$Price, 0.05, as.numeric(n[[i]]))}, 
                   l = p_list, n = names(p_list))

print(paste("outliers Calculated at", Sys.time()))
stopCluster(cl)

outlier <- do.call(c, outlier)

outliers <- data[outlier, ]

readr::write_csv(outliers, 
                 path = paste0(dir, exchange, currency, "_outliers.csv"))

data <- data[!outlier, -4]

readr::write_csv(data, 
                 path = paste0(dir, exchange, currency, "_clean.csv"))

# load clean data ####
data <- read_csv(paste0(dir, "bitstampusd_clean.csv"))


# Put in different frequencies ####
bitcoincharts_tidy_hl <- function(data, Freq, Savedir = NULL){
  
  #Prepare data
  names(data) <- c("Time", "Price", "Volume")
  data <- data %>%
    mutate(VolumeCurr = Price*Volume)
  data <- data[,-3]
  
  #convert per millisecond data to per Freq data
  data$Time <- lubridate::floor_date(data$Time, unit = Freq)
  #Make a dataframe with only Freq intervals
  y <- data.frame(Time = seq(data$Time[1], tail(data$Time, 1), by = Freq))
  
  
  #Look at points in time with more than one observation and take latest obs
  Index <- !duplicated(data$Time, fromLast = T)
  Index1 <- which(Index)#latest prices
  Index2 <- c(0, Index1[-length(Index1)]) + 1 #Observations after last price
  Index3 <- cbind(Index2, Index1)
  
  data2 <- data[Index, ]
  data2$VolumeCurr <- apply(Index3, 1, function(x){sum(data$VolumeCurr[x[1]:x[2]])})
  High <- apply(Index3, 1, function(x){max(data$Price[x[1]:x[2]])})
  Low <- apply(Index3, 1, function(x){min(data$Price[x[1]:x[2]])})
  
  data2 <- cbind(data2, High, Low)
  
  Result <- right_join(data2, y)
  
  
  if(!is.null(Savedir)){
    write_csv(Result, paste0(Savedir,"_", gsub(" ", "", Freq), ".csv"))
  }
  return(Result)
}

Freq <- c(
  # "day", 
          # "12 hours", 
          # "6 hours", 
          # "4 hours", 
          # "2 hours", 
          "1 hour", 
          "30 min", 
          "15 min", 
          "10 min"
          # "5 min" 
          # "3 min", "1 min"
)

for(i in Freq){
  print(i)
  bitcoincharts_tidy_hl(data = data, i, paste0(dir, gsub(" ", "", i), "/", exchange, currency))
}

data[data$Year==2014,][1,]


