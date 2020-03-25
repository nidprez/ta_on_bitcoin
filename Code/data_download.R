library(NiekPhD)
library(parallel)
library(tidyverse)

# Download Data ####
exchange <- "bitstamp"
currency <- "usd"

dir <- paste0(getwd(), "/Data/", exchange, "/")

library(readr)
data <- read_csv("Data/bitstamp/bitstampusd.csv")
names(data) <- c("Time", "Price", "Volume")
data$Time <- anytime::utctime(data$Time)

data <- get_histdata_bitcoincharts(exchange, currency, dir)



# Put in different frequencies ####
Freq <- c("day", 
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

for (i in Freq) {
  print(i)
  bitcoincharts_tidy_hl(data = data, i, paste0(dir, "/", exchange, currency))
}

data[data$Year==2014,][1,]


