rm(list = ls())


no_cores <- 7
exchange1 <- c(
  # "mtgox"
  "bitstamp")

currency <- "usd"
Freq1 <- c(
  "day"
           # "12hours",
           # "6hours",
           # "4hours",
           #  # "2hours",
           # "1hour"
           # "30min"
           # "15min",
           # "10min"
           # "5min"
           #"3min", 
           #"1min"
)
q= 0.1
year1 <- c(2014:2018)
long1 <- c(T)
TC1 <- c(0.0025)
hodl1 <- c(T)
N = 1000

for (ex in exchange1) {
  for (fr in Freq1) {
    for(yr in year1){
      for(lng in long1){
        for(hdl in hodl1){
          for(cost in TC1){
            
         
    exchange = ex
    Freq = fr
    year = yr
    long = lng
    hodl = hdl
    TC = cost
    
    source("C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Code/Hansen_2005_HPC_cluster_SR.R")
    gdata::keep(Freq1, exchange1, year1, long1, hodl1, TC1, N, q, currency, 
                exchange, Freq, year, long, hodl, TC, ex, fr, yr, lng, hdl, 
                cost, no_cores, sure = T)
    
    # source('C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Data/data_preparation2.R', echo=TRUE)
    # gdata::keep(Freq1, exchange1, q, currency, exchange, Freq, ex, fr, sure = T)
    # gc()
    # source('C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Data/Hansen_calcul3.R', echo=TRUE)
    # gdata::keep(Freq1, exchange1, q, currency, ex, fr, sure = T)
    # gc()
    # source('C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Data/return_calcul.R', echo=TRUE)
    # gdata::keep(Freq1, exchange1, q, currency, ex, fr, sure = T)
    
    print(paste("##########  Finished results for", ex, fr, yr, lng, hdl, cost, "at", Sys.time(), "###########"))
  }
  
        }
      }
    }
  }
}
