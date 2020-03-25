# memory.limit(100000)
rm(list = ls())
Freq1 <- c(
                "day",
                # "12hours",
                 # "6hours",
                 # "4hours",
                #  # "2hours",
                "1hour",
                "30min",
                "15min"
                # "10min"
                # "5min"
                 #"3min", 
                 #"1min"
           )

q= 0.1
exchange1 <- c(
              # "mtgox"
              # ,
              "bitstamp"
              )

currency <- "usd"

for (ex in exchange1) {
  for (fr in Freq1) {
    exchange = ex
    Freq = fr
    # source("C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Code/calculation_MA.R", echo = T)
    # gdata::keep(Freq1, exchange1, q, currency, exchange, Freq, ex, fr, sure = T)
    # 
    # source("C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Code/calculation_triple_ma.R", echo = T)
    # gdata::keep(Freq1, exchange1, q, currency, exchange, Freq, ex, fr, sure = T)
    # source("C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Code/calculation_RSI2.R", echo = T)
    # gdata::keep(Freq1, exchange1, q, currency, exchange, Freq, ex, fr, sure = T)
    source("C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Code/calculation_filter2.R", echo = F)
    gdata::keep(Freq1, exchange1, q, currency, exchange, Freq, ex, fr, sure = T)
    # source("C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Code/calculation_S&R_2.R", echo = T)
    # gdata::keep(Freq1, exchange1, q, currency, exchange, Freq, ex, fr, sure = T)
    # source("C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Code/calculation_CB2.R", echo = T)
    # gdata::keep(Freq1, exchange1, q, currency, exchange, Freq, ex, fr, sure = T)
    # source('C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Data/data_preparation2.R', echo=TRUE)
    # gdata::keep(Freq1, exchange1, q, currency, exchange, Freq, ex, fr, sure = T)
    # gc()
    # source('C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Data/Hansen_calcul3.R', echo=TRUE)
    # gdata::keep(Freq1, exchange1, q, currency, ex, fr, sure = T)
    # gc()
    # source('C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Data/return_calcul.R', echo=TRUE)
    # gdata::keep(Freq1, exchange1, q, currency, ex, fr, sure = T)
    
    print(paste("Finished results for", ex, fr, "at", Sys.time()))
  }
  
}
