####################################################################
# Calculate trading signals for every rule for different frequencies
####################################################################

# memory.limit(100000)
rm(list = ls())
Freq1 <- c("day",
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
exchange <- "bitstamp"

currency <- "usd"

  for (fr in Freq1) {
    
    Freq = fr
    source(paste0(getwd() ,"/Code/calculation_ma.R"), echo = F)
    gdata::keep(Freq1, exchange1, q, currency, exchange, Freq, fr, sure = T)
    
    source(paste0(getwd() ,"/Code/calculation_triple_ma.R"), echo = F)
    gdata::keep(Freq1, exchange1, q, currency, exchange, Freq, fr, sure = T)
    
    source(paste0(getwd() ,"/Code/calculation_rsi.R"), echo = F)
    gdata::keep(Freq1, exchange1, q, currency, exchange, Freq, fr, sure = T)
    
    source(paste0(getwd() ,"/Code/calculation_filter.R"), echo = F)
    gdata::keep(Freq1, exchange1, q, currency, exchange, Freq, fr, sure = T)
    
    source(paste0(getwd() ,"/Code/calculation_s&r.R"), echo = F)
    gdata::keep(Freq1, exchange1, q, currency, exchange, Freq, fr, sure = T)
    
    source(paste0(getwd() ,"/Code/calculation_cb.R"), echo = F)
    gdata::keep(Freq1, exchange1, q, currency, exchange, Freq, fr, sure = T)
    
    source(paste0(getwd() ,"/Code/calculation_obv.R"), echo = F)
    gdata::keep(Freq1, exchange1, q, currency, exchange, Freq, fr, sure = T)
    
    # gc()
    # source('C:/Users/nidprez/OneDrive - UGent/Research/Technical Analysis on Bitcoin/Data/return_calcul.R', echo=TRUE)
    # gdata::keep(Freq1, exchange1, q, currency, ex, fr, sure = T)
    
    print(paste("Finished results for", exchange, fr, "at", Sys.time()))
  }
  
