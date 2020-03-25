########################################################
# Calculates the amount of trades per trading rule
########################################################

gc()

Freq <- c("day",
           # "12hours",
           # "6hours",
           # "4hours",
           #  # "2hours",
           "1hour",
           "30min",
           "15min",
           "10min"
           # "5min"
           #"3min", 
           #"1min"
)

Result <- data.frame(day = rep(NA, 21195), `1hour` = NA, `30min` = NA, `15min` = NA, `10min` = NA)
exchange <- "bitstamp"
currency <- "usd"

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

wdir <- paste0(getwd(),"/Data/", exchange)


for(i in Freq){
  dir1 <- paste0(wdir, "/", exchange, currency, "_", i, "MA_Rules.csv")
  dir2 <- paste0(wdir, "/", exchange, currency, "_", i, "triple_MA_Rules.csv")
  dir3 <- paste0(wdir, "/", exchange, currency, "_", i, "RSI_Rules.csv")
  dir4 <- paste0(wdir, "/", exchange, currency, "_", i, "SR_Rules.csv")
  dir5 <- paste0(wdir, "/", exchange, currency, "_", i, "filter_Rules.csv")
  dir6 <- paste0(wdir, "/", exchange, currency, "_", i, "filter2_Rules.csv")
  dir7 <- paste0(wdir, "/", exchange, currency, "_", i, "CB_Rules.csv")
  
  dir <- c(dir1,dir2,dir3,dir4,dir5,dir6,dir7)
  
  trades <- 0
  for(j in dir){
    Rules <- data.table::fread(j)
    trades2 <- apply(Rules, 2, function(x)calc_cost(x,0,T))
    trades <- c(trades, trades2)
  }
  
  trades <- trades[-1]
  Result[,i] <- trades
  
}

