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

make_ret_tab <- function(Freq){
  bitdata <- na.locf_bitcoincharts(data.table::fread(
    paste0("Data/bitstamp/", Freq,"/bitstampusd_",Freq, ".csv")), Year = T)
  
  Ann_par <-  ifelse(Freq == "day",  365, 
                     ifelse(Freq == "1hour",  365 * 24,
                            ifelse(Freq == "30min",  365 * 48,
                                   ifelse(Freq == "15min",  365 * 48 * 2,
                                          ifelse(Freq == "10min",  365 * 48 * 3))))) 
  
  
  tab1 <- bitdata[Period >= 2012 & Period <= 2019, .(N = length(Ret),
                                                     Total = sum(Ret)*100, 
                                                     Mean = mean(Ret)*100,  
                                                     Min = min(Ret)*100, 
                                                     Max = max(Ret)*100, 
                                                     `Std. Dev.`= sd(Ret), 
                                                     Skewness = PerformanceAnalytics::skewness(Ret), 
                                                     Kurtosis = PerformanceAnalytics::kurtosis(Ret),
                                                     `Ann. Sharpe` = mean(Ret - RF)/sd(Ret - RF)*sqrt(Ann_par),
                                                     `Ann. Sortino` = PerformanceAnalytics::SortinoRatio(Ret)*sqrt(Ann_par),
                                                     
                                                     # acf = acf(Ret, lag.max = 5, type = "correlation")$acf[-1],
                                                     `Zero return`= sum(Ret == 0)/.N)]
  
  
  acfs <- matrix(acf(bitdata[Period >= 2012 & Period <= 2019]$Ret, 
                     lag.max = 5, type = "correlation", plot = F)$acf[-1], 
                 nrow = 5)
  row.names(acfs) <- paste0("$\\rho(", c(1:5), ")$")
  
  tab2 <-rbind(t(tab1), acfs)
  colnames(tab2) <- Freq
  
  
  # bitdata[-1, ] %>% 
  #   filter(Period >= 2012 & Period <= 2019) %>% 
  #   summarize(Total = sum(Ret), 
  #             Mean = mean(Ret),  
  #             Min = min(Ret), 
  #             Max = max(Ret), 
  #             `Std. Dev.`= sd(Ret), 
  #             Skewness = PerformanceAnalytics::skewness(Ret), 
  #             Kurtosis = PerformanceAnalytics::kurtosis(Ret),
  #             Sortino = PerformanceAnalytics::SortinoRatio(Ret),
  #             acf = acf(Ret, lag.max = 5, type = "correlation")$acf[2],
  #             `No trades`= sum(Ret == 0)/n())
  return(tab2)
}

ret_tab <- cbind(make_ret_tab("day"),
                 make_ret_tab("1hour"),
                 make_ret_tab("30min"),
                 # make_ret_tab("15min"),
                 make_ret_tab("10min"))
names <- row.names(ret_tab)
ret_tab2 <- ret_tab


ret_tab[1, ] <- round(ret_tab[1,], 0)
ret_tab[-1, ] <- round(ret_tab[-1, ], 3)

ret_tab[2:5, ] <- apply(ret_tab[2:5, ], 2, function(x)paste0(x, "\\%"))

ret_tab[-1, ] <- apply(ret_tab[-1, ], 2 , function(x)paste0("\\mc{", x, "}"))

sink("figures_tables/tab_sumstat_ret.tex")
knitr::kable(ret_tab,format = "latex", booktabs = T, escape = F,
             caption = "Descriptive statistics of the log return of the BTC/USD exchange rate per sampling frequency.")
sink()


# ret_tab2 <- apply(ret_tab, 2 , function(x)paste0("\\mc{", x, "}"))
# row.names(ret_tab2) <- names
# 
# 
# ret_tab[-1, ] <- apply(ret_tab[-1, ], 2 , function(x)paste0("\\mc{", x, "}"))
