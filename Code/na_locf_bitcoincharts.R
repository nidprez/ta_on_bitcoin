#function to clean data set produced by bitcoincharts.

na.locf_bitcoincharts <- function(data, Anytime = TRUE, HighLow = TRUE, LogRet = TRUE, Year = FALSE){
  
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
    data$Year <- lubridate::year(data$Time) #add factor containing the year
  }
  
  
  return(data)
}