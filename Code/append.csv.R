Freq <- c("day", "1hour","30min","15min","10min")


wdir <- getwd()

for (j in Freq) {
  setwd(paste0(wdir, "/Results/ClusterOutput"))
  
  Files <- list.files(pattern = paste0("\\N=1000_hansen_q=0.2_bitstampusd_", j,".csv$"))
  
  x <- read.csv(file = Files[1])[1,]
  
  for (i in Files[-1]) {
    print(i)
    
    x <- rbind(x, read.csv(i)[1,])
  }
  setwd(paste0(wdir, "/Results"))
  
  write.csv(x, paste0("N=1000_hansen_q=0.2_bitstampusd_", j,".csv"), row.names = FALSE)
}




setwd(wdir)
