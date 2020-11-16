source('C:/Users/nidprez/OneDrive - UGent/Research/ta_on_bitcoin/Code/find_ttr_rule.R')


no_cores=7
exchange="bitstamp"
currency="usd"
Freq="30min"
q=0.1
year=2016
long=T
TC=0.001
hodl=T
N=1000
semester = 2


path <- paste0(getwd(), "/Results/")

index <- c(NULL)


for(year in 2013:2019){
  for(semester in 1:2){
    for(Freq in c("day", "1hour", "30min", "10min")){
      

clusterres_path <- paste0(path, "fbar_meanret_year=", year, "_", semester, "_bitstampusd_", Freq, ".csv")

clusterres <- data.table::fread(clusterres_path)

f_bar <- clusterres$f_bar
omega <- clusterres$omega
nonzero <- clusterres$nonzero

index <- c(index, which(!nonzero))

print(c("#########", year, semester, Freq, "#########"))
print(prop.table(table(nonzero, sapply(Rules, function(x) x$rule)), 2))

    }
  }
}
