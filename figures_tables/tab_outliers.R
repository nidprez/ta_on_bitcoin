data <- data.table::fread("Data/bitstamp/bitstampusd_outliers.csv")
data <- data[Year != 2020 & Year != 2011]
freqtab <- data[, .N, by = Year]
