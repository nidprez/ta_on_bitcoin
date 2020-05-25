library(tidyverse)


ff_url <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip"
# Create temp_file to store the file

temp_file <- tempfile()

# Download the file

download.file(ff_url, temp_file)

# Unzip the file, to extract the data

ff_factors_raw_data <- unzip(temp_file)

# Skipping the first 3 rows

ff_factors_raw_data <- read_csv(ff_factors_raw_data, skip = 3) %>% 
  set_names(c("Time", names(.)[-1]))
ff_factors_raw_data <- ff_factors_raw_data[-nrow(ff_factors_raw_data), ]

# Check the data

head(ff_factors_raw_data)
tail(ff_factors_raw_data)

ff_factors_raw_data$Time <- as.POSIXct(as.character(ff_factors_raw_data$Time), format = "%Y%m%d", tz = "UTC")

df_rf <- as.data.frame(ff_factors_raw_data[, c("Time", "RF")])

# library(readr)
df_rf$RF <- log((df_rf$RF / 100) + 1)

x <- read_csv("Data/bitstamp/30min/bitstampusd_30min.csv")

add_RF <- function(path, df_rf, denominator){
  x <- read_csv(path)
  df_rf$RF <- df_rf$RF/denominator
  if(!("RF" %in% names(x))){
    x <- merge(x, df_rf, by = "Time", all.x = T)
    x$RF <- zoo::na.locf0(x$RF)
    readr::write_csv(x, 
                     path = path)
    print(paste("succesfully merged RF in df", path))
  }else{
    print(paste("RF was already present in", path))
  }
}


add_RF("Data/bitstamp/day/bitstampusd_day.csv", df_rf, 1)

add_RF("Data/bitstamp/1hour/bitstampusd_1hour.csv", df_rf, 24)
add_RF("Data/bitstamp/30min/bitstampusd_30min.csv", df_rf, (24*2))
add_RF("Data/bitstamp/15min/bitstampusd_15min.csv", df_rf, (24*4))
add_RF("Data/bitstamp/10min/bitstampusd_10min.csv", df_rf, (24*6))

