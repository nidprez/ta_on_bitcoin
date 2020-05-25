library(tidyverse)
library(data.table)
library(tikzDevice)
library(kableExtra)

#figure options ####
figure_width <- 4.5 #in Inches lates point is (1/72.27) Inch
height_ratio <- 0.75
theme_set(theme_classic() + theme(panel.grid.major = element_line(colour = "grey92")))

my_tikz <- function(savename, figure_width, height_ratio){
  figure_height <- height_ratio*figure_width
  tikz(paste0(savename, "_width=",figure_width, "inch_ratio=", height_ratio, ".tex"), width = figure_width, height = figure_height)
}


#dataset ####
data <- data.table::fread("Data/bitstamp/bitstampusd_clean.csv")
data$Year <- lubridate::year(data$Time)

data <- data[Year != 2020 & Year != 2011]

freqtab <- data[, .N, by = Year]

my_freqtab <- t(freqtab)
colnames(my_freqtab) <- freqtab$Year

sink("figures_tables/tab_tradefreq.tex")
t(my_freqtab[2,]) %>% 
  kable(format = "latex", booktabs = T, digits = 2, 
        caption = "Number of trades on Bitstamp in BTC/USD per year.")
sink()


my_tikz("figures_tables/fig_tradefreq", figure_width, height_ratio)
Years <- unique(freqtab$Year)
freqtab %>% 
 ggplot(aes(x = Year, y = N)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Time", y = "Frequency (in millions)") + 
  scale_y_continuous(breaks = seq(1,11,1)*1e6, labels = as.character(seq(1,11,1)), minor_breaks = NULL) +
  scale_x_continuous(labels = as.character(Years), breaks = Years, minor_breaks = NULL)

dev.off()

data$VolumeUSD <- data$Price*data$Volume

summary1 <- data[, c(as.list(summary(Price)), StDev = sd(Price)), by="Year"]

sink("figures_tables/tab_summary_price.tex")
kable(summary1,format = "latex", booktabs = T, digits = 2,
      caption = "Summary statistics of the BTC/USD exchange rate per year.")
sink()

summary2 <- data[, c(as.list(summary(Volume)), Total = sum(Volume)), by="Year"]

sink("figures_tables/tab_summary_BTCVolume.tex")
kable(summary2, format = "latex", booktabs = T, digits = 2,
      caption = "Summary statistics of the BTC/USD exchange rate per year.")
sink()

summary3 <- data[, c(as.list(summary(VolumeUSD)), Total = sum(VolumeUSD)), by="Year"]

sink("figures_tables/tab_summary_USDVolume.tex")
kable(summary3,format = "latex", booktabs = T, digits = 2,
      caption = "Summary statistics of the BTC/USD exchange rate per year.")
sink()



