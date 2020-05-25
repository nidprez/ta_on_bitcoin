library(tidyverse)
library(data.table)
library(tikzDevice)
library(kableExtra)


source("Code/na_locf_bitcoincharts.R")
data <- na.locf_bitcoincharts(fread("Data/bitstamp/day/bitstampusd_day.csv"), LogRet = F, Year = T)
data <- data[Year != 2020 & Year != 2011]



figure_width <- 2.25 #in Inches lates point is (1/72.27) Inch
height_ratio <- 0.75
theme_set(theme_classic() + theme(panel.grid.major = element_line(colour = "grey92"), panel.grid.minor.x = element_line(colour = "grey94")))

my_tikz <- function(savename, figure_width, height_ratio){
  figure_height <- height_ratio*figure_width
  tikz(paste0(savename, "_width=",figure_width, "inch_ratio=", height_ratio, ".tex"), width = figure_width, height = figure_height)
}


price_plot <- data %>% 
  ggplot(aes(x = Time, y = Price)) + 
  geom_line() + 
  scale_x_datetime(date_breaks = "2 years", date_minor_breaks = "1 year", date_labels = "%Y")

my_tikz("figures_tables/fig_BTCprice", figure_width, height_ratio)
price_plot + labs(y = "BTC-USD") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

my_tikz("figures_tables/fig_BTClogprice", figure_width, height_ratio)
price_plot + scale_y_log10() + labs(y = NULL) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()