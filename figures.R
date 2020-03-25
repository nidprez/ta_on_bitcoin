library(tikzDevice)
tikz("tradefreq.tex", width = 4, height = 2.25)
bitstamp_summary[-9,] %>% 
 ggplot(aes(x = Year, y = N)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Time", y = "Frequency (in millions)") + 
  theme_minimal() +
  scale_y_continuous(breaks = seq(1000000,11000000,2000000), labels = as.character(seq(1,11,2))) +
  scale_x_continuous(labels = as.character(2011:2018), breaks = c(2011:2018))

dev.off()