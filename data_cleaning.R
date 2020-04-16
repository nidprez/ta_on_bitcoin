library(readr)
bitstampusd <- read_csv("Data/bitstamp/bitstampusd.csv", col_names = FALSE)
names(bitstampusd) = c("V1","V2","V3")
bitstampusd$V1 <- anytime::utctime(bitstampusd$V1)

test <- c(0, diff(log(bitstampusd$V2)))
ordertest <- order(test)
ordertime <- bitstampusd$V1[ordertest]
table(lubridate::semester(ordertime[1:200], with_year = T))
table(lubridate::semester(tail(ordertime,200), with_year = T))

orderret <- test[ordertest]


biggest <- tail(ordertest, 100)
smallest <- ordertest[1:100]

around <- function(x,y = 1){
  return(c((x-y):(x+y)))
}

around(20,2)

indices <- c(sapply(which(outlier == 1), around, y = 2))

biggest2 <- c(sapply(biggest, around, y = 5))
smallest2 <- c(sapply(biggest, around, y = 5))

highret <- bitstampusd[biggest2, ]
lowret <- bitstampusd[smallest2, ]

highret_old <- bitstampusd[biggest, ]
lowret_old <- bitstampusd[smallest, ]

highret <- bitstampusd[biggest, ]
lowret <- bitstampusd[smallest, ]

testdata <- tail(bitstampusd, 100)

data <- bitstampusd %>% 
  group_by(V1) %>% 
  summarise(Price = median(V2), Volume = sum(V3))

Year <- lubridate::year(data$V1)

k <- 40
k2 <- k/2
phi <- 0.5
n <- nrow(data)

outlier <- logical(n)

closest <- function(x, k, k2, n){
  if(x <= k2){
    c(1:(k+1))[-x]
  }else if((x + k2) > n){
    c((n-k):n)[-abs(n-x-1-k)]
  }else{
    c((x-k2):(x-1), (x+1):(x+k2))
  }
}

for (i in 1:n) {
  p <- data$Price[closest(i, k, k2, n)]
  outlier[i] <- ifelse(abs(data$Price[i] - mean(p)) < (3*(sd(p) + phi)), F, T)
  print(paste(round(i/n*100, 4), "% done"))
}
