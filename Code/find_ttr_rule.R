  my_split <- function(y){
    split(y, seq(nrow(y)), drop = T)
  }
  
  # MA Rules ####
  p <- c(1,2,5,10,15,20,25,50,100,150,200) #lags in the short MA
  q <- c(2,5,10,15,20,25,50,100,150,200,250) #lags in the long MA
  x <- c(0,0.05,0.1,0.5,1,5) #percentage of the filter
  d <- c(0,2,3,4,5) #number of days that the MA should cross eachother
  k <- c(0, 5,10,25) #duration of holding the position (0 being as long as possible)
  
  MA_rules <- expand.grid(rule = "MA", p = p, q=q, x=x, d=d, k=k, KEEP.OUT.ATTRS = F, stringsAsFactors = F)
  MA_rules <- MA_rules[!(MA_rules$p >= MA_rules$q),] #delete all instances where the short MA is equal or longer than the long MA
  rownames(MA_rules) <- NULL
  
  MA_rules <- my_split(MA_rules)

  # Triple MA Rules ####
  
  # specs of the Moving Average Rules
  p <- c(2,5,10,15,20,25,50,100,150,200) #lags in the short MA
  q <- c(2,5,10,15,20,25,50,100,150,200,250) #lags in the long MA
  n <- c(1,2,5,10,15,20,25,50,100,150)
  x <- c(0,0.05,0.1,0.5,1,5) #percentage of the filter
  d <- c(0,2,3,4,5) #number of days that the MA should cross eachother
  
  triple_MA_rules <- expand.grid(rule = "Triple MA",n=n, p = p, q=q, x=x, d=d, KEEP.OUT.ATTRS = F, stringsAsFactors = F)
  triple_MA_rules <- triple_MA_rules[!(triple_MA_rules$p >= triple_MA_rules$q),] #delete all instances where the short MA is equal or longer than the long MA
  triple_MA_rules <- triple_MA_rules[!(triple_MA_rules$n >= triple_MA_rules$p),] #delete all instances where the short MA is equal or longer than the long MA
  triple_MA_rules <- triple_MA_rules[!(triple_MA_rules$n == 1),] #delete all instances where the short MA is equal or longer than the long MA
  rownames(triple_MA_rules) <- NULL
  
  triple_MA_rules <- my_split(triple_MA_rules)
  
  # RSI Rules ####
  
  h <- c(5,10,15,20,25,50,100,150,200,250)
  v <- c(10,15,20,25)
  d <- c(1,2,5)
  k <- c(0, 1,5,10,25)
  
  
  RSI_rules <- expand.grid(rule = "RSI", h = h, v = v, d = d, k = k, KEEP.OUT.ATTRS = F, stringsAsFactors = F)
  rownames(RSI_rules) <- NULL
  
  RSI_rules <- my_split(RSI_rules)
  
  # SR Rules ####
  x <- c(0.05,0.1,0.5,1,2.5,5,10)
  d <- c(0:5)
  j <- c(2,5,10,15,20,25,50,100,250)
  k <- c(0,1,5,10,25)
  
  SR_rules <- expand.grid(rule = "S\\&R", x = x, d = d, j = j, k = k, KEEP.OUT.ATTRS = F, stringsAsFactors = F)
  rownames(SR_rules) <- NULL
  
  SR_rules <- my_split(SR_rules)
  
  # Filter Rules ####
  x <- c(0.005,0.01,0.5,1,5,10,20)
  y <- c(0.005,0.01,0.5,1,5,10,20)
  d <- c(0:5)
  dx <- 0:5
  dy <- 0:4
  j <- c(1,2,5,10,20)
  k <- c(0, 5,10,15,20,25)
  
  filter_rules2 <- expand.grid(rule = "Filter", x = x, d = d, j = j, k = k, KEEP.OUT.ATTRS = F, stringsAsFactors = F)
  rownames(filter_rules2) <- NULL
  
  filter_rules2 <- my_split(filter_rules2)
  
  # Filter2 Rules ####
  filter_rules <- expand.grid(rule = "Filter 2", x = x, dx = dx, y = y, dy =dy, j = j, KEEP.OUT.ATTRS = F, stringsAsFactors = F)
  filter_rules <- filter_rules[filter_rules$y < filter_rules$x, ]
  filter_rules <- filter_rules[filter_rules$dy < filter_rules$dx, ]
  rownames(filter_rules) <- NULL
  
  filter_rules <- my_split(filter_rules)
  
  # CB Rules ####
  x <- c(0.05,0.1,0.5,1,5)
  d <- c(0:2)
  j <- c(5,10,15,20,25,50,100,200)
  c <- c(0.1,0.5,1,5,10)
  k <- c(0,1,5,10,25)
  
  CB_rules <- expand.grid(rule = "CB",x = x, d = d, j = j, k = k, c = c, KEEP.OUT.ATTRS = F, stringsAsFactors = F)
  rownames(CB_rules) <- NULL
  
  CB_rules <- my_split(CB_rules)
  
  # Rule List ####
  Rules <- c(MA_rules, triple_MA_rules, RSI_rules, SR_rules, filter_rules2, filter_rules, CB_rules)
  
  rm(MA_rules, triple_MA_rules, RSI_rules, SR_rules, filter_rules2, filter_rules, CB_rules)
