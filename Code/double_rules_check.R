
mean_ret <-colMeans(return)

num_eq <- rep(NA, length(mean_ret))

for(i in seq_along(mean_ret)){
  num_eq[i] <- sum(mean_ret == mean_ret[i])
}
