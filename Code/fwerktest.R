###################################
# test for the FWERkControl function
###################################

library(StepwiseTest)
?FWERkControl
test1
as.vector(test1)
dim(Z_c)
FWERkControl(test1, (Z_c*sqrt(n))/omega, 0,0.1)
sum(is.na(Z_c))
omega
omega = omega+0.000000005
FWERkControl(test1, (Z_c*sqrt(n))/omega, 0,0.1)
FWERkControl(test1, ((Z_c*sqrt(n))/omega), 0,0.1)
FWERkControl(test1, f_mean_boot, 0,0.1)
dim(f_mean_boot)
FWERkControl(test1, ((Z_c*sqrt(n))), 0,0.1)
FWERkControl(test1, ((Z_c*sqrt(n))), 10,0.1)
res1 <- FWERkControl(test1, ((Z_c*sqrt(n))), 10,0.1)
View(res1)
res1[["Reject"]]
sum(res1$Reject)
f_bar[res1$Reject]
plot(f_bar)
points(f_bar[res1$Reject], colour = "red")
points(f_bar[res1$Reject], col = "red")
summary(f_bar)
summary(f_bar[res1$Reject])
class(c(T,F))
summary(f_bar[as.logical(res1$Reject)])
points(f_bar[as.logical(res1$Reject)], col = "red")
plot(f_bar)
points(which(f_bar == f_bar[as.logical(res1$Reject)]),f_bar[as.logical(res1$Reject)], col = "red")
which(f_bar == f_bar[as.logical(res1$Reject)])
points(which(res1$Reject == 1),f_bar[as.logical(res1$Reject)], col = "red")
summary(f_bar[-as.logical(res1$Reject)])

