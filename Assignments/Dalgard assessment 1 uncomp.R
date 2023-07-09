#
##
### Dalgard Introductory Stats In R
####
##### Thursday 6 July,2023

# Solomon Ademoyegun

library(ISwR)
plot(rnorm(1000))
weight <- c(60, 72, 57, 90, 95, 72)
weight
height <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
bmi <- weight/height^2
bmi
sum(weight)
sum(weight)/length(weight)
xbar <- sum(weight)/length(weight)
weight - xbar
(weight - xbar)^2
sum((weight - xbar)^2)
sqrt(sum((weight - xbar)^2)/(length(weight) - 1))
mean(weight)
74.33333
sd(weight)
t.test(bmi, mu=22.5)
plot(height,weight)
plot(height, weight, pch=2)
plot(height, weight, pch=4, col = "green")
hh <- c(1.65, 1.70, 1.75, 1.80, 1.85, 1.90)
lines(hh, 22.5 * hh^2)