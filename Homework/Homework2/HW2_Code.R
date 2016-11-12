#1a
pnorm(0.57)
#1b
pnorm(-0.32)
#1c
1-pnorm(2.10)
#1d
pnorm(1.55)-pnorm(-0.32)
#1e
qnorm(0.3300)
#1f
qnorm(1-0.3987)

#2.A
pnorm(7,mean = 6, sd = 0.8)
#2.B
pnorm(5.4 ,mean = 6, sd = 0.8 , lower.tail = FALSE )
#2.C
pnorm(7.2,mean = 6, sd = 0.8) - pnorm(6,mean = 6, sd = 0.8)
#2.D
qnorm(0.85)
qnorm(0.85, mean = 6, sd = 0.8)

#3.A P(T>1.708)
pt(1.708,25,lower.tail = FALSE)

#3.B P(-t<T<t) = 0.95
#confidence interval = 0.95
#alpha = 0.05
qt(1-(0.05/2),9)

#3.C P(T>t) = 0.90
qt(0.9,50,lower.tail = FALSE)



#5
seeds <- read.csv("C:\\Users\\SHAIKHSHAWON\\Dropbox\\Fall 2015\\Stat 511\\Homework\\Homework2\\SeedWeights.csv")
str(seeds)
summary(seeds)
View(seeds)
library(lattice)
histogram(~Weight,data=seeds)
sampleMean = mean(seeds$Weight)
s <- sd(seeds$Weight)
n <- dim(seeds)[1]
qt(0.975,n-1)
error <- qt(0.975,n-1) *s/sqrt(n)
error
left <- sampleMean - error
right <- sampleMean + error
left
right