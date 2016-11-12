#1A
power.t.test(n=12,delta=2,sd=3,sig.level = 0.05,type = "one.sample", alternative = "one.sided")

#1B
testSD<-seq(3,20,1)
powerVal1B_DiffSD <- power.t.test(n=12,delta=2,sd=testSD,sig.level = 0.05,type = "one.sample", alternative = "one.sided")
plot(powerVal1B_DiffSD$power ~ testSD, type = "b", xlab = "Standard Deviation", ylab = "power", main = "Power VS SD for Intake of Zinc")

#1C
testSampleSize <- seq(12,30,1)
powerVal1C_Diffn <- power.t.test(n=testSampleSize,delta=2,sd=3,sig.level = 0.05,type = "one.sample", alternative = "one.sided")
plot(powerVal1C_Diffn$power ~ testSampleSize, type = "b", xlab = "Sample Size", ylab = "power", main = "Power VS Sample Size for Intake of Zinc")

#1D
power.t.test(n=12,delta=2,sd=3,sig.level = 0.10,type = "one.sample", alternative = "one.sided")

#1E
power.t.test(n=12,delta=1,sd=3,sig.level = 0.05,type = "one.sample", alternative = "one.sided")

#1F
power.t.test(delta=2,sd=3,p=0.9,sig.level = 0.05,type = "one.sample", alternative = "one.sided")


#QUESTION2
#A
DataHW4_2 <- read.csv(file.choose())
hist(DataHW4_2$X.Lead.,xlab = "Lead", main = "Histogram of Lead")
qqnorm(DataHW4_2$X.Lead.,xlab = "Lead")
qqline(DataHW4_2$X.Lead. )
shapiro.test(DataHW4_2$X.Lead.)

#B
mean(DataHW4_2$X.Lead.)
median(DataHW4_2$X.Lead.)

#C,D
hist(DataHW4_2$X.Lead.,xlab = "Lead", main = "Histogram of Lead")
summary(DataHW4_2$X.Lead.)
sort(DataHW4_2$X.Lead.)
library(BSDA)
SIGN.test(DataHW4_2$X.Lead., md=30)

#E,F
t.test(DataHW4_2$X.Lead. , mu=30)

#G
mean.fun <- function(d,i)
{
  m <- mean(d[i])
  n <- length(i)
  v <- (n-1)*var(d[i])/n^2
  c(m,v)
}  
set.seed(7255)
resultsHW4_2F <- boot(data=DataHW4_2$X.Lead.,mean.fun, R=1000)
boot.ci(resultsHW4_2F,type="all")

#3A
RocketPropelantData <- read.csv(file.choose())
RocketPropelantData
y1 <- mean(RocketPropelantData$X.Mixture1.)
s1 <- sd(RocketPropelantData$X.Mixture1.)
y2 <- mean(RocketPropelantData$X.Mixture2.)
s2 <- sd(RocketPropelantData$X.Mixture2.)
y1;s1;y2;s2
n1 <- length(RocketPropelantData$X.Mixture1.)
n2 <- length(RocketPropelantData$X.Mixture2.)
t.test(RocketPropelantData$X.Mixture1.,RocketPropelantData$X.Mixture2., var.equal = TRUE)
