qt(1-0.025,99)
#2
HomeRadonDetectData <- c(91.9,97.8,111.4,107.5,105.4,95.0,103.8,99.6,96.8,115.3,104.8,101.7)
mean(HomeRadonDetectData)
sd(HomeRadonDetectData)
qqnorm(HomeRadonDetectData)
qqline(HomeRadonDetectData)
shapiro.test(HomeRadonDetectData)
t.test(HomeRadonDetectData,mu=105)

#3
NicotineContentData <-read.csv("")

sampleSize <-300
sqrt(sampleSize)
stand_devVal <-3.8
y_bar <- 14.6
miu <- 14

testStat<- (y_bar-miu)/(stand_devVal/sqrt(sampleSize))
testStat

qt(1-0.01,299)

#2C

#4A
n <- seq(5,15,1)
sd <- 2.9
alpha <- 0.05
ME <- qt(1 - alpha/2 , n-1)*sd/sqrt(n)
ME
out <- data.frame(n,ME)
out

#4B
#4A
n <- seq(5,50,1)
sd <- 2.9
alpha <- 0.05
ME <- qt(1 - alpha/2 , n-1)*sd/sqrt(n)
ME
out <- data.frame(n,ME)
out
