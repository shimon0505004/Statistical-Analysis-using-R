#Q1
birds <- matrix(c(98,101,141,328),nrow = 2,byrow = TRUE)
rownames(birds) <- c("Bird","No Bird")
colnames(birds) <- c("Cancer Patients","Healthy Controls")
birds
library(epitools)
oddsratio(birds,method = "wald")

#Q2
BCG_data <- array(c(8,10,2537,619,
                    505,499,87886,87892,
                    29,45,7470,7232),
                  dim = c(2,2,3),
                  dimnames = list(Trt=c("Trt","Ctrl"),
                                  Response = c("TBpos","TBneg"),
                                  Study = c("1","2","3")))
BCG_data

#Response by study
Trt.by.study <- margin.table(BCG_data,c(1,3))
barplot(Trt.by.study,legend=T,main = "Treatment By Study")
resp.by.study <- margin.table(BCG_data,c(2,3))
barplot(resp.by.study,legend=T,main = "Response By Study")

library(lawstat)
cmh.test(BCG_data)


#B
library(metafor)
cmh <- rma.mh(ai = BCG_data[1,1,],bi= BCG_data[1,2,],ci= BCG_data[2,1,], di = BCG_data[2,2,])
cmh

#Breslow-Day test
cmh$BD
cmh$BDp

#c
mantelhaen.test(BCG_data,correct = FALSE)



#3
#Observed data

Obs <- c(109,65,22,3,1)
Y <- seq(from=0,to=4,by=1)
Y

#Calculate the mean
Muhat <- sum(Obs*Y)/sum(Obs)
Muhat

#Calculate the corresponding Poisson Probabilities
Prob <- dpois(Y,Muhat)
Prob

length(Prob)
sum(Prob)

#Fix the final entry so that the probabilities sum to 1
Prob[5] <- 1-ppois(3,Muhat)
Prob
sum(Prob)
Prob

#Calculate Expected values and Contributions to Chisquare TS
Exp <- Prob*200
X2 <- (Obs-Exp)^2/Exp
cbind(Y,Obs,Prob,Exp,X2)

#Run GOF test
ChisqTS <- sum(X2)
ChisqTS
pval <- 1-pchisq(ChisqTS,5-2)
pval

#4
poissonData <- read.csv("C:\\Users\\SHAIKHSHAWON\\Dropbox\\Fall 2015\\Stat 511\\Homework\\Homework10\\PoissonData.csv")
poissonData
str(poissonData)
mean(poissonData$Y)
s<-sd(poissonData$Y)
s
s^2
hist(poissonData$Y)
qqnorm(poissonData$Y)
qqline(poissonData$Y)
t.test(poissonData$Y)

y <- sum(poissonData$Y)
y

LowerCI <- y - (1.96*sqrt(y))
UpperCI <- y + (1.96*sqrt(y))
LowerCI/50
UpperCI/50
