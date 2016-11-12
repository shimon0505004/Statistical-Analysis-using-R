
#Ans 1
rats<-read.csv(file.choose())
rats
Diff_After_Before <-rats$X.After.-rats$X.Before.
Diff_After_Before
mean(Diff_After_Before)
sd(Diff_After_Before)


#A
#Are the difference normally distributed?
hist(Diff_After_Before)
qqnorm(Diff_After_Before)
qqline(Diff_After_Before)
#NO, most data points are deviated from the straight line in the QQPlot

#B
t.test(Diff_After_Before,mu=0,alternative = "greater")
#No, p value less than alpha

#C Two sided CI
t.test(Diff_After_Before,mu=0,alternative = "two.sided")


#D. Wilcoxon Paired test
library(coin)
wilcoxsign_test(X.After. ~ X.Before. , data = rats, distribution="exact", alternative = "greater")
#P value less than alpha 


sigma0 <- 10
df = 99
s = 11.35
Chi_Square <- df*s^2/sigma0^2
qchisq(0.975,df=99,lower.tail = FALSE)
qchisq(0.025,df=99,lower.tail = FALSE)

Lower <- sqrt((df*s^2)/(qchisq(0.025,df=99,lower.tail = FALSE)))
Upper <- sqrt((df*s^2)/(qchisq(0.975,df=99, lower.tail = FALSE)))
Lower
Upper
Chi_Square
qchisq(0.05,df=99,lower.tail = FALSE)

result = 11.35*sqrt(99)/sqrt(qchisq(0.05,df=99,lower.tail = FALSE))
result
qchisq(0.05,df=99,lower.tail = FALSE)
