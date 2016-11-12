#Question1
bankSalary <- read.csv(file.choose())
str(bankSalary)
head(bankSalary)

large <- subset(bankSalary, Size=="Large")
small <- subset(bankSalary, Size=="Small")
head(large)
head(small)

#1A: Create a scatterplot
library(lattice)
xyplot(Wages ~ LOS , data = bankSalary , groups = Size, type = c("p","r"), auto.key = list(space="right"))

#1B: Regressions
FitLarge <- lm(Wages ~ LOS, data = large)
summary(FitLarge)

FitSmall <- lm(Wages ~ LOS, data = small)
summary(FitSmall)

confint(FitLarge, level = 0.95)
confint(FitSmall, level = 0.95)


anova(FitLarge)
anova(FitSmall)

newdata <- data.frame(LOS = 0.0)
predict(FitLarge, newdata, interval = "confidence", level = 0.95)
predict(FitSmall, newdata, interval = "confidence", level = 0.95)

ANOVAFitLarge <- lm(Wages ~ as.factor(LOS), data = large)
ANOVAFitSmall <- lm(Wages ~ as.factor(LOS), data = small)

#Lack of fit test for Large bank
anova(FitLarge,ANOVAFitLarge)
anova(FitSmall,ANOVAFitSmall)

#F LOS 96 months
NewLOS <- data.frame(LOS = 96.0)
predict(FitLarge, NewLOS, interval = "confidence", level = 0.95)
predict(FitSmall, NewLOS, interval = "confidence", level = 0.95)

#xyplot(Wages ~ LOS , data = bankSalary , groups = Size, type = c("p","r"), auto.key = list(space="right"))
plot(Wages ~ LOS, data =bankSalary)
identify(bankSalary$Wages ~ bankSalary$LOS , labels = bankSalary$Wages)
plot(Wages ~ LOS, data =bankSalary)
identify(bankSalary$Wages ~ bankSalary$LOS , labels = bankSalary$LOS)

#H residual and RStudent
bankSalary
large_Subdata <- data.frame(large, Resid = resid(FitLarge), student = stdres(FitLarge) , RStudent = rstudent(FitLarge))
large_Subdata[large_Subdata$LOS == 70,]
#Bonferoni Adjusted 2-sided p-value
2*35*(1-pt(4.242492,32))

#I
#Estimated Correlation
large
cor.test(large$Wages, large$LOS)

small
cor.test(small$Wages, small$LOS)

#J
var.test(large$LOS,small$LOS)
t.test(large$LOS,small$LOS, var.equal = TRUE)

#2
#i)
library(MASS)
steelData <- read.csv(file.choose())
head(steelData)
plot(Strength ~ Thick , data = steelData, main = "i)Plot: Strength vs Thick")
FitSteelData <- lm(Strength ~ Thick , data = steelData)
abline(coef(FitSteelData))
#ii)
plot(stdres(FitSteelData) ~ fitted(FitSteelData), main = "ii)Model1: Resid vs Pred")
abline(h=0)
#iii)
qqnorm(stdres(FitSteelData))
qqline(stdres(FitSteelData))

ANOVAFitSteelData <- lm(Strength ~ as.factor(Thick), data = steelData)
anova(FitSteelData)
anova(ANOVAFitSteelData)
anova(FitSteelData,ANOVAFitSteelData)

#C) Quadratic term adding
FitSteelData2 <- lm(Strength ~ Thick +I(Thick^2) , data = steelData)
plot(Strength ~ Thick , data = steelData, main = "C)Plot: Strength vs Thick with quadratic term")
FitSteelData2$coefficients
curve((1.452457e+01) + (4.317629e-02)*x + (-5.994113e-05)*x^2 , add = TRUE)
summary(FitSteelData2)

