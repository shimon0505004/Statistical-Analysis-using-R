#1A
DiffDiets<- read.csv(file.choose())
str(DiffDiets)
DiffDiets
levels(DiffDiets$Diet)
DiffDiets$Diet <- factor(DiffDiets$Diet, levels(DiffDiets$Diet)[c(2,1,3,4)])
levels(DiffDiets$Diet)
#1A
par(mfrow=c(1,1))
#Constructing the Boxplot
boxplot(Trig~Diet,data=DiffDiets,main="Boxplots")  
library(plyr)

#1B
SumStats<-ddply(DiffDiets,c("Diet"),summarise,
                n = length(Trig),
                mean = mean(Trig),
                sd = sd(Trig),
                se = sd/sqrt(n) )
SumStats
AovFit <- aov(Trig~Diet, data = DiffDiets)
AovFit
summary(AovFit)

#1C
par(mfrow=c(2,2))
plot(AovFit)
#Megaphone Shape Found, Equal Variance not happening.
shapiro.test(residuals(AovFit))
#Shapiro-Wilks Test shows that probability of Null hypothesis of normal distribution of residuals are less than 0.05(alpha)

#1D
library(car)
leveneTest(Trig~Diet, data = DiffDiets)
#p value < 0.05 , the assumption of equal variance does not hold. 


#1E
DiffDiets <- data.frame(DiffDiets, logTrig = log(DiffDiets$Trig))
DiffDiets <- data.frame(DiffDiets, SqrtTrig = sqrt(DiffDiets$Trig))
DiffDiets <- data.frame(DiffDiets, SqrTrig = (DiffDiets$Trig)*(DiffDiets$Trig))
DiffDiets <- data.frame(DiffDiets, MinusOnePowTrig = 1/(DiffDiets$Trig))

str(DiffDiets)
DiffDiets
par(mfrow=c(2,2))
Fit2 <- aov(logTrig~Diet, data=DiffDiets)
summary(Fit2)
#plot(residuals(AovFit)~fitted(AovFit),main="Original: Resids vs Pred");abline(h=0)
plot(residuals(Fit2)~fitted(Fit2),main="Log: Resids vs Pred");abline(h=0)

Fit3 <- aov(SqrtTrig~Diet, data=DiffDiets)
summary(Fit3)
plot(residuals(Fit3)~fitted(Fit3),main="SQRT: Resids vs Pred");abline(h=0)

Fit4 <- aov(SqrTrig~Diet, data=DiffDiets)
summary(Fit4)
plot(residuals(Fit4)~fitted(Fit4),main="SQR: Resids vs Pred");abline(h=0)

Fit5 <- aov(MinusOnePowTrig~Diet, data=DiffDiets)
summary(Fit5)
plot(residuals(Fit5)~fitted(Fit5),main="1/Y: Resids vs Pred");abline(h=0)

#From the graph, only log transformation does not look like a megaphone. 

#F
Fit2
summary(Fit2)
#G
pairwise.t.test(DiffDiets$logTrig,DiffDiets$Diet,data=DiffDiets, p.adj="none")
#H
library(multcomp)
PairComps <- glht(Fit2, linfct= mcp(Diet = "Tukey"))
PairComps
summary(PairComps)

#I
Fit2
summary(Fit2)
#Observations per treatment group = 10
#SW = sqrt(MSResid), so SW^2 = MSResid
HSD<- qtukey(1-0.05,4,36)*sqrt((0.766)/10)
HSD

#J
cld(PairComps)
model.tables(Fit2,type="means", se=T)
Navy_Mean <-5.156
Black_Mean <- 5.309
Ctrl_Mean <- 6.297
Soy_Mean <- 6.329
Navy_Range <- Navy_Mean+HSD
Navy_Range
Black_Range <- Black_Mean+HSD
Black_Range

#I
DunnetComparisons <- glht(Fit2, linfct = mcp(Diet = "Dunnett"))
DunnetComparisons
summary(DunnetComparisons)
confint(DunnetComparisons)

#J
contfit <-lm(logTrig~Diet-1,data=DiffDiets)
contfit
BvN <- c(0,1,-1,0)
BNvC <- c(-1,0.5,0.5,0)
BNvS <- c(0,0.5,0.5,-1)
BNvCS <- c(-0.5,0.5,0.5,-0.5)

Cmat <-t(cbind(BvN,BNvC,BNvS,BNvCS))
Cmat
colnames(Cmat) <- c("A","B","C","D")
Cmat
contrast_Results <- glht(contfit, linfct=Cmat)
contrast_Results #Gives The Estimate of the contrasts
summary(contrast_Results,test=adjusted(type="none"))
