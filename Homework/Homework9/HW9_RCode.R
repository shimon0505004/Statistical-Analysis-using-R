#HW9

#Question 1
z_alpha_by2 <- qnorm(1-(0.05/2))
z_alpha_by2
conjectured_pie_hat <- 0.20
E <- 0.05
sample_size_n <- (z_alpha_by2*z_alpha_by2)*(conjectured_pie_hat*(1-conjectured_pie_hat))/(E*E)
sample_size_n

#Maximum sample size
conjectured_pie_hat <- 0.50
sample_size_n <- (z_alpha_by2*z_alpha_by2)*(conjectured_pie_hat*(1-conjectured_pie_hat))/(E*E)
sample_size_n

boys_superman <- 400*40/100
boys_superman
girls_superman <- 300*30/100
girls_superman

#Question 2
boys_girls <- matrix(c(160,240,90,210), nrow = 2, byrow = TRUE)
colnames(boys_girls) <- c("superman yes","superman no")
rownames(boys_girls) <- c("boys", "girls")
boys_girls
prop.table(boys_girls,1)

prop.test(c(160,90),c(400,300), alternative = "two.sided", conf.level = 0.90, correct = FALSE)

#Question 3

chisq.test(c(328,372,471,329),p = c(1/4,1/4,1/4,1/4), correct = FALSE)

Counts <- c(328,372,471,329)
probs <- c(1/4,1/4,1/4,1/4)

total <- sum(Counts)
total
Exp <- probs*total
Exp
Resid <- Counts - Exp
SEResid <- sqrt(total*probs*(1-probs))
PearsonResids <- Resid/SEResid
PearsonResids

critval <- qchisq(0.95,df=3)
critval

#Question 4

schizophrenia_data <- matrix(c(2,21,8,16),byrow = TRUE, nrow =2)
colnames(schizophrenia_data) <- c("Relapse","No Relapse")
rownames(schizophrenia_data) <- c("Personal Therapy","Family Therapy")
schizophrenia_data
chisq.test(schizophrenia_data,correct = FALSE)
fisher.test(schizophrenia_data)
