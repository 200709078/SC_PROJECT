
library(MASS)
install.packages("dplyr")
library(dplyr)
install.packages("ggpubr")
library(ggpubr)


library(MASS)
library(ggplot2)
data <- Cars93

data <- birthwt

birthwt.mean <- mean(data$bwt)
birthwt.sd <- sd(data$bwt)
birthwt.se <- birthwt.sd/sqrt(length(data))



#histogram
hist(data$bwt, col="blue",xlab = "Birth Weight" ,main = "Birth Weight Data",breaks = 20)
#qqplot
qqnorm(data$bwt, pch = 1, frame = FALSE)
qqline(data$bwt, col = "steelblue", lwd = 2,xlab = "Birth Weight" )

shapiro.test(data$bwt)

#H0: Data is Normally Distributed
#H1: Data is not Normally Distributed    alpha=0.05

#If p ≤ 0.05: then the null hypothesis can be rejected 
#(i.e. the variable is NOT normally distributed).
#If p > 0.05: then the null hypothesis cannot be rejected 
#(i.e. the variable MAY BE normally distributed).

# So p is greater than 0.05 our data is normally distributed.



#t-test

t.test(data$bwt,mu=2000)


#Confidence interval 

t.score <-  qt(p=0.05, df=188,lower.tail=F)

error <- t.score * birthwt.se
lower.bound <- birthwt.mean  - error
upper.bound <- birthwt.mean  + error
print(c(lower.bound,upper.bound))

a <- subset(data, bwt < 2500)


#1 — Hypothesis for first Fisher’s Exact Test (Alternative Two Sided):
#Ho: The odds ratio is equal to 1
#Ha: The odds ratio is not equal to 1
#2 — Hypothesis for first Fisher’s Exact Test (Alternative Less):
#Ho: The odds ratio is equal to or greater than 1
#Ha: The odds ratio is less than 1
#3 — Hypothesis for first Fisher’s Exact Test (Alternative Greater):
#Ho: The odds ratio is equal to or less than 1
#Ha: The odds ratio is greater than 1

