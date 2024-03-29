library(MASS)
library(ggplot2)
data <- Cars93

#histograms
hist(data$MPG.city, col="blue",xlab = "MPG Ciy" ,main = "Histogram Plot for MPG City",breaks = 15)
hist(data$MPG.highway, col="red", xlab = "MPG Highway",main = "Histogram Plot for MPG Highway",breaks=15)
data$MPG.city
data
#density plots with basic R
d.highway <- density(data$MPG.city)
plot(d.highway, xlab = "MPG Highway",main = "Histogram Plot for MPG Highway",col="blue")
d.city <- density(data$MPG.highway)
plot(d.city,xlab = "MPG Ciy" ,main = "Density Plot for MPG City",col="red")

#density plots with ggplot2
ggplot(data, aes(x=MPG.highway)) + 
  geom_density()

ggplot(data, aes(x=MPG.city)) + 
  geom_density()

#qqplots
qqnorm(data$MPG.city, pch = 1, frame = FALSE,xlab = "MPG Ciy" )
qqline(data$MPG.city, col = "steelblue", lwd = 2)

qqnorm(data$MPG.highway, pch = 1, frame = FALSE,xlab = "MPG Highway")
qqline(data$MPG.highway, col = "steelblue", lwd = 2)

#boxplots
boxplot(data$MPG.city)
boxplot(data$MPG.highway)



#getting rid of outliers with subset function
without.outliers.data<- subset(data,MPG.highway<40 & MPG.highway<40)

#log transform
Cars93.log <- transform(without.outliers.data, MPG.highway.log=log(MPG.highway), MPG.city.log=log(MPG.city))

#histograms
hist(Cars93.log$MPG.city.log, col="blue",xlab = "MPG Ciy" ,main = "Histogram Plot for MPG City",breaks = 15)
hist(Cars93.log$MPG.highway.log, col="red", xlab = "MPG Highway",main = "Histogram Plot for MPG Highway",breaks=15)

#density plots with basic R
d.highway <- density(Cars93.log$MPG.city.log)
plot(d.highway, xlab = "MPG Highway",main = "Histogram Plot for MPG Highway",col="blue")
d.city <- density(Cars93.log$MPG.highway.log)
plot(d.city,xlab = "MPG Ciy" ,main = "Density Plot for MPG City",col="red")

#density plots with ggplot2
ggplot(Cars93.log, aes(x=MPG.highway.log)) + 
  geom_density()

ggplot(Cars93.log, aes(x=MPG.city.log)) + 
  geom_density()

#qqplots
qqnorm(Cars93.log$MPG.city.log, pch = 1, frame = FALSE)
qqline(Cars93.log$MPG.city.log, col = "steelblue", lwd = 2,xlab = "MPG Ciy" )

qqnorm(Cars93.log$MPG.highway.log, pch = 1, frame = FALSE)
qqline(Cars93.log$MPG.highway.log, col = "steelblue", lwd = 2,xlab="MPG Highway")

#boxplots
boxplot(Cars93.log$MPG.city.log)
boxplot(Cars93.log$MPG.highway.log)





############################3333333





library(MASS)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggpubr")
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













########################









#setwd("/Users/mADEMatik/Desktop/LAB/lab2")
###LOOK For the test assumptions for midterm.

###This two samples are dependent so we have to do paired t.test with parameter of paired = TRUE
before.video <- c(25,41,41,54,29,50,54,46,54,33,33,54,37,12,29,41)
after.video <- c(41,66,92,71,71,54,88,54,70,50,58,79,88,46,67,46)

difference <- after.video-before.video
mean(difference)

#Normality test for difference
#H0: Data is Normally Distributed
#H1: Data is not Normally Distributed    alpha=0.05

#If p ≤ 0.05: then the null hypothesis can be rejected 
#(i.e. the variable is NOT normally distributed).
#If p > 0.05: then the null hypothesis cannot be rejected 
#(i.e. the variable MAY BE normally distributed).

# So p is greater than 0.05 our data is normally distributed.


shapiro.test(difference)


#T test for difference 
t.test(difference,mu=0,alternative = "two.sided",conf.level = 0.95)
#T test for paired 
t.test(after.video,before.video, mu = 10 ,paired = TRUE, alternative = "greater",conf.level = 0.95)

#Conclusion : We are 95% confident that the video increases the students grades at least 10 points. 


####Let'assume they are independent

GradesA <- c(25,41,41,54,29,50,54,46,54,33,33,54,37,12,29,41)
GradesB <- c(41,66,92,71,71,54,88,54,70,50,58,79,88,46,67,46)


#Checking normality for each data


shapiro.test(GradesA)

shapiro.test(GradesB)

#Checking the variances are equal or not

var.test(GradesA,GradesB)


t.test(GradesA,GradesB,alternative = "two.sided",mu=0,paired = FALSE,var.equal = TRUE)

#Conclusion: So, we are 95% confident that these two classes have different mean scores.

####ANOVA

#We are searching for the drug effect of 4 painkillers for migrene. 
#We randomly choose 10 individuals for 4 groups.
#Group1 <- Drug A ..... Group4 <- Drug D
#After 30 minutes ,individuals tell their pain scores like this.
A <- c(4.4,5.1,4.3,3.3,2.1,4.7,3.3,4.5,2.1,2.1)
B <- c(6.9,8.1,4.2,5.5,4.3,6.6,5.4,8.2,6.3,6.3)
C <- c(6.7,7.1,6.4,6.6,7.2,5.4,6.0,5.0,5.2,5.2)
D <- c(6.6,4.1,4.4,5.6,8.2,6.4,6.0,5.0,5.2,5.2)

#Box plot for each drug.
boxplot(A,B,C,D)

#Checking the normality
shapiro.test(A)
shapiro.test(B)
shapiro.test(C)
shapiro.test(D)

#Checking the variances are equal

pain <- c(A,B,C,D)
drug <- as.factor(c(rep("A",10),rep("B",10),rep("C",10),rep("D",10)))
migrene <- data.frame(pain,drug)

bartlett.test(pain~drug,data = migrene)

#Anova Part

model <- aov(pain~drug,data = migrene)
summary(model)

#Reject H0 => At least one of the groups mean is different than the others.
#H0:meani= meanj
TukeyHSD(model)

#Conclusion: We are 95% confident that the mean Groups B-C-D are the same, but A is different than those.
plot(TukeyHSD(model, conf.level=.95), las = 2)
resting <- as.factor(c(rep("Yes",5),rep("No",5),rep("Yes",5),rep("No",5),rep("Yes",5),rep("No",5),rep("Yes",5),rep("No",5)))

migrene <- cbind(migrene,resting)

boxplot(pain~drug+resting,data = migrene)

model2way <-aov(pain~drug+resting,data = migrene,) 
summary(model2way)

TukeyHSD(model2way)











