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





