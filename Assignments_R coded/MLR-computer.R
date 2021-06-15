library(dummies)
library(moments)
#exporting the database
com <- read.csv(file.choose())
View(com)
com<-dummy.data.frame(data=com,sep='.')
attach(com)

#Exploratory data analysis (EDA)
#To find measure of central tendency(mean,median,max..)
summary(com)

#measures of dispersion column wise
apply(com,2,var)
apply(com,2,sd)
apply(com,2,range)

#skewness values
skewness(com$X)
skewness(com$price)
skewness(com$speed)
skewness(com$ads)
skewness(com$trend)
skewness(com$hd)
skewness(com$ram)
skewness(com$screen)
skewness(com$cd)
skewness(com$multi)
skewness(com$premium)

#kurtosis
kurtosis(com$price)
kurtosis(com$speed)
kurtosis(com$ads)
kurtosis(com$trend)
kurtosis(com$hd)
kurtosis(com$ram)
kurtosis(com$screen)

#price,hd,ram,screen has heavy peaks
#speed,ads,trend have lighter peaks

#Probablity distrubutions of variables
pnorm(speed,mean=mean(speed),sd=sd(speed))
pnorm(hd,mean=mean(hd),sd=sd(hd))
pnorm(ram,mean=mean(ram),sd=sd(ram))
pnorm(screen,mean=mean(screen),sd=sd(screen))
pnorm(ads,mean=mean(ads),sd=sd(ads))
pnorm(trend,mean=mean(trend),sd=sd(trend))

#graphical representations...
#bar plot
layout(matrix(1:6,nrow =3))
plot(price~speed,data=com)
plot(price~hd,data=com)
plot(price~ram,data=com)
plot(price~screen,data=com)
plot(price~ads,data=com)
plot(price~trend,data =com)

#boxplot representation 
layout(matrix(1:6,nrow=3))
x<-boxplot(price~speed,data = com,ylab="price",xlab="speed")
x<-boxplot(price~hd,data = com,ylab="price",xlab="hd")
x<-boxplot(price~ram,data = com,ylab="price",xlab="ram")
x<-boxplot(price~screen,data =com,ylab="price",xlab="screen")
x<-boxplot(price~ads,data =com,ylab="price",xlab="ads")
x<-boxplot(price~trend,data =com,ylab="price",xlab="trend")
# or for boxplot
apply(com,2,boxplot)

#for histogram
apply(com,2,hist)

# correlation b/n Output price and variables-Scatter plot
pairs(com)

# Correlation Coefficient matrix - Strength & Direction of Correlation
cor(com)
library(corpcor)
cor2pcor(cor(com))
#linear model
model.com <-lm(price~speed+hd+ram+screen+ads+trend,data = com)
summary(model.com)
#Variance Inflation factor to check collinearity b/n variables 
vif(model.com)
#prediction based on speed
model.comS<-lm(price~speed)
summary(model.comS)
#prediction based on ADS
model.comA<-lm(price~ads)
summary(model.comA)
#prediction based on Trend
model.comT<-lm(price~trend)
summary(model.comT)
#prediction on ads,trend and speed
model.comSAT<-lm(price~speed+ads+trend)
summary(model.comSAT)

#covariance for model parameters
vcov(model.com)

#regression diagonstics
influence(model.com)

#plotting of the final model graph
plot(model.com)