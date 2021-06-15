library(dummies)
library(moments)
#view of the data
startup <- read.csv(file.choose())
View(startup)
startup<-dummy.data.frame(data=startup,sep='.')
attach(startup)

#Exploratory data analysis (EDA)
#To find  measure of central tendency(mean,median,max..)
summary(startup)

#measures of dispersion column wise
apply(startup,2,var)
apply(startup,2,sd)
apply(startup,2,range)

#skewness values
skewness(startup$R.D.Spend)
skewness(startup$Administration)
skewness(startup$Marketing.Spend)
skewness(startup$State)
skewness(startup$Profit)


#kurtosis
kurtosis(startup$R.D.Spend)
kurtosis(startup$Administration)
kurtosis(startup$Marketing.Spend)
kurtosis(startup$State)
kurtosis(startup$Profit)
#for all the attributes/columns-sharper tails and flatter peaks

#Probablity distrubutions of variables
pnorm(R.D.Spend,mean=mean(R.D.Spend),sd=sd(R.D.Spend))
pnorm(Adminstration,mean=mean(Adminstration),sd=sd(Adminstration))
pnorm(Marketing.Spend,mean=mean(Marketing.Spend),sd=sd(Marketing.Spend))
pnorm(Profit,mean=mean(Profit),sd=sd(Profit))

#graphical representations...
#dot plot
layout(matrix(1:4,nrow =2))
plot(Profit~R.D.Spend,data=startup)
plot(Profit~Administration,data=startup)
plot(Profit~Marketing.Spend,data=startup)

#boxplot representation 
layout(matrix(1:6,nrow=3))
x<-boxplot(Profit~R.D.Spend,data = startup,ylab="Profit",xlab="R.D.Spend")
x<-boxplot(Profit~Administration,data = startup,ylab="Profit",xlab="Adminstration")
x<-boxplot(Profit~Marketing.Spend,data = startup,ylab="Profit",xlab="Marketing.Spend")
# or for boxplot
apply(startup,2,boxplot)

#for histogram
apply(startup,2,hist)

# correlation b/n Output price and variables-Scatter plot
pairs(startup)

# Correlation Coefficient matrix - Strength & Direction of Correlation
cor(startup)

library(corpcor)
cor2pcor(cor(startup))

#linear model
model.startup <-lm(Profit~R.D.Spend+Administration+Marketing.Spend,data = startup)
summary(model.startup)

#logarithmic transformation
model.startupL <-lm(Profit~log(R.D.Spend+Administration+Marketing.Spend),data = startup)
summary(model.startupL)
#exponential transformation
model.startupE <-lm(log(Profit)~R.D.Spend+Administration+Marketing.Spend,data = startup)
summary(model.startupE)

#prediction model
predict(model.startup)

#covariance for model parameters
vcov(model.startup)

#regression diagonstics
influence(model.startup)

#plotting of the graph
plot(model.startup)