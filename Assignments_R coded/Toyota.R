library(dummies)
library(moments)
#view of the data
tc<- read.csv(file.choose())
View(tc)
tc<-tc[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

tc<-dummy.data.frame(data=tc,sep='.')
attach(tc)

#Exploratory data analysis (EDA)
#To find measure of central tendency(mean,median,max..)
summary(tc)

#measures of dispersion column wise
apply(tc,2,var)
apply(tc,2,sd)
apply(tc,2,range)

#skewness values
apply(tc,2,skewness)


#kurtosis
apply(tc,2,kurtosis)

#Probablity distrubutions of variables
pnorm(Age_08_04,mean=mean(Age_08_04),sd=sd(Age_08_04))
pnorm(KM,mean=mean(KM),sd=sd(KM))
pnorm(HP,mean=mean(HP),sd=sd(HP))
pnorm(cc,mean=mean(cc),sd=sd(cc))
pnorm(Doors,mean=mean(Doors),sd=sd(Doors))
pnorm(Gears,mean=mean(Gears),sd=sd(Gears))
pnorm(Quarterly_Tax,mean=mean(Quarterly_Tax),sd=sd(Quarterly_Tax))
pnorm(Weight,mean=mean(Weight),sd=sd(Weight))

#graphical representations...
#bar plot
layout(matrix(1:8,nrow =4))
plot(price~Age_08_04,data=tc)
plot(price~KM,data=tc)
plot(price~HP,data=tc)
plot(price~CC,data=tc)
plot(price~Doors,data=tc)
plot(price~Gears,data =tc)
plot(price~Quaterly_Tax,data=tc)
plot(price~Weight,data = tc)

#boxplot representation 
apply(tc,2,boxplot)

#for histogram
apply(tc,2,hist)

# correlation b/n Output price and variables-Scatter plot
pairs(tc)

# Correlation Coefficient matrix - Strength & Direction of Correlation
cor(tc)

#linear model
cor(com)
library(corpcor)
model.tc<-lm(Price~(Age_08_04+KM+HP+CC+Doors+Gears+Quaterly_Tax+Weight),data = tc)
summary(model.tc)
#R-squared value-0.863
#p-value is less than 0.05
#prediction on KM
model.tcK<-lm(Price~KM)
summary(model.tcK)
#prediction model on HP
model.tcH<-lm(Price~HP)
summary(model.tcH)
#PREDICTION MODEL ON CC
model.tcC<-lm(Price~cc)
summary(model.tcC)
#prediction model on gears
model.tcG<-lm(Price~Gears)
summary(model.tcG)
#prediction model on quaterly tax
model.tcQ<-lm(Price~Quarterly_Tax)
summary(model.tcQ)
#prediction model on weight
model.tcG<-lm(Price~Weight)
summary(model.tcG)
#prediction model
predict(model.tc)

#covariance for model parameters
vcov(model.tc)

#regression diagonstics
influence(model.tc)

#plotting of the graph
plot(model.tc)


  
  