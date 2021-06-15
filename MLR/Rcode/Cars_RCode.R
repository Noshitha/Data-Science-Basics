
data("mtcars")
View(mtcars)
Cars <- read.csv(file.choose()) # choose the Cars.csv data set
View(Cars)
attach(Cars)
# Exploratory Data Analysis(60% of time)
# 1. Measures of Central Tendency
# 2. Measures of Dispersion
# 3. Third Moment Business decision
# 4. Fourth Moment Business decision
# 5. Probability distributions of variables
# 6. Graphical representations
  #  > Histogram,Box plot,Dot plot,Stem & Leaf plot, 
  #     Bar plot

summary(Cars)


# 7. Find the correlation b/n Output (MPG) & (HP,VOL,SP)-Scatter plot
pairs(Cars)

# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
cor(Cars)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Cars))

# The Linear Model of interest
model.car <- lm(MPG~VOL+HP+SP+WT,data=Cars)

summary(model.car)

# Prediction based on only Volume 
model.carV<-lm(MPG~VOL)
summary(model.carV) # Volume became significant

# Prediction based on only Weight
model.carW<-lm(MPG~WT)
summary(model.carW) # Weight became significant

# Prediction based on Volume and Weight
model.carVW<-lm(MPG~VOL+WT)
summary(model.carVW) # Both became Insignificant

# So there exists a collinearity problem b/n volume and weight
### Scatter plot matrix along with Correlation Coefficients
panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
{
  usr<- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<- format(c(r,0.123456789),digits=digits)[1]
  txt<- paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex<-0.4/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
}
pairs(Cars,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")

# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(model.car)
library(car)
## plotting Influential measures 
influenceIndexPlot(model.car,id.n=3) # index plots for infuence measures
influencePlot(model.car,id.n=3) # A user friendly representation of the above

# Regression after deleting the 77th observation, which is influential observation
model.car1<-lm(MPG~VOL+SP+HP+WT,data=Cars[-77,])
summary(model.car1)

# Regression after deleting the 77th & 71st Observations
model.car2<-lm(MPG~VOL+SP+HP+WT,data=Cars[-c(71,77),])
summary(model.car2)

## Variance Inflation factor to check collinearity b/n variables 
vif(model.car)
## vif>10 then there exists collinearity among all the variables 

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.car,id.n=2,id.cex=0.7)

## VIF and AV plot has given us an indication to delete "wt" variable

## Final model
finalmodel<-lm(MPG~VOL+SP+HP)
summary(finalmodel)

# Evaluate model LINE assumptions 
plot(finalmodel)
#Residual plots,QQplot,std-Residuals Vs Fitted,Cook's Distance 
qqPlot(model.car,id.n = 5)
# QQ plot of studentized residuals helps in identifying outlier 

