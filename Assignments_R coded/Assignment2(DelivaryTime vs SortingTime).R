# Build a prediction model for Delivary time
#step 1 import data set

Deli <- read.csv(file.choose()) 
View(Deli)

#step 2 analysing the data

#plots:-

plot(Deli$Delivery.Time,Deli$Sorting.Time)
boxplot(Deli)
hist(Deli$Sorting.Time)
hist(Deli$Delivery.Time)
summary(Deli)

# Correlation coefficient value

st<- Deli$Sorting.Time
dt <- Deli$Delivery.Time
cor(st,dt)
# If |r| is near to 0.85 then Co-relation is moderately strong(Correlation Co-efficient = 0.8259973). 
# This has a positive Correlation 

#variance and standard devation
var(Deli$Delivery.Time)
sd(Deli$Delivery.Time)
var(Deli$Sorting.Time)
sd(Deli$Sorting.Time)

#linear model:- without using any transformation

reg<-lm(st~dt)
summary(reg)

# The multiple-R-Squared Value is 0.6823 which is smaller(In General)
# Adjusted R-Squared Value is 0.6655
# The Probability Value pvalue is 3.983e-06(Overall Probability Model is also much less than 0.05)

confint(reg,level = 0.95) # confidence interval

# predict(reg,type="prediction")

#step3: traansformations
# we may have to do transformation of variables for better R-squared value
# Applying transformations

# Logarthmic transformation
reg_log<-lm(dt~log(st))  
summary(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval="predict")
# Multiple R-squared value for the above model is 0.6954
# Adjusted R-squared: 0.6794  

# we may have to do different transformation for a better R-squared value
# Applying different transformations

# Exponential model

reg_exp<-lm(log(dt)~st) # regression using Exponential model
summary(reg_exp)
confint(reg_exp,level=0.95)

# Multiple R-squared value -: 0.7109
# Adjusted R SQuare Value -: 0.6957 
# Higher the R-sqaured value - Better chances of getting good model 
# for Delivery Time and Sorting Time


# Quadratic model

quad_mod <- lm(dt~st+I(st^2),data=Deli)
summary(quad_mod)
confint(quad_mod,level=0.95)

#Adjusted R-Squared = 0.6934
#Multiple R -Squared Value = 0.6594


# Quadratic model
st_sq=st*st
qd_model <- lm(dt~st+st_sq,data=Deli)
summary(qd_model)
confint(quad_mod,level=0.95)
# Adjusted R-Squared = 0.6934 
#Multiple R -Squared Value = 0.6594

#thus ,exponential fits the best!!


