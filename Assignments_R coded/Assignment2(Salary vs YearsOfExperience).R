#build a prediction model for Salary_hike
#step 1 import data set

sal <- read.csv(file.choose()) 
View(sal)

#step 2 analysing the data

#plots:-

plot(sal$YearsExperience,sal$Salary)
boxplot(sal)
hist(sal$YearsExperience)
hist(sal$Salary)
summary(sal)

# Correlation coefficient value for Salary Hike and Churn_out_Date

x<- sal$YearsExperience
y <- sal$Salary
cor(x,y)
# If |r| is greater than  0.85 then Co-relation is Strong(Correlation Co-efficient = -0.9117216). 
# This has a strong positive Correlation 

#variance and standard devation
var(sal$YearsExperience)
var(sal$Salary)
sd(sal$YearsExperience)
sd(sal$Salary)

#linear model:- without using any transformation

reg<-lm(x~y)
summary(reg)

# The multiple-R-Squared Value is  0.957 
# Adjusted R-Squared Value is  0.9554 
# The Probability Value pvalue is < 2.2e-16(too less)

confint(reg,level = 0.95) # confidence interval

# predict(reg,type="prediction")

#step3: traansformations
# we may have to do transformation of variables for better R-squared value
# Applying transformations

# Logarthmic transformation
reg_log<-lm(x~log(y))  
summary(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval="predict")
# Multiple R-squared value for the above model is 0.932
# Adjusted R-squared:  0.9295

# we may have to do different transformation for a better R-squared value
# Applying different transformations

# Exponential model

reg_exp<-lm(log(x)~y) # regression using Exponential model
summary(reg_exp)
confint(reg_exp,level=0.95)

# Multiple R-squared value - 0.8539
# Adjusted R SQuare Value - 0.8487
# Higher the R-sqaured value - Better chances of getting good model 
# we may have to do different transformation for a better R-squared value


# Quadratic model

quad_mod <- lm(x~y+I(y^2),data=sal)
summary(quad_mod)
confint(quad_mod,level=0.95)

#Adjusted R-Squared = 0.9544
#Multiple R -Squared Value = 0.9575


# Quadratic model
y_sq=y*y
qd_model <- lm(x~y+y_sq,data=sal)
summary(qd_model)
confint(quad_mod,level=0.95)
# Adjusted R-Squared = 0.9544
#Multiple R -Squared Value = 0.9575

