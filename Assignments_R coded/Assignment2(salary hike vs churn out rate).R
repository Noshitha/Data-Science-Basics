# Build a prediction model for Churn_out_rate 
#step 1 import data set

sh.cr <- read.csv(file.choose()) 
View(sh.cr)

#step 2 analysing the data

#plots:-

plot(sh.cr$Salary_hike,sh.cr$Churn_out_rate)
boxplot(sh.cr)
hist(sh.cr$Salary_hike)
hist(sh.cr$Churn_out_rate)
summary(sh.cr)

# Correlation coefficient value for Salary Hike and Churn_out_Date

cr<- sh.cr$Churn_out_rate
sh <- sh.cr$Salary_hike
cor(cr,sh)
# If |r| is greater than  0.85 then Co-relation is Strong(Correlation Co-efficient = -0.9117216). 
# This has a strong negative Correlation 

#variance and standard devation
var(sh.cr$Salary_hike)
var(sh.cr$Churn_out_rate)
sd(sh.cr$Salary_hike)
sd(sh.cr$Churn_out_rate)

#linear model:- without using any transformation

reg<-lm(cr~sh)
summary(reg)

# The multiple-R-Squared Value is 0.8312 which is greater than 0.8(In General)
# Adjusted R-Squared Value is 0.8101 
# The Probability Value pvalue is 0.0002386(Overall Probability Model is also less than 0.05)

confint(reg,level = 0.95) # confidence interval

# predict(reg,type="prediction")
# Adjusted R-squared value for the above model is 0.8101 

#step3: traansformations
# we may have to do transformation of variables for better R-squared value
# Applying transformations

# Logarthmic transformation
reg_log<-lm(cr~log(sh))  
summary(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval="predict")
# Multiple R-squared value for the above model is 0.8486
# Adjusted R-squared:  0.8297 

# we may have to do different transformation for a better R-squared value
# Applying different transformations

# Exponential model

reg_exp<-lm(log(cr)~sh) # regression using Exponential model
summary(reg_exp)
confint(reg_exp,level=0.95)

# Multiple R-squared value - 0.8735
# Adjusted R SQuare Value - 0.8577 
# Higher the R-sqaured value - Better chances of getting good model 
# for Delivery Time and Sorting Time


# Quadratic model

quad_mod <- lm(cr~sh+I(sh^2),data=sh.cr)
summary(quad_mod)
confint(quad_mod,level=0.95)

#Adjusted R-Squared = 0.9662
#Multiple R -Squared Value = 0.9737


# Quadratic model
sh_sq=sh*sh
qd_model <- lm(cr~sh+sh_sq,data=sh.cr)
summary(qd_model)
confint(quad_mod,level=0.95)
# Adjusted R-Squared = 0.9662 
#Multiple R -Squared Value = 0.9737


