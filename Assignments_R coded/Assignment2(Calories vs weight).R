#STEP 1:IMPORTING THE DATA SET

cal<- read.csv(file.choose())
View(cal)

#STEP 2:ANALYSING THE DATA 

#standard deviation of weight gained = 333.6925
#variance of weight gained =111350.7
#standard deviation of calories = 752.1095
#variance of calories =565668.5

var(cal$Calories.Consumed)
sd(cal$Calories.Consumed) 
var(cal$Weight.gained..grams.)
sd(cal$Weight.gained..grams.)

#STEP 3 LINEAR MODEL:-

#Multiple R-squared:  0.8968,	Adjusted R-squared:  0.8882
#correlation coefficient= 0.946991
# If |r| is greater than  0.85 then Co-relation is Strong(Correlation Co-efficient = -0.9117216). 
# This has a strong positive Correlation 

x=cal$Weight.gained..grams.
y=cal$Calories.Consumed
cor(x,y) 
reg<-lm(x~y)
summary(reg)
plot(cal$Weight.gained..grams.,cal$Calories.Consumed)
boxplot(cal$Weight.gained..grams.~cal$Calories.Consumed ,data = cal,
        xlab="WEIGHT GAIN",ylab="CALORIES CONSUMED")
counts <- table(cal$Weight.gained..grams.)
barplot(counts, main="WEIGHT GAINED BAR PLOT", 
        xlab="WEIGHT ")

#STEP 3:APPLYING TRANSFORMATIONS  to check whether the predicted values are better

# LOGARITHMIC TRANSFORMATIONS:-

#Multiple R-squared:  0.8776,	Adjusted R-squared:  0.8674  

x=cal$Weight.gained..grams.
y=cal$Calories.Consumed
reg_exp<-lm(log(x)~y) # regression using Exponential model
summary(reg_exp)
confint(reg_exp,level=0.95)

x=cal$Weight.gained..grams.
y=cal$Calories.Consumed
hist(log(x))
plot(log(x),y)


#EXPONENTIAL TRANSFORMATION:-

#Multiple R-squared:  0.8077,	Adjusted R-squared:  0.7917 
#the R squared value can be a better one ,so we choose next transformation

reg_log<-lm(Weight.gained..grams.~log(Calories.Consumed))  
summary(reg_log)
confint(reg_log,level=0.95)
predict(reg_log)

x=cal$Weight.gained..grams.
y=cal$Calories.Consumed
hist(exp(x))
plot(exp(x),y)

# Quadratic transformation:-
#Multiple R-squared:  0.9078,	Adjusted R-squared:  0.8911 

temp=Weight.gained..grams.*Weight.gained..grams.
qd_model <- lm(Calories.Consumed~Weight.gained..grams.+temp,data=cal)
summary(qd_model)
#finally,we observe that quadratic transformation suits the best.

var(cal$Calories.Consumed)
sd(cal$Calories.Consumed) 
var(cal$Weight.gained..grams.)
sd(cal$Weight.gained..grams.)

x=cal$Weight.gained..grams.
y=cal$Calories.Consumed
temp=x*x
qx=x+temp
qy=y
var(qx)
var(qy)
sd(qx)
sd(qy)

#finally the linear model after application of quadratic transformation
#Multiple R-squared:  0.9078,	Adjusted R-squared:  0.8911 