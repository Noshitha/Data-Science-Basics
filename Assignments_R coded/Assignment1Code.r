/*Question 6*/
x=Q6$`Candies count`
y=Q6$Probability
z=(x*y)
z1=sum(x*y)
print(z1/6)

/*Question 7*/
sum(Q7$Points)/32
sum(Q7$Score)/32
sum(Q7$Weight)/32

median(Q7$Points)
median(Q7$Score)
median(Q7$Weight)

x <- table(Q7$Points)
x
names(x)[which(x==max(x))]
y <- table(Q7$Score)
y
names(y)[which(y==max(y))]
z <- table(Q7$Weight)
z
names(z)[which(z==max(z))]

variance.result = var(Q7$Points) # calculate variance
print (variance.result)
variance.result = var(Q7$Score) # calculate variance
print (variance.result)
variance.result = var(Q7$Weight) # calculate variance
print (variance.result)

sd.result = sqrt(var(Q7$Points)) # calculate standard deviation
print (sd.result)
 sd.result = sqrt(var(Q7$Score)) # calculate standard deviation
 print (sd.result)
 sd.result = sqrt(var(Q7$Weight)) # calculate standard deviation
 print (sd.result)
 
 range(Q7$Points)
 range(Q7$Score)
 range(Q7$Weight)
 
 /*Q10*/
 demo(graphics)
 v=c(Q9$Speed,Q9$Distance) 
 hist(v,xlab="speed",col="distance",border="blue")
 hist(Q9$Speed)
 hist(Q9$Distance)
 hist(Q9$SP)
 hist(Q9$WT)
 
 library(moments)
 
 skewness(Q9$Speed)
 skewness(Q9$Distance)
 skewness(Q9$WT)
 skewness(Q9$SP)
 
 kurtosis(Q9$Speed)
 kurtosis(Q9$Distance)
 kurtosis(Q9$WT)
 kurtosis(Q9$SP)
 
 
 x=c(34,36,36,38,38,39,39,40,40,41,41,41,41,42,42,45,49,56)
 mean(x)
 median(x)
 variance.result = var(x) # calculate variance
 print (variance.result)

 MPG=cars_csv$MPG
 hist(MPG)