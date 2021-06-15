fam <- read.csv(file.choose()) # Choose the affaispy Data set
View(fam)
#getting general info from the data
#make sure the given database is proper
sum(is.na(fam))#No NA's Present!
dim(fam) #Size of dataset :601 - 10
colnames(fam)
fam <- fam[,-1]# eliminating the column X which is an index

#EDA 
#1st business moment
summary(fam)
#2nd business moment
apply(fam, 2,sd)
apply(fam, 2, var)
apply(fam,2,range)
#3rd business moment
library(dummies)
library(moments)
apply(fam,2,skewness)
tapply(fam,2,kurtosis)
kurtosis(fam$affairs)#7.2
kurtosis(fam$yearsmarried)#1.432
kurtosis(fam$age)#3.22
kurtosis(fam$rating)#2.787923
kurtosis(fam$religiousness)#1,999
kurtosis(fam$education)#2.690
kurtosis(fam$occupation)#2.22077

#age and affairs have higher peak
#structure of the model
str(fam)
#column names in the model
colnames(fam)
#dimensions of the dataset
dim(fam) #601 - 9
#ggplot

ggplot(fam, aes(x ="",y=age, fill=y))+ 
  geom_boxplot()+labs(x="age",y="")
#THE GLM
attach(fam)

Lmodel <- glm(affairs ~ age +gender+ yearsmarried + religiousness 
               + education + occupation + rating, data= fam)
summary(Lmodel)
Lmodel
#Null Deviance:	    6529 
#Residual Deviance: 5671
View(fam)
#PLOTS
plot(fam)
attach(fam)

Lmodel <- glm(affairs~ age + gender+ yearsmarried + religiousness
              + education +occupation +rating,data=fam ,
              family = "binomial")
summary(Lmodel)
Lmodel

#Finding the odds
TrainModel <- predict(Lmodel ,type='response')
plot(TrainModel)
exp(coef(Lmodel))

#confusion matrix table
prob_y <- as.data.frame(predict(Lmodel, type = "response", fam))
final_y <- cbind(fam, prob_y)
prob_y
final_y
#confusion matrix table with threshold 
confusion_y <- table(prob_y>0.5, fam$affairs)
confusion_y
#accuracy of the model
accuracy <- sum(diag(confusion_y)/sum(confusion_y))
accuracy

# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob_y>=0.5,1,0)
yes_no <- ifelse(prob_y>=0.5,"yes","no")
pred_values
yes_no
# Creating new column to store the above values
fam[,"prob"] <- prob_y
fam[,"pred_values"] <- pred_values
fam[,"yes_no"] <- yes_no
View(fam)
View(fam[,c(1,10:12)])


install.packages("caret",dependencies = TRUE)
library(caret)
View(fam)

confusionMatrix(table(fam$affairs,
                      fam$pred_values))
#or
table(fam$affairs,fam$pred_values)

#ROCR CURVE
library(ROCR)
#1)CALCULATIONG THE VALUES OF ROCR
rocrpred<-prediction(prob,fam$affairs)
rocrperf<-performance(rocrpred,'tpr','fpr')
rocrperf
rocrpred
# changing params for the ROC plot - width, etc
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
#STRUCTURE OF ROCR
str(rocrperf)
#PLOT
plot(rocrperf)
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)







