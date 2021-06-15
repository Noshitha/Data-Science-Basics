library(moments)
library(dummies)
bank <- read.csv(file.choose()) # Choose the  Data set
View(bank)
#make sure the given database is proper
sum(is.na(bank)) #no na values
dim(bank) #Size of dataset :45211    32
colnames(bank)
#EDA 
#1st business moment
summary(bank)
#2nd business moment
apply(bank, 2,sd)
apply(bank, 2, var)
apply(bank,2,range)
#3rd and 4rth business moment
apply(bank,2,skewness)
apply(bank,2,kurtosis)
#structure of the model
str(bank)
#column names in the model
colnames(bank)
#dimensions of the dataset
dim(bank)

#gg plot
ggplot(bank, aes(x ="",y=age, fill=y))+ 
  geom_boxplot()+labs(x="age",y="")
ggplot(bank, aes(x =duration, fill=y))+ geom_histogram(bins = 30)
ggplot(bank, aes(x =age, fill=y))+ geom_histogram(bins = 30)
ggplot(bank,aes(x =day, fill=y))+ geom_histogram(bins = 30)
#To find the null devience and residual deviance in logistic regression
y_model <-glm(y~.,data = bank)
summary(y_model)
attach(bank)
y_model<-glm(y~.,data = bank_data,family = "binomial")
exp(coef(y_model))#Null deviance: 4670.3  Residual deviance: 3394.0 

#confusion matrix table
prob_y <- as.data.frame(predict(y_model, type = "response",bank))
final_y <- cbind(bank, prob_y)
prob_y
final_y

#confusion matrix considering the threshold value
confusion_y <- table(prob_y>0.5, bank$y)
confusion_y

#accuracy of the model
accuracy <- sum(diag(confusion_y)/sum(confusion_y))
accuracy
#0.8980 accuracy

# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob_y>=0.5,1,0)
yes_no <- ifelse(prob_y>=0.5,"yes","no")

# Creating new column to store the above values
bank[,"prob"] <- prob_y
bank[,"pred_values"] <- pred_values
bank[,"yes_no"] <- yes_no
View(bank)
View(bank[,c(32,33:35)])#view as per predicted values without the given attributes or columns
table(bank$y,bank$pred_values)

#for the case of ROC curve in logistic regression
# More area under the ROC Curve better is the logistic regression model obtained
library(ROCR)
rocrpred<-prediction(prob_y,bank$y)
rocrpred
rocrperf<-performance(rocrpred,'tpr','fpr')
rocrperf
str(rocrperf)
plot(rocrperf)

# Getting cut off or threshold value along with true positive and false positive rates in a data frame 
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

# Sorting data frame with respect to tpr(true positive rate) 
library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)


