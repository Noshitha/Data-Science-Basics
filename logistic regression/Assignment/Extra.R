fam <- read.csv(file.choose()) # Choose the claimants Data set
View(fam)
#make sure the given database is proper
sum(is.na(fam))#No NA's Present!
dim(fam) #Size of dataset :1097 - 7
colnames(fam)

#Prediction Model on Linear Regression

mod_lm <- lm(naffairs ~.,data=fam)
pred1 <- predict(mod_lm,fam)
pred1
plot(fam$kids,pred1)
plot(pred1)