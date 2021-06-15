crime_data <- read.csv(file.choose())
View(crime_data)
ncol(crime_data)
crime_data_sub <- crime_data[,2:5]
# Normalized the data
norm_crime_data_sub <- scale(crime_data_sub)
norm_crime_data_sub
# calculating distance matrix
d <- dist(norm_crime_data_sub, method = "euclidean")
str(d)
d1 <- dist(norm_crime_data_sub,method = "manhattan")
str(d1)
#Hierarchial Clustering
crime_cluse <- hclust(d, method = "complete")
plot(crime_cluse, hang=-1)
rect.hclust(crime_cluse,plot(crime_cluse,hang=-1),k=4,border="red")
groups <- cutree(crime_cluse,k=4)
crime_data_final <- cbind(crime_data, groups)
aggregate(crime_data_final[,2:6], by=list(crime_data_final$groups), FUN = mean)
#we see the 2 group had highest no of assaults ,urbanpop and rape groups 
#clustering is been done!!
