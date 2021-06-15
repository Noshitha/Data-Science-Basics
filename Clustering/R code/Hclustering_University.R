Uni <- read.csv(file.choose())
View(Uni)
# Normalizing continuous columns to bring them under same scale
plot(Uni$SAT)
normalized_data<-scale(Uni[,2:7]) #excluding the university name columnbefore normalizing
normalized_data
#distance matrix
d <- dist(normalized_data, method = "euclidean") 
d2 <- dist(normalized_data , method = "manhattan")
d
d2
#Hierarchical Clusturing
fit <- hclust(d, method="complete")
fit2 <- hclust(d,method = "single")
fit
?hclust
plot(fit2)
plot(fit)# display dendrogram
plot(fit, hang=-1)


?cutree
rect.hclust(fit, k=3, border="red")
?rect.hclust
groups <- cutree(fit, k=3) # cut tree into 5 clusters

membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(Uni, membership)

View(final)

write.csv(final, file="final.csv",row.names = F)

aggregate(Uni[,-1],by=list(final$membership),mean)
