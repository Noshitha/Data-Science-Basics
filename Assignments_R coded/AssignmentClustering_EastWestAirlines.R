EastWestAirlines <- read.csv(file.choose()) 
View(EastWestAirlines)

sub_EastWestAirlines <- EastWestAirlines[,2:12]
norm_airline <- scale(sub_EastWestAirlines)
# Hirerachical CLustering
distanct_airline <- dist(norm_airline,method="euclidean")
str(distanct_airline)
distanct_airline1 <- dist(norm_airline,method = "manhattan")
str(distanct_airline)
airline_clust <- hclust(distanct_airline, method = "complete")
#plot(airline_clust, hang = -1)
group_airline <- cutree(airline_clust,k=5)
EastWestAirlines_New <- cbind(EastWestAirlines, group_airline)
setnames(EastWestAirlines_New, 'group_airline', 'group_hclust')
aggregate(EastWestAirlines_New[,2:12],by= list(EastWestAirlines_New$group_hclust), FUN = mean)