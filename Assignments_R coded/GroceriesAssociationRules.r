#importing dataset
groceries<-read.csv(file.choose()) 
View(groceries)
#Market Basket Analysis
summary(groceries)
library(arules)
library(arulesViz)
itemFrequency(groceries[1:5])
itemFrequencyPlot(groceries, topN = 20)
#Defining rules for support=0.006 and confidence =0.2
groceryrules <- apriori(groceries, parameter = list(support =0.006, confidence = 0.25, minlen = 2))
groceryrules
inspect(head(sort(groceryrules, by = "lift")))
inspect(head(sort(groceryrules, by = "confidence")))
plot(groceryrules,method = "scatterplot")
plot(groceryrules,method = "graph")
plot(groceryrules,method = "matrix")
#Defining rules for support=0.02 and confidence =0.2
groceryrules <- apriori(groceries, parameter = list(support =0.02, confidence = 0.25, minlen = 2))
groceryrules
inspect(head(sort(groceryrules, by = "lift")))
inspect(head(sort(groceryrules, by = "confidence")))
plot(groceryrules,method = "scatterplot")
plot(groceryrules,method = "graph")
plot(groceryrules,method = "matrix")
#Defining rules for support=0.04 and confidence =0.2
groceryrules <- apriori(groceries, parameter = list(support =0.04, confidence = 0.25, minlen = 2))
groceryrules
inspect(head(sort(groceryrules, by = "lift")))
inspect(head(sort(groceryrules, by = "confidence")))
plot(groceryrules,method = "scatterplot")
plot(groceryrules,method = "graph")
plot(groceryrules,method = "matrix")