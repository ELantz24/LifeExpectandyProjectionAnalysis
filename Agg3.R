rm(list=ls())

library(cluster)

data <- read.csv("life expectancy.csv")

data <- data %>% 
  na.omit()

data$Region=as.factor(data$Region)
data$IncomeGroup=as.factor(data$IncomeGroup)


data <- data %>%
  filter(Year == 2013)

data <- data %>% select (-c("Year", "Country.Code")) 
head(data)


d=daisy(data[,2:14], metric="gower")
d


#data2 <- scale(data[,4:14])


##can alternatively use "manhattan" or "binary" (for Jaccard's)
#data3 <- dist(data2, method = "manhattan")

aResult <- agnes(d, method = "ward")

aResult

labels=data$Country.Name
plot(aResult, labels=labels)
rect.hclust(aResult, k = 5, border = "red")

aClusters <- cutree(aResult, k = 5)

myData<- data.frame(data, aClusters)


cluster3<-subset(myData, aClusters == 4)
cluster3$Country.Name


summary(subset(myData, aClusters == 1))
summary(subset(myData, aClusters == 2))
summary(subset(myData, aClusters == 3))
summary(subset(myData, aClusters == 4))
summary(subset(myData, aClusters == 5))


summary(as.factor(aClusters))