#uploading the data into R#
library(readr)
wine <- read_csv("C:/Users/Desktop/ExcelR/Assignment/PCA/wine.csv")

attach(wine)

winex<- wine [-1]

attach(winex)

#checking the correlationship
cor(winex)
##some are having cor-relationship with other variable##

##PC##
pcawine <- princomp(winex,cor = TRUE, scores = TRUE, covmat = NULL)
#summary of PCA#
summary(pcawine)

#loading of PCA
#pcawine$loadings

#checking if sum of square are one or not
sum(pcawine$loadings[,1]**2)
#1

#checking the relaiton between PCA1 AND PCA2
plot(pcawine$scores[,1], pcawine$scores[,2])
cor(pcawine$scores)
#points all over the place
#no correlationship

#information content
plot(pcawine)

#taking till 3th PCA, As mention in problem statement
#we can go for more as per requirement

#creating new data with PCA
newwine <- cbind(winex, pcawine$scores[,1:3])
View(newwine)

#clustering the new data
#last 3 columns
#from 14 to 16 columns
clus_wine <- newwine[,14:16]
View(clus_wine)

#standardization the data
norm_wine <- scale(clus_wine)

#finding the distance by Euclidean distance
winedis <- dist(norm_wine, method = "euclidean")

#clustering the data using Hierachical method
#complete method
hiwine1 <- hclust(winedis, method = "complete")
?hclust
#Displaying Dendrogram
plot(hiwine1, hang = -1)

#plot making more sense for complete method
#as cluster formation are clearly visible

#average method
hiwine2 <- hclust(winedis, method= "average")

#Displaying Dendrogram
plot(hiwine2, hang = -1)

#plot making some sense for average method also
#cluster formation visible but only 1 variable cluster are forming

#complete method
hiwine3 <- hclust(winedis, method = "centroid")

#Displaying Dendrogram
plot(hiwine3, hang = -1)
#plot not making any sense

#single method
hiwine4 <- hclust(winedis, method = "single")

#Displaying Dendrogram
plot(hiwine4, hang = -1)
#plot making no sense

##complete method is making more sense##
#according to dendrogram cluster are around 12

plot(hiwine1, hang = -1)
rect.hclust(hiwine1, k=12, border="red")
##12 cluster are making sense but it is subjective call
##as no data specificaiton was given

# cut tree into 12 clusters
?rect.hclust
groups <- cutree(hiwine1, k=12)
# groups or cluster numbers##
membership<-as.matrix(groups)
#creating a new data frame with membership
final <- data.frame(newwine, membership)
View(final)

#clubing them together#
aggregate(newwine,by=list(final$membership),mean)
#we can go for lesser no of cluster 
#neme that as per requirement