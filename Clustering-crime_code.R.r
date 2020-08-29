library(readr)
crime <- read_csv("C:/Users/Desktop/ExcelR/Assignment/Clustering/crime_data.csv")
View(crime)
attach(crime)

# Normalizing continuous columns to bring them under same scale
nordata <- scale(crime[,2:5])

# distance matrix
dis <- dist(nordata, method = "euclidean") 
?hclust
fit <- hclust(dis, method="complete")

# display dendrogram
plot(fit)
plot(fit, hang=-1)

rect.hclust(fit, k=6, border="red")

# cut tree into 4 clusters
?rect.hclust
groups <- cutree(fit, k=4) 

##cluster by average method##
fit2 <- hclust(dis, method="average")
# display dendrogram
plot(fit2)
plot(fit2, hang=-1)

rect.hclust(fit2, k=5, border="red")

## cluster by centroid##
fit3 <- hclust(dis, method="centroid")
# display dendrogram
plot(fit3)
plot(fit3, hang=-1)

rect.hclust(fit3, k=5, border="red")
## centroid is not making sense##

## cut tree into 5 clusters##
groups <- cutree(fit, k=5) 
# groups or cluster numbers##
membership<-as.matrix(groups)
final <- data.frame(crime, membership)
