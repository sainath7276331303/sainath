zoo <- read.csv("F:/Excelr/Datasets/zoo.csv")
View(zoo)
dim(zoo)
table(zoo$type)
prop.table(table(zoo$type))
round(prop.table(table(zoo$type))*100,1)
summary(zoo[c("hair","tail","legs")])
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))
zoo_norm <- as.data.frame(lapply(zoo[2:18], norm))  
View(zoo_norm)
head(zoo_norm)
train_zoo <- zoo_norm[1:50,]
test_zoo <- zoo_norm[51:101 ,]
zoo_train_labels <- zoo[1:150,1]
zoo_test_labels <- zoo[151:214,1]
round(prop.table(table(zoo_train_labels))*100,2)
round(prop.table(table(zoo_test_labels))*100,2)
library(caret)
library(lattice)
library(class)
zoo_test_labels <- zoo_test_labels["type"]
zoo_train_labels <- zoo_train_labels["type"]
zoo_test_pred <- knn(train = train_zoo, test = test_zoo,
                     cl = zoo_train_labels, k=i)
library(gmodels)
CrossTable(x = zoo_test_labels, y = zoo_test_pred,
           prop.chisq=FALSE)