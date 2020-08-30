glass <- read.csv("F:/Excelr/Datasets/glass.csv")
View(glass)
dim(glass)
#table to check how many classes in output variable Type
table(glass$Type)
# table or proportation of enteries in the datasets. What % of each entry or class is
prop.table(table(glass$Type))
#roundup proportion values in 1 decimal
round(prop.table(table(glass$Type))*100,1)
#take any three input variables to check the ranges
summary(glass[c("Na","Mg","Al")])
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))
#Apply the normalization function to glass dataset from column 1 to 9 because last column is output variable
glass_norm<-as.data.frame(lapply(glass[1:9], norm))
View(glass_norm)
head(glass_norm)
#create training and test datasets (in 70% and 30 %)
train_glass <- glass_norm[1:150,]
test_glass <- glass_norm[151:214 ,]
#Get labels for training and test datasets
glass_train_labels <- glass[1:150,1]
glass_test_labels <- glass[151:214,1]
#proportion of above training and test datasets
round(prop.table(table(glass_train_labels))*100,2)
round(prop.table(table(glass_test_labels))*100,2)
install.packages("caret")
library("caret")
library(lattice)
# Build a KNN model on taining dataset
library("class")
# Building the KNN model on training dataset and also need labels which we are                including cl. Once we build the prediction model
# we have to test on test dataset
glass_test_pred <- knn(train = train_glass, test = test_glass,
                       cl = glass_train_labels, k=21)
library(gmodels)
CrossTable(x = glass_test_labels, y = glass_test_pred,
           prop.chisq=FALSE)
glass_test_pred1 <- knn(train = train_glass, test = test_glass,
                        cl = glass_train_labels, k=11)
CrossTable(x = glass_test_labels, y = glass_test_pred1,
           prop.chisq=FALSE)
