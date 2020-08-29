
#reading the data
library(readr)
glass <- read_csv("C:/Users/Desktop/ExcelR/Assignment/10. KNN/glass.csv")
View(glass)
#converting the num into factor
glass$Type<- as.factor(glass$Type)

#checking the proption
#As KNN is baised towards the majority class
table(glass$Type)
#as 1 & 2 class have more proption the algo make become baised towards them

#Splitting should be done randomly and proption of 1-7 should be checked
library("caTools")
sample = sample.split(glass,SplitRatio = 0.6)

train =subset(glass,sample ==TRUE)
test =subset(glass, sample==FALSE)

table(train$Type)
table(test$Type)

#Get labels for training and test datasets

train_labels <- train$Type
test_labels <- test$Type

#as the data is in different scale we normalize it
#Create a function to normalize the data
norm <- function(x){ return((x-min(x))/(max(x)-min(x)))}

#Applying the normalize fun to data
train_nor <- as.data.frame(lapply(train[1:9], norm))
test_nor <- as.data.frame(lapply(test[1:9], norm))
#Building a KNN model
library("class")
#install.packages("caret")
library("caret")

#writing a for loop for differnt values of k in knn
# we have to test on test dataset
test_acc <- NULL
train_acc <- NULL
for (i in seq(1,25,1))
{
  train_glass_pred <- knn(train=train_nor,test=train_nor,cl=train_labels,k=i)
  train_acc <- c(train_acc,mean(train_glass_pred==train_labels))
  test_glass_pred <- knn(train = train_nor, test = test_nor, cl = train_labels, k=i)
  test_acc <- c(test_acc,mean(test_glass_pred==test_labels))
}

# Testing Accuracy 

# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(1,25,1),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(1,25,1),test_acc,type="l",main="Test_accuracy",col="red")

acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(1,25,1)))
# Plotting 2 different graphs on same co-ordinate axis
#install.packages("ggplot2")
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))

#we can go with 2 or 3
?knn
glass_pred2 <- knn(train = train_nor,test = test_nor,cl= train_labels, k= 2)

test_labels
table(glass_pred2,test_labels)
#it seems like as value of k increases accuracy is decreasing
#for k vlaue of 2 it is giving okay results
#if we need to further improve upon accuracy we need more data points