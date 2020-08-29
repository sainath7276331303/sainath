#reading the data
library(readr)
Zoo <- read.csv(file.choose())
View(Zoo)
#attaching for further use
attach(Zoo)

############################### EDA ######################################

#checking for null values
table(is.na(Zoo))

#cheking the data distribution
table(Zoo$animal.name) #100 % pure data
#analysis becomes different in such cases

#checking the structure and summary of data
str(Zoo)
summary(Zoo)

#splitting the data into training and testing
library("caTools")
sample = sample.split(Zoo,SplitRatio = 0.75)

train =subset(Zoo[,2:18],sample ==TRUE)
test =subset(Zoo[,2:18], sample==FALSE)

#Get labels for training and test datasets
train_label <- subset(Zoo[,1], sample== TRUE)
test_label <- subset(Zoo[,1], sample== FALSE)

#as the data is in different scale we normalize it
#Create a function to normalize the data
norm <- function(x){ return((x-min(x))/(max(x)-min(x)))}

#Applying the normalize fun to data
train_nor <- as.data.frame(lapply(train, norm))
test_nor <- as.data.frame(lapply(test, norm))

#Building a KNN model
library("class")
#install.packages("caret")
library("caret")

#creating the test and trainning accuracy as null
test_acc <- NULL
train_acc <- NULL
#writing a for loop for different values of k
?knn
for (i in seq(3,200,2))
{
  train_zoo_pred <- knn(train= train_nor,test=test_nor,cl=train_label,k=i)
  train_acc <- c(train_acc,mean(train_zoo_pred == train_label))
  test_zoo_pred <- knn(train = train_nor, test = test_nor, cl = train_label, k=i)
  test_acc <- c(test_acc,mean(test_zoo_pred==test_label))
}

# Testing Accuracy 

# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,200,2),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,200,2),test_acc,type="l",main="Test_accuracy",col="red")


acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(3,200,2)))
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))

#it seems like k= 47
#but it makes no sense to make 

train_zoo_pred2 <- knn(train= train_nor,test=test_nor,cl=train_label,k=41)
train_acc2 <- mean(train_zoo_pred2 == train_label)
train_acc2
