library(caret)
library(ggplot2)
library(psych)
install.packages("kernlab")
library(kernlab)
library(e1071)
forest <- read.csv("F:/Excelr/Datasets/forestfires.csv")
View(forest)
str(forest)
table(forest$size_category)
View(forest)
#Creating dummies
forest$day=as.integer(factor(forest$day,levels = c("sun","mon","tue","wed","thu","fri","sat"),labels = c(1,2,3,4,5,6,7)))
forest$month=as.integer(factor(forest$month,levels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),labels=c(1,2,3,4,5,6,7,8,9,10,11,12)))
forest$size_category=as.integer(factor(forest$size_category,levels = c("large","small"),labels=c(1,0)))
str(forest)
#Normalising
norm <- function(x){
     return((x-min(x))/(max(x)-min(x)))
   }
forest_norm<-as.data.frame(lapply(forest, norm))
head(forest_norm)
#Splitting of data into test and train
splittrain<-createDataPartition(forest_norm$area,p=0.8,list = F)
View(splittrain)
train_forest<-forest_norm[splittrain,]
test_forest<-forest_norm[-splittrain,]
View(test_forest)
modelrbfdot <- ksvm(area~.,data=train_forest,kernel="rbfdot")
predrbfdot <- predict(modelrbfdot,newdata=test_forest)
cor(predrbfdot,test_forest$area)                           
plot(predrbfdot)
#2.	Model 2 : besseldot
modelbessel <- ksvm(area~.,data=train_forest,kernel="besseldot")
predbessel <- predict(modelbessel,newdata=test_forest)
cor(predbessel,test_forest$area)
plot(predbessel)
#3.	Model 3: polydot
modelpoly <- ksvm(area~.,data=train_forest,kernel="polydot")
predpoly <- predict(modelpoly,newdata=test_forest)
cor(predpoly,test_forest$area)                            
plot(predpoly)
#4.	Model 4: vanilladot
modelvanilla <- ksvm(area~.,data=train_forest,kernel="vanilladot")
predvanilla <- predict(modelvanilla,newdata=test_forest)
cor(predvanilla,test_forest$area)
plot(predvanilla)
#5.	Model 5: tanhdot
modeltanh <- ksvm(area~.,data=train_forest,kernel="tanhdot")
predtanh <- predict(modeltanh,newdata=test_forest)
cor(predtanh,test_forest$area)
plot(predtanh)
#Model 6 : anovadot
modelanova <- ksvm(area~.,data=train_forest,kernel="anovadot")
predanova <- predict(modelanova,newdata=test_forest)
cor(predanova,test_forest$area)
plot(predanova)

          
     