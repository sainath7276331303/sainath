library(C50)
data("iris")
View(iris)
iris_setosa<-iris[iris$Species=="setosa",]
iris_versicolor <- iris[iris$Species=="versicolor",] 
iris_virginica <- iris[iris$Species=="virginica",]
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])
# Building model on training data 
irisc5.0_train <- C5.0(iris_train[,-5],iris_train$Species)
windows()
plot(irisc5.0_train) # Tree graph
# Training accuracy
pred_train <- predict(irisc5.0_train,iris_train)
mean(iris_train$Species==pred_train)
library(caret)
confusionMatrix(pred_train,iris_train$Species)
predc5.0_test <- predict(irisc5.0_train,newdata=iris_test)
mean(predc5.0_test==iris_test$Species)
confusionMatrix(predc5.0_test,iris_test$Species)
library(gmodels)
# Cross tables
CrossTable(iris_test$Species,predc5.0_test)

##### Using tree function 
library(tree)
# Building a model on training data 
iris_tree <- tree(Species~.,data=iris_train)
plot(iris_tree)
text(iris_tree,pretty = 0)

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(iris_tree,newdata=iris_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(iris_tree,newdata=iris_test)
pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]
mean(pred_tree$final==iris_test$Species)
CrossTable(iris_test$Species,pred_tree$final)

