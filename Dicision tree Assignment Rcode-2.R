library(C50)
fraud  <- read.csv("F:/Excelr/Datasets/Fraud_check.csv")
View(fraud)
str(fraud)
Income=ifelse(fraud$Taxable.Income<=30000,"Risky","Good")
FC=data.frame(fraud,Income)
View(Income)
View(FC)
table(FC$Income)

#Split the dataset into train and test model
FC_train<-rbind(FC[1:300,])
FC_test<-rbind(FC[300:600,])
str(FC)

# Building training dataset
fraud_train<-C5.0(FC_train[,-10],FC$Income)
windows()
plot(fraud_train) #Tree graph

# Training accuracy
pred_train <-predict(fraud_train,FC_train)
pred_train
mean(FC_train$Income==pred_train)
library(caret)
confusionMatrix(pred_train,FC_train$Income)

# Predicting on test dataset
predc5.0_test <-predict(fraud_train,newdata =FC_test)
predc5.0_test
mean(predc5.0_test==FC_test$Income)
confusionMatrix(predc5.0_test,FC_test$Income)
library(gmodels)

# Cross table
CrossTable(FC_test$Income,predc5.0_test)

# Using tree function 
library(tree)

# Building a model on training dataset
fraud_tree<-tree(Income~.,data=FC_train)
plot(fraud_tree)
text(fraud_tree,pretty=0)

# Predicting the test data using the model
pred_tree  <- as.data.frame(predict(fraud_tree,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df  <- predict(fraud_tree,newdata=FC_test)
pred_tree$final  <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]
pred_tree$final  <-as.factor(pred_tree$final)
View(pred_tree$final)
summary(pred_tree$final)
summary(FC_test$Income)
mean(pred_tree$final==FC_test$Income)       
CrossTable(FC_test$Income,pred_tree$final)     
confusionMatrix(FC_test$Income,pred_tree$final)
