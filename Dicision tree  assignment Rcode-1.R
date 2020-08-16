install.packages("C50")
library(C50)
company <- read.csv("F:/Excelr/Datasets/Company_Data.csv")
View(company)
str(company)
High=ifelse(company$Sales < 10,"No","Yes")
CD=data.frame( company,High )
View(High)
View(CD)
table(CD$High)   

#Split the data into train and test
CD_train<-rbind(CD[1:200,])
CD_test<-rbind(CD[200:400,])
str(CD)

# Building training data set
company_train<-C5.0(CD_train[,-10],CD$High)
windows()
plot(company_train) #tree graph

# Training accuracy
pred_train<-predict(company_train,CD_train)
pred_train
mean(CD_train$High==pred_train)
library(caret)
confusionMatrix(pred_train,CD_train$High)

# predicting on test data
predc5.0_test<-predict(company_train,newdata =CD_test)
predc5.0_test
mean(predc5.0_test==CD_test$High)
confusionMatrix(predc5.0_test,CD_test$High)
library(gmodels)

# Cross table solution
CrossTable(CD_test$High,predc5.0_test)

##### Using tree function 
install.packages("tree")
library(tree)

# Building a model on training data 
company_tree<-tree(High~.,data=CD_train)
plot(company_tree)
text(company_tree,pretty=0)

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(company_tree,newdata=CD_test))
pred_tree["final"] <- NULL
pred_test_df  <- predict(company_tree,newdata=CD_test)
pred_tree$final  <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]
pred_tree$final <-as.factor(pred_tree$final)
View(pred_tree$final)

summary(pred_tree$final)

summary(CD_test$High)

mean(pred_tree$final==CD_test$High)    

CrossTable(CD_test$High,pred_tree$final)

confusionMatrix(CD_test$High,pred_tree$final)
