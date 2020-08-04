library(randomForest)
fraud <- read.csv("F:/Excelr/Datasets/Fraud_check.csv")
View(fraud)
str(fraud)
summary(fraud)
colnames(fraud)
prop.table(table(fraud$Urban))
prop.table(table(fraud$Taxable.Income))
Income_result<-NULL
Income_result<-ifelse(fraud$Work.Experience>10,5,1)
fraud[,"Income_result"]<-Income_result
fraud$Work.Experience<-as.factor(fraud$Work.Experience)
fraud$Urban<-as.factor(fraud$Urban)
fraud$Taxable.Income<-as.factor(fraud$Taxable.Income)
fraud$Income_result<-as.factor(fraud$Income_result)
str(fraud)
fraud_risk<-fraud[fraud$Income_result=="5",]
fraud_good<-fraud[fraud$Income_result=="1",]
data_train<-rbind(fraud_risk[1:150,],fraud_good[1:150,])
View(data_train)
data_test<-rbind(fraud_risk[151:199,],fraud_good[151:201,])
View(data_test)
fit.forest<-randomForest(Income_result~.,data = data_train,na.action=na.roughfix,importance=TRUE)
# Training accuracy 
mean(data_train$Income_result==predict(fit.forest,data_train))
# Prediction of train data
pred_train <- predict(fit.forest,data_train)
library(caret)
# Confusion Matrix
confusionMatrix(data_train$Income_result, pred_train)
# Predicting test data 
pred_test <- predict(fit.forest,newdata=data_test)
mean(pred_test==data_test$Income_result)  
# Confusion Matrix 
confusionMatrix(data_test$Income_result, pred_test)
# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)
acc_company <- mean(fraud$Income_resultis==predict(fit.forest))
acc_company
varImpPlot(fit.forest)
