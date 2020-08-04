install.packages("randomForest")
library(randomForest)
company<-read.csv("F:/Excelr/Datasets/Company_Data.csv")
View(company)
str(company)
colnames(company)
summary(company)
prop.table(table(company$Urban))
prop.table(table(company$US))
Sales_result<-NULL
Sales_result<-ifelse(company$Sales>7.490,1,0)
company[,"Sales_result"]<-Sales_result
company$ShelveLoc<-as.factor(company$ShelveLoc)
company$Urban<-as.factor(company$Urban)
company$US<-as.factor(company$US)
company$Sales_result<-as.factor(company$Sales_result)
str(company)
sales_high<-company[company$Sales_result=="1",]
sales_low<-company[company$Sales_result=="0",]
data_train<-rbind(sales_high[1:150,],sales_low[1:150,])
data_test<-rbind(sales_high[151:199,],sales_low[151:201,])
fit.forest<-randomForest(Sales_result~.,data = data_train,na.action=na.roughfix,importance=TRUE)
# Training accuracy 
mean(data_train$Sales_result==predict(fit.forest,data_train))
# Prediction of train data
pred_train <- predict(fit.forest,data_train)
library(caret)
# Confusion Matrix
confusionMatrix(data_train$Sales_result, pred_train)
# Predicting test data 
pred_test <- predict(fit.forest,newdata=data_test)
mean(pred_test==data_test$Sales_result)  
# Confusion Matrix 
confusionMatrix(data_test$Sales_result, pred_test)
# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)
acc_company <- mean(company$Sales_resultis==predict(fit.forest))
acc_company
varImpPlot(fit.forest)

