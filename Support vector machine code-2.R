salary_train <- read.csv("F:/Excelr/Datasets/SalaryData_Train(1).csv")
View(salary_train)
str(salary_train)
salary_test <- read.csv("F:/Excelr/Datasets/SalaryData_Test(1).csv")
View(salary_test)
str(salary_test)
salary <- rbind(salary_train,salary_test)
View(salary)
str(salary)
colnames(salary)
library(caret)
library(ggplot2)
library(psych)
install.packages("kernlab")
library(kernlab)
library(e1071)
salary_train <- salary[1:30000,]
salary_test <- salary[30001:45221,]
View(salary)
model1<-ksvm(Salary~.,data = salary_train,kernel = "vanilladot")
model1
# kernel = rfdot 
model_rfdot<-ksvm(Salary ~.,data = salary_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=salary_test)
mean(pred_rfdot==salary_test$Salary)
# kernel = vanilladot
model_vanilla<-ksvm(Salary ~.,data = salary_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=salary_test)
mean(pred_vanilla==salary_test$Salary)
# kernal = besseldot
model_besseldot<-ksvm(Salary ~.,data = salary_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=salary_test)
mean(pred_bessel==salary_test$Salary)
# kernel = polydot
model_poly<-ksvm(Salary ~.,data = salary_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = salary_test)
mean(pred_poly==salary_test$Salary)
