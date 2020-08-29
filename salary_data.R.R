#Reading the data
library(readr)
salary_train <- read.csv(choose.files()) #choose training data
salary_test <- read.csv(choose.files()) #choose testing data

#combining the data for analysis
salary_all <- rbind(salary_train, salary_test)

#view the data
View(salary_all)
View(salary_test)
View(salary_train)

################################ EDA ######################################

#checking for null values
table(is.na(salary_all))
#no null values

#checking the distribution of data
hist(salary_all$age) #rightly skewed
boxplot(salary_all$age, horizontal = T) #having outliers

#for categorical data we use table
table(salary_all$workclass) #private are in more proportion 
table(salary_all$education) #HS-grad are in more prop
table(salary_all$educationno) #9 are in more prop
table(salary_all$occupation) # adm-clerical and sales are in prop
table(salary_all$relationship) #husband and not-in-family are in prop
table(salary_all$race) #white are in more prop
table(salary_all$sex) #male are in more than female

hist(salary_all$capitalgain) #highly skewed data
boxplot(salary_all$capitalgain, horizontal = T) #Too many outliers

hist(salary_all$capitalloss) #highly skewed data
boxplot(salary_all$capitalloss, horizontal = T) # too many outliers

hist(salary_all$hoursperweek) #centrally skewed data
boxplot(salary_all$hoursperweek, horizontal = T) #too many outliers 

table(salary_all$native) #united state have too many entries
table(salary_all$Salary) # <=50k is having more entries

#as our data is having different prop and baised towards <=50k
#the relationship may vary

#Checking the structure
str(salary_train)
str(salary_test)
str(salary_all)
#checking the summary
summary(salary_all)
#highly baised data in salary.

#checking the train and test data
summary(salary_train)
summary(salary_test)
#data is highly baises towards <= 50k

######################## writing a naive bayes model #######################
#naive bayes is in e1071
library(e1071)

#creating a model
model<-naiveBayes(salary_train$Salary~.,data=salary_train)

#storing the predicted values 
predicted <- predict(model,newdata = salary_test[,-1])

#creating a table of predicted values
table(predicted)
#comparing it with original values
table(salary_test$Salary)
#for accuracy we need a table
table1 <- table(predicted ,salary_test$Salary)
#checking the accuracy
accuracy1 <- ((sum(diag(table1)))/(sum(table1)))
accuracy1
#accuracy is 81.73%

##### for increasing the accuracy #####
#using laplase smoothing
#checking with different values of laplace
?naiveBayes

model1 <- naiveBayes(salary_train$Salary~., data = salary_train, laplace = 20)
pred1<-predict(model1,newdata = salary_test[,-1])

table(pred,salary_test$Salary)

table2 <- table(pred1,salary_test$Salary)
##as the data is highly biased towards <= 50k our model will be baised towards it

#checking the accuracy 
accuracy2 <- ((sum(diag(table2)))/(sum(table2)))
accuracy2

#creating a model on different family
model3 <- naiveBayes(salary_train$Salary~.,data = salary_train,family = 'Gaussian' )
#checking the summary
summary(model3)

#prediciting on test data
pred3 <- predict(model3, newdata = salary_test[,-1])
table3 <- table(pred3,salary_test$Salary)

#checking the accuracy 
accuracy3 <- ((sum(diag(table3)))/(sum(table3)))
accuracy3
