#reading the data
library(readr)
emp_data <- read_csv("C:/Users/Desktop/ExcelR/Assignment/Simple Linear Regg/emp_data.csv")
# pre viewing the data
View(emp_data)
#attaching the data for further use
attach(emp_data)

######################## EDA #################################

#checking null values
table(is.na(emp_data))
#no null values in our data

#checking for outliers

#for salary hike
boxplot(emp_data$Salary_hike)
#no outliers

#churn out rate
boxplot(emp_data$Churn_out_rate)
#no outliers

#checking for distribution of data

#for salary hike 
boxplot(emp_data$Salary_hike)
#no outlier here

#checking the distribution

# for salary data
hist(emp_data$Salary_hike)

#for churn out rate
hist(emp_data$Churn_out_rate)

#### Model Development ###########################
## x=Salary    y=Churn##
#plotting the data
plot(Salary_hike ,Churn_out_rate)
#they have a strong relationship with negative trend

#checking the corelation
cor(Salary_hike,Churn_out_rate)
# -91.11  

#creating a model
###################### y~x #######################

linear_reg<- lm(Churn_out_rate~Salary_hike)

# summary
summary(linear_reg)
#r squ = 83.12

#checking errors
linear_reg$residuals
##RMSE##
linear_rmse <- sqrt(mean((linear_reg$residuals)^2))
linear_rmse
# rmse = 3.99

#transforming the data and trying new model
###################### Transformed Model #################################

######### y ~ x^2 ###########

#plotting the above 
plot(Churn_out_rate,((Salary_hike)^2))
#checking their corelation
cor(Churn_out_rate,((Salary_hike)^2))
#as corelation we will check for other transformation

######## y ~ log(x) ########

#plotting the above
plot(Churn_out_rate, (log(Salary_hike)))
#checking their corelaiton
cor(Churn_out_rate, (log(Salary_hike)))
# - 92.12 
#it is best till now we will go with it for model

### creating the model #########

log_reg <- lm(Churn_out_rate~(log(Salary_hike)), data = emp_data)

#summary of the model
summary(log_reg)
# r squ = 84.86
#better than previous one

#checking the rmse
rmse_log <- sqrt(mean((log_reg$residuals)^2))
rmse_log
# rmse = 3.78
#better rmse than previous model
#we can finalize it