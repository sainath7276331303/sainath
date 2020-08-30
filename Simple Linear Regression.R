#-------------------------Calories_consumed--------------------------------#
library(readr)
calories <- read.csv("file.choose()")
View(calories)
summary(calories)
plot(calories$Calories.Consumed, calories$Weight.gained..grams.)
attach(calories)
cor(Calories.Consumed, Weight.gained..grams.)
Weight_gain_model <- lm(calories$Weight.gained..grams.~calories$Calories.Consumed, calories)
summary(Weight_gain_model)
predict(Weight_gain_model)

#------------------------Delivery_time---------------------------------#
library(readr)
Delivery_time <- read.csv("file.choose()")
View(Delivery_time)
summary(Delivery_time)
plot(Delivery_time$Sorting.Time, Delivery_time$Delivery.Time)
attach(Delivery_time)
cor(Sorting.Time, Delivery.Time)
Delivery_time_model <- lm(Delivery_time$Sorting.Time~Delivery_time$Delivery.Time)
summary(Delivery_time_model)
predict(Delivery_time_model)

#---------------------------Emp_data------------------------------#
library(readr)
Emp_data <- read.csv("File.choose()")
View(Emp_data)
summary(Emp_data)
plot(emp_data$Salary_hike, emp_data$Churn_out_rate)
attach(Emp_data)
cor(Salary_hike, Churn_out_rate)




#-----------------------Salary_data---------------------------------#
library(readr)
salary_data <- read.csv("File.choose()")
View(salary_data)
summary(salary_data)
plot(salary_data$YearsExperience, salary_data$Salary)
attach(salary_data)
cor(YearsExperience, Salary)
salary_data_model <- lm(salary_data$Salary~salary_data$YearsExperience)
summary(salary_data_model)
predict(salary_data_model)
plot(log(YearsExperience), Salary)
cor(log(YearsExperience), Salary)
salary_log <- lm(Salary ~ log(YearsExperience)) 
summary(salary_log)
predict(salary_log)
salary_log$residuals
sqrt(sum(salary_log$residuals^2)/nrow(salary_data))
confint(salary_log,level=0.95)
predict(salary_log, interval="confidence")
plot(YearsExperience, log(Salary))
cor(YearsExperience, log(Salary))
salary_exp <- lm(log(Salary) ~ YearsExperience)
summary(salary_exp)
salary_exp$residuals
confint(salary_exp, level = 0.95)
predict(salary_exp, interval="confidence")
#  Polynomial model with 2 degree
plot(Salary,YearsExperience)
plot(Salary*Salary, YearsExperience)
cor(Salary*Salary,  YearsExperience)
plot(Salary*Salary,  log(YearsExperience))
cor(Salary, log(YearsExperience))
reg2degree <- lm(log(YearsExperience) ~ Salary + I(Salary*Salary))
summary(reg2degree)
logpol <- predict(reg2degree)
expy <- exp(logpol)
err = YearsExperience$AT - expy
sqrt(sum(err^2)/nrow(wc_at))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")
#  Polynomial model with 3 degree

reg3degree<-lm(log(YearsExperience)~Salary + I(Salary*Salary) + I(Salary*Salary*Salary))
summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)
library(dplyr)
# visualization
ggplot(data = salary_data, aes(x = salary + I(Salary^2) + I(Salary^3), y = YearsExperience)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_data, aes(x=Salary+I(Salary^2)+I(Salary^3), y=expy3))
