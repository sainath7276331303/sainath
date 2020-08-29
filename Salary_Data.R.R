#uploading the data

library(readr)
Salary_Data <- read_csv("C:/Users/Desktop/ExcelR/Assignment/Simple Linear Regg/Salary_Data.csv")

#previewing data
View(Salary_Data)

#attaching the data for further use
attach(Salary_Data)

######### EDA ################

#summary of data mean and median
summary(Salary_Data)

#checking the data for null values
table(is.na(Salary_Data))
#no null values

#checking the distribution of data

#for years of experience
hist(Salary_Data$YearsExperience)
#for salary
hist(Salary_Data$Salary)
#they both are normally distributes

#checking for outliers
#for years of exp
boxplot(Salary_Data$YearsExperience)
#no outlier
#for distribution of salary
boxplot(Salary_Data$Salary)
#no outliers here 

################################ MODEL DEVELOPMENT ##############################

##y = Salary_hike, x= Yearsexperience##

#Ploting the given data for insight
plot(YearsExperience,Salary)
#here they have good postive relationship between each other

#checking corealtion between them
cor(YearsExperience,Salary)
#97.82%
#they have a storng corelationship


#buliding up a linear model
########## y~x #########

linear_reg<- lm(Salary~YearsExperience, data = Salary_Data)

#checking summary
summary(linear_reg)
#r squ = 95.7% 
#which is really good

#checking errors
linear_reg$fitted.values

#mean of error should be zero as no tranformation is done
mean(linear_reg$residuals)

## checking RMSE##
rmse_lin <- sqrt(mean((linear_reg$residuals)^2))

rmse_lin
# rmse = 5592.04

# no other model will give us this good result but simply checking other model too

############ y ~ x^2 ##########
plot(Salary, ((YearsExperience)^2))
cor(Salary, (YearsExperience^2))
#95.6%
#slightly lesser than orignial
 
#### y~ log(x) ####
plot(Salary,(log(YearsExperience)))
cor(Salary,log(YearsExperience))
#92.40%
#lesser than original one

############ log(y) ~x ########
plot(log(Salary), YearsExperience)
cor(log(Salary), YearsExperience)
#96.53%
#still lesser than original one

#so our 1st model is giving us the best results