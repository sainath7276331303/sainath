##reading the file in R##

library(readr)
calories_consumed <- read_csv("C:/Users/ExcelR/Assignment/Simple Linear Regg/calories_consumed.csv")
View(calories_consumed)

#attaching file for further use
attach(calories_consumed)

######### EDA #####################

#summary of data
summary(calories_consumed)
#we can check mean, mode and median here

#checking null values
is.na(calories_consumed)
table(is.na(calories_consumed))

#In our data there are no null values

#plotting the data
windows()
plot(calories_consumed,`Weight gained (grams)`)

#Checking outliers
windows()
boxplot(calories_consumed)
#no outliers either

#checking the corealation between x and y
cor(`Weight gained (grams)`, `Calories Consumed`)
## as it is 95%, they are storngly realated##

############################ DIFFERENT regration model #######################
############################       lm(Y ~ X)           #########################

simple_reg <- lm(`Weight gained (grams)`~`Calories Consumed`)
#checking the summary
summary(simple_reg)
#r_squ1 <- 89.68

#checking the errors
reg$residuals

#sum of errors should be zero
sum(reg$residuals)

#mean of errors should be zero
mean(reg$residuals)

#RMSE value
rmse1 <- sqrt(sum(reg$residuals^2)/nrow(calories_consumed))
#RMSE1 <- 103.3

##################### transforming the data for different models ###############
#####################                y ~ x^2                     #########################

#creating the required variable
calories_consumed$`Weight gained (grams2)`<- (calories_consumed$`Weight gained (grams)`)^2

#checking the coreltion between them 
cor(calories_consumed, `Weight gained (grams2)`)

############ creating the model #################

squ_model <- lm(`Calories Consumed`~`Weight gained (grams2)`, data = calories_consumed)

#checking the summary
summary(squ_model)
#r sq = 80.33

# checking rmse

rmse2 <- sqrt(sum(squ_model$residuals^2)/nrow(calories_consumed))
rmse2
#RMSE2 = 321

############################### y ~ log(x) ###########################
#creating the required variable

calories_consumed$`Weight gained (gramslog)` <- log(calories_consumed$`Weight gained (grams)`)

#plotting the variale vs y

plot(calories_consumed,`Weight gained (gramslog)`)

#checking their corelation

cor(calories_consumed, `Weight gained (gramslog)`)

############ creating the model #########################

log_model <- lm(`Calories Consumed` ~`Weight gained (gramslog)`, data = calories_consumed)
#checking the summary
summary(log_model)
# as r sqa = 87.76

#CHECKING RMSE

rmse3 <- sqrt(sum(log_model$residuals^2)/nrow(calories_consumed))
rmse3
#RMSE3 = 235.55

############# making the rmse table ######################

table_rmse <- data.frame(c("1","2","3"), c(rmse1,rmse2,rmse3))
View(table_rmse)
#as we can see 1st model have the best RMSE