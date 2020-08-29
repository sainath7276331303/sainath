##uploading the file##

library(readr)
delivery_time <- read_csv("C:/Users/Desktop/ExcelR/Assignment/Simple Linear Regg/delivery_time.csv")

#checking data
View(delivery_time)

#attaching further use
attach(delivery_time)
str(delivery_time)
############### EDA ###################################

#MEAN, MODE AND MEDIAN
summary(delivery_time)
#we can see mean mode and median below

#checking missing values
table(is.na(delivery_time))
#no missing values

#checking outliers by box plot
boxplot(delivery_time)
#no outliers

#distribution of data
hist(delivery_time$`Sorting Time`)
hist(delivery_time$`Delivery Time`)
#they are normally distributed

######### MODEL DEVELOPMENT ###########################

########################         y ~x           ###########################
##y = delivery time, x= sorting time##
#PLOTING THE DATA
plot(`Delivery Time`,`Sorting Time`)

#checking the corelation between them
cor(`Delivery Time`,`Sorting Time`)
#it is 82%

# creating the model
simple_reg<- lm(`Delivery Time`~`Sorting Time`)

#checking the summary
summary(simple_reg)
#r sq = 68.23

##checking the RMSE
mean(simple_reg$residuals)
rmse1 <- sqrt(mean(simple_reg$residuals^2))

#storing the rmse
rmse1

## clearly transformation is needed for better r sqr and RMSE ##

################ TRANSFORMING THE DATA ###########################

# we can tranform the data and check the relaiton if its good we will bulid the model

######### transforming x ##############

####### y ~ x^2 ##

# creating a new variable

delivery_time["sorting_squ"] <- (delivery_time$`Sorting Time`)^2 
#attaching it for further use
attach(delivery_time)
#plotting to see corelation
plot(`Delivery Time`~sorting_squ)
#checking the corelation
cor(`Delivery Time`, sorting_squ)
#it is 79%
#we can skip if for as it is not more than previous coreltion

###### y ~log(x) ######

# creating a new variables

delivery_time["sorting_log"] <- log(delivery_time$`Sorting Time`)
#attaching it for further use
attach(delivery_time)
#plotting the above data
plot(`Delivery Time`, sorting_log)
#checking the corelation
cor(`Delivery Time`, sorting_log)
# it is 83.39%

################### buliding a model #################
log_model_x <- lm(`Delivery Time`~sorting_log, data = delivery_time)
# checking the summary
summary(log_model_x)
#r_sq = 69.54%

#checking the rmse
rmse2 <- sqrt(mean((log_model_x$residuals)^2))
# we will go further in transformation to find any other better relationship
# if we dont get it, we will go with the above one

####################### transforming the y #####################

# log(y) ~ x
#as log of delivery time is not avaiable we will
#creating the required variable

delivery_time["delivery_log"]<- log(delivery_time$`Delivery Time`)
#attaching for further use
attach(delivery_time)
#plotting the above
plot(delivery_log, `Sorting Time`)
#checking the corelation
cor(delivery_log, `Sorting Time`)
# it is 84.31%

# as the above have highest corelation we can go for model

log_model_y <- lm(delivery_log~`Sorting Time`, data = delivery_time)
summary(log_model_y)  
# r squ = 71%  

#checking rmse
rmse3 <- sqrt(mean((exp(log_model_y$residuals))^2))
rmse3

#making table of rmse
rmse_table <- data.frame(c("1","2","3"), c(rmse1, rmse2,rmse3))
View(rmse_table)
#as we can see the third have the best rmse and significant r squ
#we can deploy the model