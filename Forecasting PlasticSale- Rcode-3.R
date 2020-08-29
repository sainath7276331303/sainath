#reading the file from system
library(readr)
plastic_sales <- read_csv("PlasticSales.csv")
View(plastic_sales)
#attaching the file
attach(plastic_sales)
###################### EDA ##########################
#checking null values
table(is.na(plastic_sales))
#no null values

#checking outliers
boxplot(plastic_sales$Sales)
#no outlier#checking the distribution of data
hist(plastic_sales$Sales)

#plotting with time series data
windows()
plot(plastic_sales$Sales, type = "o")
#the plot seems like upward linear trend with additive seasonlity

#for model buliding we will need some pre-requsite
# 12 dummy variables 

x<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )# Creating dummies for 12 months
#renaming the column names
colnames(x)<-month.abb
View(x)

# creating a new time peroid index t
t <- 1:60

#creating t square 
t_squ <- t^2

#creating log of y i.e, log of passengers
sales_log <- log(plastic_sales$Sales)

#combining them and creating new data
newplastic <- cbind(plastic_sales, x,sales_log,t,t_squ)
#previewing the new data
View(newplastic)

#splitting into train and test data
train_plastic <- newplastic[1:48,]
test_plastic <- newplastic[49:60,]

#creating different model

############################### LINEAR MODEL ##################################

linear_pastic <- lm(Sales~t, data = train_plastic)
summary(linear_pastic)
#r squ = 33.05
#checking on test data
linear_pred <- data.frame(predict(linear_pastic, interval = "predict", newdata = test_plastic))
View(linear_pred)
#checking the RMSE
rmse_linear <- sqrt(mean((linear_pred$fit- test_plastic$Sales)^2))
rmse_linear
#RMSE = 260

############################### EXPONENTIAL MODEL #############################

expo_pastic <- lm(sales_log~t, data = train_plastic)
summary(expo_pastic)
#r squ = 31.73
#no need to check RMSE as r squ is not significant

############################# Quadratic MODEL ##################################

quad_pastic <- lm(Sales~t+t_squ, data = train_plastic)
summary(quad_pastic)
#r squ = 33.44
#r squ is not significant

########################### Additive Seasonality ###############################

additive_pastic <- lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data = train_plastic)
summary(additive_pastic)
#r squ = 76.91
#checking on test data
additive_pred <- data.frame(predict(additive_pastic,interval = "predict", newdata = test_plastic))
View(additive_pred)
#checking rmse
rmse_additive <- sqrt(mean((additive_pred$fit - test_plastic$Sales)^2))
rmse_additive
# RMSE = 235.60

########################## Additive seasonality with linear trend ############################

add_lin_plastic <- lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data= train_plastic)
summary(add_lin_plastic)
#r sq = 97.36
#checking on test data
add_lin_pred <- data.frame(predict(add_lin_plastic,interval = "predict", newdata = test_plastic))
View(add_lin_pred)
#checking rmse
rmse_lin_add <- sqrt(mean((add_lin_pred$fit - test_plastic$Sales)^2))
rmse_lin_add
# RMSE = 135.55

######################### Additive Seasonality with Quadratic ###############################

add_quad_plastic <- lm(Sales~t+t_squ+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data = train_plastic)
summary(add_quad_plastic)
#r squ = 98.32
#checking on test data
add_quad_plastic <- data.frame(predict(add_quad_plastic, interval = "predict", newdata = test_plastic))
View(add_quad_plastic)
#checking rmse
rmse_quad_add <- sqrt(mean((add_quad_plastic$fit- test_plastic$Sales)^2)) 
rmse_quad_add
#RMSE = 218.19
#THOUGH r squ is more rmse is more which is not acceptable

###################### Multiplicative Seasonality #############################################

multi_plastic <- lm(sales_log~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data = train_plastic)
summary(multi_plastic)
#r squ = 79.16
#as r squared significanlty decerase we can skip RMSE

##################### Multiplicative Seasonality linear trend ############################

multi_exp_plastic <- lm(sales_log~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data = train_plastic)
summary(multi_exp_plastic)
#r squ = 98.15
#checking on test data
multi_exp_pred <- data.frame(predict(multi_exp_plastic, interval = "predict", newdata = test_plastic))
View(multi_exp_pred)
#checking RMSE
rmse_multi_lin <- sqrt(mean((exp(multi_exp_pred$fit)- test_plastic$Sales)^2))
rmse_multi_lin
#RMSE = 160.68

#STORING THE RMSE FOR COMPARSION
table_RMSE <- data.frame(c("1","2","3","4","5"), c(rmse_additive,rmse_lin_add,rmse_linear, rmse_multi_lin, rmse_quad_add))
View(table_RMSE)

#Additive seasonality with linear trend is our final model
#################################### FINAL MODEL ###########################################

final_plastic <- lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data= newplastic)
summary(final_plastic)
final_plastic$residuals
#now we will forcast for next 12 months for forcasting purpose
#creatign dummeies for next 12 months 

x1<- data.frame(outer(rep(month.abb,length = 12), month.abb,"==") + 0 )
#renaming the column names
colnames(x1)<-month.abb
View(x1)
#creating time series index t
t <- 61:72

#binding them all
new_forcast_plastic <- cbind(x1,t)

#saving as data frame
new_forcast_plastic <- as.data.frame(new_forcast_plastic)

#now forcasting next 12 value by our final model
final_forcast <- predict(final_plastic,newdata = new_forcast_plastic)

#saving the forcasted values
#backtransforming the error as y was transformed
newtest_plastic["forcast"] <- final_forcast 
View(new_forcast_plastic)

#saving the residuals of the new model
#to check the relaionship bet them
resid <-(residuals(final_plastic))
resid[1:10]
View(resid)
#plotting the acf cure to find the corelaiton between errors
windows()
acf(resid,lag.max = 12)

# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lag-1 
# basic ARIMA model

k <- arima(resid, order=c(1,0,0))
str(k)
summary(k)
windows()
acf(k$residuals, lag.max = 12)

#creating data frame of previous error and new errors
View(data.frame(res= resid, newresid= k$residuals))
#predicting the next 4 error from our model
pred_res <- predict(k, ahead = 12) 
#saving the errors
newtest_plastic["predict_error"] <- (pred_res$pred)

#adding the error for better forcasted values
newtest_plastic["Fianl Forcast"] <- newtest_plastic$predict_error+newtest_plastic$forcast
View(newtest_plastic)

