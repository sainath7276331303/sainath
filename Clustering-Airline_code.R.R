#Forcasting on coke sale data
#calling data in R enviro
library(readxl)
air <- read_excel("Airlines+Data.xlsx")
#previewing the data
View(air)
attach(air)
###################### EDA ##########################
#checking null values
table(is.na(air))
#no null values

#checking outliers
boxplot(air$Month)
boxplot(air$Passengers)
#no outliear either

#checking the distribution of data
hist(air$Passengers)

#plotting with time series data
windows()
plot(air$Passengers, type = "o")
#by observing the data we can say its  upward trend and multiplicative seasonality

#for model buliding we will need some pre-requsite
# 12 dummy variables 

x<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
#renaming the column names
colnames(x)<-month.abb
View(x)

# creating a new time peroid index t
t <- 1:96

#creating t square 
t_squ <- t^2

#creating log of y i.e, log of passengers
passenger_log <- log(air$Passengers)

#combining them and creating new data
newair <- cbind(air,x,t,t_squ,passenger_log)
#previewing the new data
View(newair)

#splitting into train and test
trainair <- newair[1:84,]
testair <- newair[85:96,]

#creating different model

############################### LINEAR MODEL ####################################################

linearair <- lm( Passengers~t, data = trainair)
summary(linearair)
#r squ = 79.23

#predicting the test data
linear_pred <- data.frame(predict(linearair, interval = 'predict', newdata = testair))
View(linear_pred)
#checking the rmse of test data
rmse_linear <- sqrt(mean((testair$Passengers-linear_pred$fit)^2))
rmse_linear
# RMSE = 53.19

############################ EXPONENTIAL MODEL ####################################

expair <- lm(passenger_log~t, data = trainair)
summary(expair)
# r sq = 82.39

#predicting on the test data
exp_pred <- data.frame(predict(expair, interval = "predict", newdata = testair))
#  checking the test data rmse
rmse_exp <-  sqrt(mean((testair$Passengers- exp(exp_pred$fit))^2))
rmse_exp
# RMSE = 46.05

############################# Quadratic MODEL #####################################

quad_air <- lm(Passengers~(t+t_squ), data = trainair)
summary(quad_air)
#r sq = 79.63

#predicting on test data
quad_pred <-data.frame(predict(quad_air, interval = "predict", newdata = testair))
View(quad_pred)
#checking the rmse
rmse_quad <- sqrt(mean((testair$Passengers-quad_pred$fit)^2))
rmse_quad
#rmse = 48.05

########################### Additive Seasonality ###################################

addair <- lm(Passengers~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data = trainair)
summary(addair)
# r squ = 16.74
#predicting the test data
add_pred <- data.frame(predict(addair, interval = "predict", newdata = testair))
View(add_pred)
#checking the rmse
rmse_add <- sqrt(mean((testair$Passengers-add_pred$fit)^2))
rmse_add
# RMSE = 132.81

########################## Additive seasonality with linear trend ##################

addair_lin <- lm(Passengers~(t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec), data = trainair)
summary(addair_lin)
#r sqa = 95.51
#predicting the test data
addair_pred <- data.frame(predict(addair_lin, interval = "predict", newdata = testair))
View(addair_pred)
#checking the test data RMSE
rmse_add_lin <- sqrt(mean((testair$Passengers-addair_pred$fit)^2))
rmse_add_lin
# RMSE = 35.34

######################### Additive Seasonality with Quadratic ###########################

addair_quad <-lm(Passengers~(t+t_squ+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec), data = trainair)
summary(addair_lin)
#r sqa = 95.51
#predicting the test data
addairlin_pred <- data.frame(predict(addair_quad, interval = "predict", newdata = testair))
View(addairlin_pred)
#checking the test data RMSE
rmse_air_quad <- sqrt(mean((testair$Passengers- addairlin_pred$fit)^2))
rmse_air_quad
# RMSE = 26.36

###################### Multiplicative Seasonality ######################################

multiair <- lm(passenger_log~(Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec), data = trainair)
summary(multiair)
# r squ = 15.48
#checking for test data
multiair_pred <- data.frame(predict(multiair, interval = "predict", newdata = testair))
View(multiair_pred)
#checking RMSE of test data
rmse_multi <- sqrt(mean((testair$Passengers- exp(multiair_pred$fit))^2))
rmse_multi
# RMSE = 140.06

##################### Multiplicative Seasonality additive trend ###################

multiexpair <- lm(passenger_log~(t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec), data = trainair)
summary(multiexpair)
# r squ = 97.63
#checking on test data
multiexp_pred <- data.frame(predict(multiexpair, interval = "predict", newdata = testair))
View(multiexp_pred)
#checking RMSE of test data
rmse_multiexp <- sqrt(mean((testair$Passengers-exp(multiexp_pred$fit))^2))
rmse_multiexp
#RMSE = 10.51

# Preparing table on model and it's RMSE values
table_RMSE <- data.frame(c("1","2","3","4","5","6","7","8"), c(rmse_add, rmse_add_lin, rmse_air_quad,rmse_exp,rmse_linear,rmse_multi,rmse_multiexp,rmse_quad))
View(table_RMSE)

#for multiplicative seasonality and exponcial trend RMSE is low are R sq is high
#we can deploy above model

################################# Final Model ############################
#applying on the whole data
finalair <- lm(passenger_log~(t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec), data = newair)
summary(finalair)
# r sq = 98.29

#now we will forcast for next 12 months for forcasting purpose
#creatign dummeies for next 12 months 

x1<- data.frame(outer(rep(month.abb,length = 12), month.abb,"==") + 0 )
#renaming the column names
colnames(x1)<-month.abb
View(x1)
#creating time series index t
t <- 97:108

#binding them all
predicted_air <- cbind(x1,t)

#saving as dataframe
predict_air <- as.data.frame(predicted_air)
#previewing the new formed data
View(predict_air)

#now forcasting next 12 value by our final model
final_forcast <- predict(finalair, newdata = predict_air) 
#saving the forcasted values
#backtransforming the error as y was transformed
predict_air["forcast"] <- exp(final_forcast)
View(predict_air)

#saving the residuals of the new model
#to check the relaionship bet them
resid <- exp(residuals(finalair))
resid[1:10]
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
pred_res <- predict(k, ahead = 4)
str(pred_res)
#saving the errors
predict_air["forcasted error"] <- (pred_res$pred )
View(predict_air)
#adding the error for better forcasted values
predict_air["Fianl Forcast"] <- predict_air$forcast+predict_air$`forcasted error`
View(predict_air)

#ARIMA MODEL, takes p,d and q input respectively for AR, I & MA

#I value can be 0 and 1

#Finding q value for arima 
windows()
acf(air$Passengers, lag.max = 24)

#Finding p value for arima
windows()
pacf(air$Passengers, lag.max = 12)
# p= 10
