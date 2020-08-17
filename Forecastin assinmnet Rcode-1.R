library(readxl)
airlines <- read_excel("F:/Excelr/Datasets/Airlines+Data.xlsx")
View(airlines)
plot(airlines$Passengers,type = "o")
plot(log(airlines$Passengers),type = "o")
summary(airlines)
# creating 12 dummy variables for months
x<-data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )
View(x)
colnames(x) <- month.abb
View(x)
trackdata<-cbind(airlines,x)
head(trackdata)
colnames(trackdata)[2]<-"Passengers"
colnames(trackdata)
trackdata["t"]<- c(1:96)
View(trackdata)
head(trackdata)
colnames(trackdata)[2]<-"Passengers"
trackdata["log_passengers"]<-log(trackdata["Passengers"])
trackdata["t_square"]<-trackdata["t"]*trackdata["t"]
attach(trackdata)
head(trackdata)
#split data into train and test
train<-trackdata[1:68,]
test<-trackdata[69:96,]
#######1. Linear Model######
linear_model<-lm(Passengers~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
linear_pred
linear_model_rmse<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
linear_model_rmse
#######2.  Exponential Model######
expo_model<-lm(log_passengers~t,data = train)
expo_pred<-data.frame(predict(expo_model,interval = "predict",newdata = test))
summary(expo_model)
exp_model_rmse
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo
#######3.   Quadratic Model######
quad_model<-lm(Passengers~t+t_square,data = train)
summary(quad_model)
quad_pred<-data.frame(predict(quad_model,interval = "predict",newdata = test))
quad_pred<-data.frame(predict(quad_model,interval = "predict",newdata = test))
quad_rmse<-sqrt(mean((test$Passengers-quad_pred$fit)^2,na.rm=T))
quad_rmse
##########4.Additive seasonality######################
add_seas <-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(add_seas) 
add_seas_pred <- data.frame(predict(add_seas,interval = "predict",newdata = test))
add_seas_rmse <- sqrt(mean((test$Passengers-add_seas_pred$fit)^2,na.rm = T))
add_seas_rmse  
######5.Additive seasonality with linear############
add_seast <- lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(add_seast) 
add_seast_pred <- data.frame(predict(add_seast,interval = "predict",newdata = test))
add_seast_rmse <- sqrt(mean((test$Passengers-add_seast_pred$fit)^2,na.rm = T))
add_seast_rmse       
##### 6.	Additive seasonality with quadratic ###########
add_seasq <- lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(add_seasq)                     
add_seasq_pred <- data.frame(predict(add_seasq,interval = "predict",newdata = test))
add_seasq_rmse <- sqrt(mean((test$Passengers-add_seasq_pred$fit)^2,na.rm = T))
add_seasq_rmse 
######7.multiplicative seasonality#########
mul_seas_model <- lm(log_passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(mul_seas_model)         
mul_seas_pred <- data.frame(predict(mul_seas_model,interval = 'predict',newdata = test))
mul_seas_rmse <- sqrt(mean((test$Passengers-mul_seas_pred$fit)^2,na.rm = T))
mul_seas_rmse 
#######8.Multiplicative seasonality with linear##########
mul_seast_model <- lm(log_passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(mul_seast_model)                
mul_seast_pred <- data.frame(predict(mul_seast_model,interval = 'predict',newdata = test))
mul_seast_rmse <- sqrt(mean((test$Passengers-mul_seast_pred$fit)^2,na.rm = T))
mul_seast_rmse  
# showing all RMSE in table format
table_formate <- data.frame(c("linear_model_rmse","rmse_expo","quad_rmse","add_seas_rmse","add_seast_rmse","add_seasq_rmse","mul_seas_rmse","mul_seast_rmse"),c(linear_model_rmse,rmse_expo,quad_rmse,add_seas_rmse,add_seast_rmse,add_seasq_rmse,mul_seas_rmse,mul_seast_rmse))
colnames(table_formate) <- c("model","RMSE")
View(table_formate)
table_formate
# Final model
finalmodel <- lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = trackdata)
finalmodel
summary(finalmodel)
# Auto.arima method
install.packages("tseries")
library(tseries)
Airline_ts<-as.ts(airlines$Passengers)
Airline_ts<-ts(Airline_ts,start = c(1995,1),end = c(2002,12),frequency = 12)
class(Airline_ts)
start(Airline_ts)
end(Airline_ts)
sum(is.na(Airline_ts))
summary(Airline_ts)
decompdata<-decompose(Airline_ts,"multiplicative")
plot(decompdata)
cycle(Airline_ts)
boxplot(Airline_ts~cycle(Airline_ts))
#Model Building
newmodel <- auto.arima(Airline_ts,ic = "aic",trace = T)
newmodel
plot.ts(newmodel$residuals)
# Verifying p,d,q values using acf and pacf
acf(newmodel$residuals) 
pacf(newmodel$residuals)
acf(diff(newmodel$residuals))
#Forecasting the model
install.packages("forecast")
library(forecast)
forecasting <- forecast(newmodel,level = c(41),h=10*12)
plot(forecasting)
Box.test(newmodel$residuals,lag = 5,type = "Ljung-Box")
Box.test(newmodel$residuals,lag = 10,type ="Ljung-Box" )
