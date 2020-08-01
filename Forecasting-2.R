library(readxl)
cocacola <- read_excel("F:/Excelr/Datasets/CocaCola_Sales_Rawdata.xlsx")
View(cocacola)
plot(cocacola$Sales,type = "o")
plot(log(cocacola$Sales),type= "o")
summary(cocacola)
# creating 12 dummy variables for months
x<-data.frame(outer(rep(month.abb,length = 42), month.abb,"==") + 0 )
View(x)
colnames(x) <- month.abb
View(x)
data<-cbind(cocacola,x)
head(data)
colnames(data[2])<- "sales"
colnames(data)
data["t"]<- c(1:42)
View(data)
head(data)
colnames(data)[2] <- "sales"
data["log_sales"] <- log(data["sales"])
data["t_square"]<-data["t"]*data["t"]
attach(data)
head(data)
#split data into train and test
train<-data[1:30,]
test<-data[31:42,]
#######1. Linear Model######
linear_model<-lm(sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
linear_pred
linear_model_rmse<-sqrt(mean((test$sales-linear_pred$fit)^2,na.rm = T))
linear_model_rmse
#######2.  Exponential Model######
expo_model<-lm(log_sales~t,data = train)
expo_pred<-data.frame(predict(expo_model,interval = "predict",newdata = test))
summary(expo_model)
expo_model
rmse_expo<-sqrt(mean((test$sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo
#######3.   Quadratic Model######
quad_model<-lm(sales~t+t_square,data = train)
summary(quad_model)
quad_pred<-data.frame(predict(quad_model,interval = "predict",newdata = test))
quad_pred<-data.frame(predict(quad_model,interval = "predict",newdata = test))
quad_rmse<-sqrt(mean((test$sales-quad_pred$fit)^2,na.rm=T))
quad_rmse
##########4.Additive seasonality######################
add_seas <-lm(sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(add_seas) 
add_seas_pred <- data.frame(predict(add_seas,interval = "predict",newdata = test))
add_seas_rmse <- sqrt(mean((test$sales-add_seas_pred$fit)^2,na.rm = T))
add_seas_rmse  
######5.Additive seasonality with linear############
add_seast <- lm(sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(add_seast) 
add_seast_pred <- data.frame(predict(add_seast,interval = "predict",newdata = test))
add_seast_rmse <- sqrt(mean((test$sales-add_seast_pred$fit)^2,na.rm = T))
add_seast_rmse       
##### 6.	Additive seasonality with quadratic ###########
add_seasq <- lm(sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(add_seasq)                     
add_seasq_pred <- data.frame(predict(add_seasq,interval = "predict",newdata = test))
add_seasq_rmse <- sqrt(mean((test$sales-add_seasq_pred$fit)^2,na.rm = T))
add_seasq_rmse 
######7.multiplicative seasonality#########
mul_seas_model <- lm(log_sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(mul_seas_model)         
mul_seas_pred <- data.frame(predict(mul_seas_model,interval = 'predict',newdata = test))
mul_seas_rmse <- sqrt(mean((test$sales-mul_seas_pred$fit)^2,na.rm = T))
mul_seas_rmse 
#######8.Multiplicative seasonality with linear##########
mul_seast_model <- lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(mul_seast_model)                
mul_seast_pred <- data.frame(predict(mul_seast_model,interval = 'predict',newdata = test))
mul_seast_rmse <- sqrt(mean((test$sales-mul_seast_pred$fit)^2,na.rm = T))
mul_seast_rmse  
# showing all RMSE in table format
table_formate <- data.frame(c("linear_model_rmse","rmse_expo","quad_rmse","add_seas_rmse","add_seast_rmse","add_seasq_rmse","mul_seas_rmse","mul_seast_rmse"),c(linear_model_rmse,rmse_expo,quad_rmse,add_seas_rmse,add_seast_rmse,add_seasq_rmse,mul_seas_rmse,mul_seast_rmse))
colnames(table_formate) <- c("model","RMSE")
View(table_formate)
table_formate
# Final model
finalmodel <- lm(sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = data)
finalmodel
summary(finalmodel)
# Auto.arima method
install.packages("tseries")
library(tseries)
cocacola_ts<-as.ts(cocacola$Sales)
cocacola_ts<-ts(cocacola_ts,start = c(1986,1),end = c(1996,12),frequency = 12)
class(cocacola_ts)
start(cocacola_ts)
end(cocacola_ts)
sum(is.na(cocacola_ts))
summary(cocacola_ts)
decompdata<-decompose(cocacola_ts,"multiplicative")
plot(decompdata)
cycle(cocacola_ts)
boxplot(cocacola_ts~cycle(cocacola_ts))
#Model Building
newmodel <- newmodel <- auto.arima(cocacola_ts,ic = "aic",trace = T)
newmodel
plot.ts(newmodel$residuals)
# Verifying p,d,q values using acf and pacf
acf(newmodel$residuals) 
pacf(newmodel$residuals)
acf(diff(newmodel$residuals))
#Forecasting the model
install.packages("forecast")
library(forecast)
forecasting <- forecast(newmodel,level = c(95),h=10*12)
plot(forecasting)
Box.test(newmodel$residuals,lag = 5,type = "Ljung-Box")
Box.test(newmodel$residuals,lag = 10,type ="Ljung-Box" )
