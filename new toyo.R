#uploading the data
library(readr)
toyo <-read.csv(choose.files())

#previewing the data
View(toyo)

#attaching for further use
attach(toyo)

################################## EDA #####################################

#removing the id no and converting necessary into numeric
toyo <- toyo[,-1]

#checking for null values 
table(is.na(toyo)) #no null value

#manufacturing year and other can be converted into factor
toyo$Mfg_Year <- as.factor(toyo$Mfg_Year)
toyo$HP <- as.factor(toyo$HP)
toyo$cc <- as.factor(toyo$cc)
#converting model, fuel type, color from factor to numeric
toyo$Model <- as.numeric(toyo$Model)
toyo$Fuel_Type <- as.numeric(toyo$Fuel_Type)
toyo$Color <- as.numeric(toyo$Color)
toyo$Mfg_Year <- as.numeric(toyo$Mfg_Year)

#checking for factor
is.factor(toyo)

#MEAN AND MEDIAN
summary(toyo)

#checking data distribution
hist(toyo$Model) 
boxplot(toyo$Model, horizontal = T) #have outliers

hist(toyo$Price) 
boxplot(toyo$Price, horizontal = T) #have outliers

hist(toyo$Age_08_04)
boxplot(toyo$Age_08_04 , horizontal=T) #have outliers in left

################### creating a model #####################################
###### y ~ all x #####
toyo_all <- m(Price~., data = toyo)

#checking the summary 
summary(toyo_all)
#r squ = 92.19

#rmse
rmse_all <- sqrt((mean((toyo_all$residuals)^2)))
rmse_all
#rmse = 1013.09

## checking the influencing values##
library(car)

influenceIndexPlot(toyo_all, id.n= 3) 
#222 observation is most influencing

influencePlot(toyo_all, id.n=3) #from both graph
#222 is most influencing

######## removing 222 and creating a model #########
 
toyo_all1<- lm(Price~., data = toyo[-c(222),])
#checking the summary
summary(toyo_all1)
#r squ = 92.54

#checking the rmse
rmse1 <- sqrt((mean((toyo_all1$residuals)^2)))
rmse1
#rmse = 990

#### further removing influncing values and creating model ####
toyo_all2<- lm(Price~., data = toyo[-c(81,222),])

#checking the summary
summary(toyo_all2)
#r squ = 92.52

#checking rmse
rmse2 <- sqrt((mean((toyo_all2$residuals)^2)))
rmse2
#rmse = 990.62

#it seems like rmse is slightly increased so we can only remove 222 observations

