#uploding the file
computer<-read.csv(choose.files())

#previewing the data
View(computer)

################################### EDA ###################################

#removing the unnessary columns
computer<-computer[,2:11]

#checking null values
table(is.na(computer))

#attaching for further use
attach(computer)

#checking the structure
str(computer)

#checking the summary
summary(computer)
#data is highly baised towards no in multi and yes in premium

#checking the distribution of data
hist(price) #normally distributed
boxplot(price, horizontal =T) #there are many outliers

hist(speed) #not normally distributed 
boxplot(speed, horizontal =T) #with no outliers

hist(hd) #positively skewed data
boxplot(hd, horizontal =T) #too many outliers

hist(ram) #not normally distributed
boxplot(ram, horizontal = T) #with outliers

hist(screen) #not normally distributed
boxplot(screen, horizontal = T) #with outliers

table(cd) # normally distributed
table(multi) #not normally distributed
table(premium) #not normally distributed

hist(ads) #normally distributed
boxplot(ads, horizontal = T) #with no outliers

####### creating factor to numerical where ever required ########

computer$cd<-as.integer(computer$cd)
computer$multi<-as.integer(computer$multi)
computer$premium<-as.integer(computer$premium)

#checking the structure
str(computer)

#checking the corelation
cor(computer)

############################# creating a model ###########################
modelc0<-lm(computer$price~.,data = computer)

#checking the summary
summary(modelc0)
#r squ = 77.56

#checking RMSE
rmse1 <- sqrt(mean((modelc0$residuals)^2))
rmse1
#rmse = 275.13

#checking the influncing inputs
library("car")
vif(modelc0)

#the influencers are given by
influencePlot(modelc0)
influenceIndexPlot(modelc0)
#1441 & 1701 are influencers
#removing them and creating new model

modelc1<-lm(sqrt(price)~.,data = computer[-c(1441,1701),])
#checking the summary
summary(modelc1)
#r squ = 78.64
rmse2  <- sqrt(mean((modelc1$residuals)^2))
#rmse = 2.79

#as model 2 have better rmse and more rsqu value we can deploy our model