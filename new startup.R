##impoting the data##

library(readr)
startups <- read_csv("C:/Users/Desktop/ExcelR/Assignment/Multi Linear Regg/50_Startups.csv")
#previewing the data
View(startups)

###################################### EDA ####################################

#checking for null values
table(is.na(startups))
#there are no null values in our data

#data distributions for every variable
hist(startups$`R&DSpend`)
hist(startups$Administration) #seems to have outliers
hist(startups$`Marketing Spend`)
hist(startups$Profit)

#state are in character format converting them into factors
startups$State <- as.factor(startups$State)
#for distribution of factor we have 
table(startups$State)
#they are distributed normally

#converting them into numeric
startups$State <- as.numeric(startups$State)

#checking for outliers
boxplot(startups$`R&DSpend` , horizontal = T) #no outliers
boxplot(startups$Administration , horizontal = T) #no outliers
boxplot(startups$`Marketing Spend` , horizontal = T) #no outliers
boxplot(startups$Profit , horizontal = T) #no outliers

#attaching the file for further use
attach(startups)

#checking mean and median
summary(startups)

#checking the corelation between different variable

##Ploting for 2 variable##

#market spend and profit
plot(`Marketing Spend`, Profit)
#checking corelation bet them
cor(`Marketing Spend`,Profit) #74.77%

#transforming and checking the coreltion

#sqrt of market spend and profit
plot(sqrt(`Marketing Spend`) , Profit)
#corelationship between them
cor(sqrt(`Marketing Spend`), Profit) #69.15%

#log of market spend and profit
plot(log(`Marketing Spend`), Profit)
#as the plot suggest they have poor relationship

#plotting marketing spend with different inputs
plot(`Marketing Spend`, `R&DSpend`) 
#they are strongly corelated which is a problem
cor(`Marketing Spend`,`R&DSpend`) #72.42 %

################################# MODEL DEVELOPMENT ############################

## y=profit##

##Grphical representation##
#all graph in one
pairs(startups) 
#confusing as no names 
#we will use different plot

## For all graph in one with corelation##
#install.packages("GGally")
#install.packages("stringi")
library(GGally)

windows()
ggpairs(startups)
##### INSIGHTS ######
#R&D spend and profit have 97.3% corelation
#but R&D also have corelation with marketing spend
#state and adminsitration seems to have very less(PTO)
#coreltion with profit.

#we can check corelation by
## corelation between them##
cor(startups)

##pure corelation##
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(startups))
#it seems after removing the influence of other inputs
#profit and r&d spend are closly related

#################################### y ~ all (x) #######################################

##creating a simple multiple model##

startup_All <- lm(startups$Profit~., data = startups)
#checking the summary 
summary(startup_All)
r_squ_all = 95.07

##RMSE##
rmse_all <- sqrt(mean((startup_All$residuals)^2))
rmse_all
# RMSE = 8855.32

#there can be influencing value due to which our model may be effecting
##checking the influncers##

library(car)
influence.measures(startup_All) 
#no insight can be drawn from below

#checking for top 3 influencing values
windows()
influenceIndexPlot(startup_All, id.n = 3) 
#49 and 50 observation are most influencing

influencePlot(startup_All, id.n= 3)

##removing influence values##
#removing only 50 for now
startup1 <- lm(Profit~.,data = startups[-c(50),])
#checking the summary
summary(startup1)
r_squ1 = 96.18
#r squ = 96.18 
#r squ is improved

##checking the RMSE##
rmse_all1 <- sqrt(mean((startup1$residuals)^2))
rmse_all1
#rmse = 7388.08

#now removing both and buliding a model
##removing 49 and 50 influence values##
startup2 <- lm(Profit~.,data = startups[-c(50,49),])
#checking the summary
summary(startup2)
r_squ2 = 96.27
#r squ = 96.27 %

## checking the RMSE##
rmse_all2 <-sqrt(mean((startup2$residuals)^2))
rmse_all2
# rmse = 7031.52

##removing the 3 rd influence values##
startup3 <- lm(Profit~.,data = startups[-c(50,49,47),])
#checking the summarty
summary(startup3)
r_squ3 = 96.15
# r squ = 96.15

##checking the RMSE##
rmse_all3<-  sqrt(mean((startup3$residuals)^2))
rmse_all3
#rmse= 6967.53

#as r squ and rmse of third model is not that different 
#we will only remove 2 influncing values

#as input variable have colinearlity problem
#checking the influencing input by:
##VIF value##
#install.packages("car")
library(car)
?vif()
vif(startup2)
#### Insight ###
## R&D is causing the problem

#we can check by plotting the inputs
windows()
avPlots(startup2)
#but by graph it seems like state have less corelation with profit
#we can remove and check 

##creating model with 2 variables and removing 49 and 50 observaitons##

startup4 <- lm(Profit ~ `R&DSpend`+`Marketing Spend`+Administration, data = startups[-c(50,49),])
#checking the summary
summary(startup4)
r_squ4 <- 96.27

##RMSE##
rmse_4 <- sqrt(mean((startup4$residuals)^2))
rmse_4

#checking the influncing input of above model
vif(startup4)
#plotting the graph
windows()
avPlots(startup4)
#marketing spend and administration both are having same 

#trying on at at time and creating a model by removing 49 and 50

startup5 <- lm( Profit~ Administration+ `R&DSpend`, data = startups[-c(50,49),])
#checking the summary
summary(startup5)
r_squ5 <- 96.14

##checking the RMSE##
rmse_5 <- sqrt(mean((startup5$residuals)^2))

#checking the influencing input
vif(startup5)
#checking the plots
windows()
avPlots(startup5)
#by graph and vif no we can see they are strongly related

#checking it with markting spend and r&d spend and removing 49 and 50
startup6 <- lm(Profit~ `Marketing Spend`+`R&DSpend`, data = startups[-c(50,49),])
#checking summary
summary(startup6)
r_squ6 = 96.09
## checking RMSE##
rmse_6 <- sqrt(mean((startup6$residuals)^2))
rmse_6

#making table of rmse and r squ

table <- data.frame(c("r_squ_all","r_squ1","r_squ2","r_squ3","r_squ4","r_squ5","rsqu6"),c(r_squ_all, r_squ1, r_squ2, r_squ3,r_squ4, r_squ5,r_squ6))
table["RMSE"] <- c(rmse_all, rmse_all1,rmse_all2,rmse_all3,rmse_4,rmse_5,rmse_6)
colnames(table) <- c("model","Value","RMSE")

#from above table we can observe model 3 is having good r squ and less rmse
#we can use it to deploy the model