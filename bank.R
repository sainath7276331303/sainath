#uploading the data
library(readr)
bank <- read.csv("C:/Users/Desktop/ExcelR/Assignment/Logistic Regression/bank-full.csv", sep = ';')
#attaching for further use
attach(bank)

##################################### EDA #############################################

#checking for null values
table(is.na(bank))
#Measuring central tendencies#
summary(bank)
# in marital married have more observaiton
# seconday has more no of observation in education
# no in default
# no in loan
# cellular in contact 
# may in month
# unknown in poutcome
# no in y

#checking data distribution for others
hist(bank$balance) #highly skewed data
boxplot(balance, horizontal = T) #too many outliers

hist(pdays) #highly skewed data
boxplot(pdays, horizontal = T) #too many outliers

hist(previous) #highly skewed data
boxplot(previous, horizontal = T) #too many outliers

table(bank$day)
table(bank$month)
table(y)
table(poutcome)

################# Creating Dummies #####################
library(mlr)
jobx <- createDummyFeatures(bank$job)
maritalx <- createDummyFeatures(bank$marital)
edu <- createDummyFeatures(bank$education)
defaultx <- createDummyFeatures(bank$default) 
house <- createDummyFeatures(bank$housing)
loanx <- createDummyFeatures(bank$loan)
contactx <- createDummyFeatures(bank$contact)
month <- createDummyFeatures(bank$month)
pout <- createDummyFeatures(bank$poutcome)

## Creating data frame for model building##
library(dplyr)

# removing the columns for which we created dummies
newbank1 <- select(bank, -c(job, marital,education,default,housing,loan,contact,month, poutcome))
#combining the dummies together
newbank2 <- data.frame(jobx,maritalx,edu,defaultx,house,loanx,contactx,month,pout)
#bankx <- data.frame(bank("job","marital","education","default","housing","loan","contact","month"),newbank)

#creating new data base by combining dummies and remaining data points
bankx <- data.frame(newbank1,newbank2)
#checking the summary
summary(bankx)
View(bankx)
###################### creating a model ##########################
#for logistic regg we have glm 
?glm
glm_bank_all <- glm(y~., family = 'binomial', data = bankx)
#checking the summary
summary(glm_bank_all)
#residual deviance should be lesser than null deviance
#aic = 21648

#influencing inputs is given by
exp(coef(glm_bank_all)) 

### storing the fitted values ###
#fitted values are
glm_bank_all$fitted.values
#fitted values are nothing but probablity associated with class 1
prob_all <- glm_bank_all$fitted.values                                        

#predicting values based on our model
?predict
#prob <- predict(glm_bank_all, type = c("response"), bankx )
prob <- predict(glm_bank_all,bankx,type="response")

#confusin matrix and giving the limit as 0.5 randomly

confmat <- table(prob_all >0.5, bankx$y)
confmat
#the original values are
table(bank$y)

#accuracy of the model is given by
accu <- sum(diag(confmat))/sum(confmat)
accu

## for better accuracy we need a accurate cut off value

##changing the cut off value##
library("ROCR")
rocplot <- prediction(prob, bank$y)
rocrpred <- prediction(prob_all, bankx$y)
rocrperf <- performance(rocrpred,'tpr','fpr')

##ploting rocr curve##
windows()
plot(rocrperf,colorize = T, text.adj=c(-0.2,1.7), print.cutoffs.at = seq(0.1, by=0.1) )

##the cutoff value lies between 0.1 and 0.2##

#Data Frame for cutoff values and tpr-fpr
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr = rocrperf@x.values,tpr= rocrperf@y.values)

#column names for the dataframe
colnames(rocr_cutoff)<- c("cutoff","FPR","TPR")

#Rounding values to two decimals
rocr_cutoff <- round(rocr_cutoff,2)

#arrange tpr#
library(dplyr)
rocr_cutoff<- arrange(rocr_cutoff, desc(TPR))
View(rocr_cutoff)

##taking the cutoff value as 0.1-0.2
#confusin matrix
?range()
#x <- seq(0.1, 0.2, by=0.01)
#accu1 <- c()
#for i in x
 # { confu1 <- table(prob_all >i, bankx$y)
  #  accu1 <- sum(diag(confu1))/ sum(confu1)
  #}

confu1

#the original
table(bank$y)

#accuracy is given by
accu1 <- sum(diag(confu1))/ sum(confu1)
accu1
#accuracy = 84.81 %

##AUC curve##
#install.packages("pROC")
library(pROC)
?performance
auc <- performance(rocrpred, measure = "auc")
auc <- auc@y.values[[1]]
auc #91 area under the curve
#model is good enough to deploy
