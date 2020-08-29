library(readr)
credit_card <- read.csv("C:/Users/akritipc/Downloads/creditcard.csv")
View(credit_card)
summary(credit_card)
str(credit_card)
sum(is.na(credit_card))
credit_card <- na.omit(credit_card)
dim(credit_card)
colnames(credit_card)
credit_card <- credit_card[,-1]
mod_lm <- lm(majorcards ~. ,data = credit_card)
pred1 <- predict(mod_lm,credit_card)
pred1
plot(credit_card$reports,pred1)
plot(pred1)
model <- glm(majorcards ~.,data = credit_card,family = "binomial")
exp(coef(model))
prob <- predict(model,credit_card,type="response")
summary(model)
confusion <- table(prob>0.5,credit_card$reports)
confusion
Accuracy <- sum(diag(confusion)/sum(confusion))
Accuracy
pred_values <- NULL
yes_no <- NULL
pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")
credit_card[,"prob"] <- prob
credit_card[,"pred_values"] <- pred_values
credit_card[,"yes_no"] <- yes_no
View(credit_card[,c(1,7:9)])
table(credit_card$reports,credit_card$pred_values)
install.packages("ROCR")
library(ROCR)
rocrpred <- prediction(prob,credit_card$majorcards)
rocrperf <- performance(rocrpred,'tpr','fpr')
str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)
head(rocr_cutoff)
install.packages("dplyr")
library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
head(rocr_cutoff,n = 15)
