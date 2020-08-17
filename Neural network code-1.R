library(corrplot)
library(GGally)
library(dplyr)
install.packages("neuralnet")
library(neuralnet)
library(nnet)
library(caret)
install.packages("NeuralNetTools")
library(NeuralNetTools)
startup <- read.csv("F:/Excelr/Datasets/50_Startups.csv")
View(startup)
summary(startup)
str(startup)
ggpairs(startup[,-4])
startup$statedummy[startup$State=="New York"] <- 0
startup$statedummy[startup$State=="California"] <-1
startup$statedummy[startup$State=="Florida"] <- 2
startup$statedummy <- as.numeric(startup$statedummy)
str(startup[,-4])
View(startup)
#Normalising the dataset
norm <- function(x){
       return((x-min(x))/(max(x)-min(x)))
     }
denorm <- function(x,min,max){
     return((max-min)*x+min)   
   }
data <- as.data.frame(lapply(startup[,-4],norm))
View(data)
head(data)
startup_train <- data[1:30,]
startup_test <- data[31:50,]
# Building model
formula_nn <- paste("profit",paste(colnames(startup[-4]),Marketing.Spend ="+"),Administration="~")
startup_model <- neuralnet(Profit~Marketing.Spend+Administration+R.D.Spend+statedummy,data = startup_train)
str(startup_model)
plot(startup_model)
# compute function to generate ouput for the model prepared
set.seed(12323)
model_results <- compute(startup_model,startup_test)
str(model_results)
predicted_profit <- model_results$net.result
cor(predicted_profit,startup_test$Profit)
plot(predicted_profit,startup_test$Profit)
plotnet(model,x_names = colnames(data[,-4]),y_names = colnames(data[,4]),cex=0.8)
max_s <- max(data$Profit)
min_s <- min(data$Profit) 
actualpred_profit <- denorm(predicted_profit,min = min_s,max = max_s)
actualpred_profit
model2 <- neuralnet(Profit~Marketing.Spend+Administration+R.D.Spend+statedummy,data = startup_train,hidden = 5,rep = 5)
plot(model2,"best")
plotnet(model2,cex=0.8)
str(model2)
model2result <- compute(model2,startup_test)
pred2profit <- model2result$net.result
cor(pred2profit,startup_test$Profit)  
