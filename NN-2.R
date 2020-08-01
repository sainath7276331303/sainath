forestfire <- read.csv("F:/Excelr/Datasets/forestfires (2).csv")
View(forestfire)
library(corrplot)
library(GGally)
library(dplyr)
install.packages("neuralnet")
library(neuralnet)
library(nnet)
library(caret)
install.packages("NeuralNetTools")
library(NeuralNetTools)
summary(forestfire)
str(forestfire)
ggpairs(forestfire[-31])
forestfire$size_categorydummy[forestfire$size_category=="small"] <- 0
forestfire$size_categorydummy[forestfire$size_category=="large"] <-1
forestfire$size_categorydummy <- as.numeric(forestfire$size_categorydummy)
str(forestfire[-31])
View(forestfire[-31])
forestfire_train <- forestfire[1:300,]
forestfire_test <- forestfire[301:517,]
forest_model <- neuralnet(area~DC+ISI+wind+FFMC+DMC+RH+rain+temp+size_categorydummy,data = forestfire_train)
str(forest_model)
plot(forest_model)
# compute function to generate ouput for the model prepared
set.seed(12323)
model_results <- compute(forest_model,forestfire_test)
str(model_results)
predicted_area <- model_results$net.result
cor(predicted_area,forestfire_test$area)
plot(predicted_area,forestfire_test$area)
max_s <- max(forestfire$area)
min_s <- min(forestfire$area)
actualpred_profit <- denorm(predicted_area,min = min_s,max = max_s)
actualpred_profit
model2 <- neuralnet(area~DC+ISI+wind+FFMC+DMC+RH+rain+temp+size_categorydummy,data = forestfire_train,hidden= 5,rep = 5)
plot(model2,"best")
plotnet(model2,cex=0.8)
str(model2)
model2result <- compute(model2,forestfire_test)
pred2area <- model2result$net.result
cor(pred2area,forestfire_test$area)  

