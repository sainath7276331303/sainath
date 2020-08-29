##loading the libraries
library("recommenderlab")
library(caTools)

#book rating data
library(readr)
book <- read_csv("C:/Users/Desktop/ExcelR/Assignment/Recommendation system/books.csv", 
                  col_types = cols(X1 = col_skip(), `users[, 1]` = col_skip()))View(book)

#meta data
str(book)
#rating distribution
hist(book$`ratings[, 3]`)
#in order to bulid recommendation the datatype should be realRatingMatrix 
?as
rate_matrix <- as(book, "realRatingMatrix")
rate_matrix<- as(book,"realRatingMatrix")


