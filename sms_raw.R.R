# uploading the data
sms_raw <- read.csv(choose.files())
str(sms_raw)

# for text mining we will need

# install.packages("tm")
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))
sms_corpus$content[1:10]

# preparing a clean corpus

sms_corpus_clean <- tm_map(sms_corpus,tolower)
sms_corpus_clean <- tm_map(sms_corpus_clean,removeNumbers)
sms_corpus_clean<-tm_map(sms_corpus_clean, removeWords,stopwords())
sms_corpus_clean<-tm_map(sms_corpus_clean,removePunctuation)

# creating a fun for punc removal
removeNumPunc <-function(x)  gsub("[^[:alpha:][:space:]]*","",x)
corpus_clean <- tm_map(sms_corpus_clean,content_transformer(removeNumPunc))
corpus_clean<-tm_map(sms_corpus_clean,stripWhitespace)

# checking the contain
corpus_clean$content[1:10]

# creating Document term matrix(DTM)

sms_corpus_dtm<-DocumentTermMatrix(sms_corpus_clean)

# number of documnets
sms_corpus_dtm$nrow

#number of words after cleaning the raw corpus
sms_corpus_dtm$ncol

# prepairing train and test data from original sms raw
# to get the classification column 
train_sms_raw<-sms_raw[1:4250,]      
test_sms_raw<-sms_raw[4251:5559,]

# creating training and testing on DTM
train_sms_dtm <- sms_corpus_dtm[1:4250,]
test_sms_dtm <- sms_corpus_dtm[4251:5559,]

# creating training and testing on DTM
train_corpus<- corpus_clean[1:4250]
test_corpus<-corpus_clean[4251:5559]


# checking the proptions ham and spam in the train and test dataset
prop.table(table(train_sms_raw$type))
prop.table(table(test_sms_raw$type))
# almost same

# created a dictionary with words repeating more than 3 times in the dtm 
sms_dict <- findFreqTerms(train_sms_dtm,3)

# checking the first 100 words
list(sms_dict[1:100])

# creating a new dtm from the cleaned corpus with the words whose frequency is >3
sms_train <- DocumentTermMatrix(train_corpus,list(dictionary=sms_dict))
sms_test <- DocumentTermMatrix(test_corpus,list(disctionary=sms_dict))

# a function which will checks if word is there in the dtm then 1 else 0 frequency value doent matter and converting them into factors, giving them labels also. 
convert_count<-function(x){
  x<-ifelse(x>0,1,0)
  x<-factor(x,levels = c(0,1),labels = c("NO","YES"))
}

# applying the convert_count function to get final sms_train and sms_test
sms_train <- apply(sms_train,MARGIN = 2,convert_count)
sms_test <- apply(sms_test,MARGIN=2,convert_count)


################################## naivebayes  model with laplace smoothing ###########################

library(e1071)
model <- naiveBayes(sms_train , train_sms_raw$type,laplace = 15)

# predicting on same data

pred_train<-predict(model,newdata = sms_train)        

# checking the accuracy for train data
mean(pred_train==train_sms_raw$type)   
table(pred_train,train_sms_raw$type)

# predicting on test data

pred_test<-predict(model,newdata = sms_test)

# accuracy for test data
mean(pred_test==test_sms_raw$type)
table(pred_test,test_sms_raw$type)

################### naive bayes with multinomial #################
################### the accuracy was same        #################

################### checking for multinominal    #################
library(e1071)
model2 <- naiveBayes(sms_train , train_sms_raw$type,laplace = 11,family= "Multinomial")

# predicting on same data

pred_train2<-predict(model2,newdata = sms_train)        

# checking the accuracy for train data
mean(pred_train2==train_sms_raw$type)   
table(pred_train2,train_sms_raw$type)

# predicting on test data

pred_test2 <- predict(model2,newdata = sms_test)

# accuracy for test data
mean(pred_test2==test_sms_raw$type)
table(pred_test2,test_sms_raw$type)
