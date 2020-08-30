library(rvest)
install.packages("XML")
library(XML)
library(magrittr)
install.packages("tm")
library(tm)
########################Review extraction###########################
url <- "https://www.amazon.in/Samsung-Galaxy-M30-Gradation-Blue/product-reviews/B07HGJJ58K/ref=cm_cr_arp_d_paging_btm_next_2?reviewerType=all_reviews"
Galaxy <- NULL
for(i in 1:10){
  murl <- read_html(as.character(paste(url,i,sep = "=")))
  rev <- murl%>%
    html_nodes(".review-text")%>%
    html_text()
  Galaxy <- c(Galaxy,rev)
}
write.table(Galaxy,"Galaxy.txt",row.names = F)
SamsungM30 <- unique(readLines("Galaxy.txt"))
df_SamsungM30 <- gsub(pattern = "http.*",replacement = "",x=SamsungM30)
df_SamsungM30 <- gsub("http.","",df_SamsungM30)
df_SamsungM30 <- gsub("#.*","",df_SamsungM30)
df_SamsungM30 <- gsub("@.*","",df_SamsungM30)
library(textcat)
table(textcat(df_SamsungM30))
df_SamsungM30[which(textcat(df_SamsungM30)=="norwegian")]
consider <- c(which(textcat(df_SamsungM30)!="norwegian"))
df_SamsungM30 <- df_SamsungM30[consider]
##############Loading the text to be analysed#########
##############Stopwords######################
stop <- readLines("C:/Users/akritipc/Documents/Galaxy.txt")
Galaxy_corp <- Corpus(VectorSource(df_SamsungM30))
Galaxy_corp <- tm_map(Galaxy_corp,removePunctuation)
Galaxy_corp <- tm_map(Galaxy_corp,removeWords,stop)
Galaxy_corp <- tm_map(Galaxy_corp,stripWhitespace)
Galaxy_tdm <- TermDocumentMatrix(Galaxy_corp)
#################Convert tdm to dtm####################
Galaxy_dtm <- t(Galaxy_tdm)
rowtotals <- apply(Galaxy_dtm,1,sum)
Galaxy_dtm2 <- Galaxy_dtm[rowtotals>3,]
Galaxy_dtm2$dimnames$Terms
install.packages("topicmodels")
library(topicmodels)
LDA
Galaxy_LDA <- LDA(x=Galaxy_dtm2,10)
Galaxy_LDA_terms <- terms(Galaxy_LDA,5)
Galaxy_LDA_terms
topics <- terms(Galaxy_LDA)
table <- table(names(topics),unlist(topics))
library(cluster)
library(dendextend)
cluster <- hclust(dist(table),method = "ward.D2")
colr <- color_branches(cluster,k=4)
plot(colr)
#####################NLP#########################
library(syuzhet)
Galaxy <- get_sentences(df_SamsungM30)
class(Galaxy)
###########################Sentiment analysis###########################
sentiment_vector <- get_sentiment(Galaxy, method = "bing")
head(sentiment_vector)
afinn_Galaxy <- get_sentiment(Galaxy, method = "afinn")
head(afinn_Galaxy)
sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)
plot(sentiment_vector, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")
#######################Plot for NRC########################
plot(sentimentlist$nrc,type = "l",main = "NRC plot")
abline(h=0,col ="red")
abline(h=1,col ="blue")
abline(h=2,col="yellow")
###################Sentences with negative emotion values###################
negative <- sentimentlist$nrc[which.min(sentimentlist$nrc)]
Galaxy[which(sentimentlist$nrc==negative)]
#####################Sentences with positive emotions values################
positive <- sentimentlist$nrc[which.max(sentimentlist$nrc)]
Galaxy[which(sentimentlist$nrc==positive)]
#################Emotion plot################
nrc_data <- get_nrc_sentiment(Galaxy)
barplot(sort(colSums(prop.table(nrc_data[,1:8]))),cex.names = 0.8,col = 1:8)
##################word cloud#############################
library(tm)
install.packages("SnowballC")
library(SnowballC)
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
freq <- rowSums(as.matrix(Galaxy_tdm))
length(freq)
head(freq)
data_frame <- data.frame(word=names(freq),freq=freq)
data_frame <- data_frame[-8,]
windows()
wordcloud(words = data_frame$word,freq = data_frame$freq,min.freq = 2,max.words = 200,random.order = F,colors = brewer.pal(10,"Dark2"))
findFreqTerms(Galaxy_dtm,lowfreq = 5)
findAssocs(Galaxy_dtm,terms = "fast",corlimit = 0.3)
head(data_frame,10)
barplot(data_frame[(1:11),]$freq,names.arg = data_frame[(1:11),]$word,col = "navyblue")