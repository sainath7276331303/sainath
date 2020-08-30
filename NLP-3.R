library(rvest)
install.packages("XML")
library(XML)
library(magrittr)
url <- "https://www.imdb.com/title/tt0068646/reviews?ref_=ttls_li_tt"
IMDB_godfather <- NULL
for(i in 1:10){
  murl <- read_html(as.character(paste(url,i,sep = "=")))
  rev <- murl%>%
    html_nodes(".show-more__control")%>%
    html_text()
  IMDB_godfather <- c(IMDB_godfather,rev)
}
length(IMDB_godfather)
length(unique(IMDB_godfather))
IMDB_godfather[duplicated(IMDB_godfather)]
write.table(unique(IMDB_godfather),"godfather.txt",row.names = F)
godfather <- readLines("godfather.txt")
df_godfather <- gsub(pattern = "http.*",replacement = "",x=godfather)
df_godfather <- gsub("https.*","",df_godfather)
df_godfather <- gsub('\""','',df_godfather)
library(textcat)
table(textcat(df_godfather))
df_godfather[which(textcat(df_godfather)=="norwegian")]
consider <- c(which(textcat(df_godfather)!="norwegian"))
df_godfather <- df_godfather[consider]
length(df_godfather)
##################Text mining#########################
library(tm)
library(NLP)
stopword <- readLines("C:/Users/akritipc/Documents/godfather.txt")
View(stopword)
#############Removing stop words###############
godfather_corp <- Corpus(VectorSource(df_godfather))
godfather_corp <- tm_map(godfather_corp,removePunctuation)
godfather_corp <- tm_map(godfather_corp,removeNumbers)
godfather_corp <- tm_map(godfather_corp,stripWhitespace)
################Creating term document matrix#####################
godfather_tdm <- TermDocumentMatrix(godfather_corp)
godfather_dtm <- t(godfather_tdm)
rowtotal <- apply(godfather_dtm,1,sum)
godfather_dtm2 <- godfather_dtm[rowtotal>3,]
godfather_dtm2$dimnames$Terms
######################LDA######################
library(topicmodels)
godfather_LDA <- LDA(x=godfather_dtm2,10)
godfather_LDA_terms <- terms(godfather_LDA,5)
godfather_LDA_terms
topic <- terms(godfather_LDA)
table <- table(names(topic),unlist(topic))
head(table)
library(cluster)
library(dendextend)
cluster <- hclust(dist(table),method = "ward.D2")
colr <-color_branches(cluster,k=3) 
plot(colr)
###################NLP################################
library(syuzhet)
godfather <- get_sentences(df_godfather)
class(godfather)
########################Sentiment analysis#############################
sentiments <- c("syuzhet","afinn","bing","nrc","stanford","custom")
A <- NULL
sentimentlist <- NULL
for(i in sentiments[1:4]){
  sentimentlist[[i]] <- get_sentiment(godfather,method = i)
  A[[i]] <- table(get_sentiment(godfather,method = i))
}
A
sentiments
###############Plot for NRC#############
plot(sentimentlist$nrc,type = "l",main = "NRC plot",xlab = "Time",ylab = "Emotion")
abline(h=0,col ="red",lwd=2)
abline(h=1,col ="blue",lwd =1)
abline(h=2,col ="yellow",lwd=1)
abline(h=4,col ="green",lwd=1)
#########To extract the sentences with most negative emotions#########
negative <- sentimentlist$bing[which.min(sentimentlist$bing)]
godfather[which(sentimentlist$bing==negative)]
#######To extract the sentences with most positive emotions#########
positive <- sentimentlist$bing[which.max(sentimentlist$bing)]
godfather[which(sentimentlist$bing==positive)]
############Percent based fig#######
percentvalues <- get_percentage_values(sentimentlist$bing)
plot(percentvalues,type = "l",main = "percentage baced means",xlab = "TIME",ylab = "emotion value",col ="blue")
ft_values <- get_transformed_values(sentimentlist$bing,low_pass_size = 2,x_reverse_len = 100,scale_vals = T,scale_range = F)
plot(ft_values,col =ifelse(ft_values>0,"red","blue"))
#################Emotion plot############
nrc_data <- get_nrc_sentiment(godfather)
barplot(sort(colSums(prop.table(nrc_data))),horiz = F,cex.names = 0.8,col = 1:8)
#############Word cloud###########
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
freq <- (rowSums(as.matrix(godfather_tdm)))
length(freq)
ord <- order(freq,decreasing = TRUE)
head(freq)
data_frame <- data.frame(word =names(freq),freq= freq)
windows()
wordcloud(words = data_frame$word,freq = data_frame$freq,min.freq = 3,max.words = 150,random.order = F,colors = brewer.pal(10,"Dark2"))
findFreqTerms(godfather_dtm,lowfreq = 5)
findAssocs(godfather_dtm,terms = "godfather",corlimit = 0.2)
head(data_frame,10)
barplot(data_frame[1:10,]$freq,names.arg = data_frame[1:10,]$word,col = "navyblue")