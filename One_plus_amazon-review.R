#libraries for text mining
#install.packages("rapportools")
library(rvest)
library(XML)
library(magrittr)
library(rapportools)
#One Plus 7 Amazon Reviews 
aurl <- "https://www.amazon.in/OnePlus-Glacier-Display-Storage-3800mAH/product-reviews/B07DJHXTLJ/ref=cm_cr_arp_d_paging_btm_next"
oneplus_reviews <- NULL

for (i in 1:20){
  
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  
  rev <- murl %>%
    
    html_nodes(".review-data") %>%
    
    html_text()
  
  if (i==1) cat("\nrev : ",rev)
  
  oneplus_reviews <- c(oneplus_reviews, trim.space(rev))
  
}

#writing the above file
write.table(oneplus_reviews,'Oneplus.txt')
getwd()

#symentic ananlysis
#installing the packages
#("rJava")  # download Java 64 bit before installing the package
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("wordcloud")
#install.packages("RWeka")
#install.packages("textir")
#install.packages("igraph")
#install.packages("qdap")
#install.packages("maptpx")

#invocking the packages
library(rJava)
library(tm)		# load all required packages, install if not loaded
library(SnowballC)
library(wordcloud)

library(RWeka)	
library(qdap)		
library(textir)
library(maptpx)
library(data.table)
library(stringr)
library(slam)

library(ggplot2)

makewordc = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  windows()
  wordcloud(freq.df$word[1:120], freq.df$freq[1:120],scale = c(4,.5),random.order = F, colors=1:10)
} 

# Making positive wordcloud function 
makeposwordc = function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching positive words
  pos.matches = match(names(freq), c(pos.words,"approvals"))
  pos.matches = !is.na(pos.matches)
  freq_pos <- freq[pos.matches]
  names <- names(freq_pos)
  windows()
  wordcloud(names,freq_pos,scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}

# Making negatice wordcloud function
makenegwordc = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching positive words
  neg.matches = match(names(freq), neg.words)
  neg.matches = !is.na(neg.matches)
  freq_neg <- freq[neg.matches]
  names <- names(freq_neg)
  windows()
  wordcloud(names[1:120],freq_neg[1:120],scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}




words_bar_plot <- function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  head(freq.df, 20)
  library(ggplot2)
  windows()
  ggplot(head(freq.df,50), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Words") + ylab("Frequency") +
    ggtitle("Most frequent words")
  
}

pos_words_bar_plot <- function(x){
  pos.matches = match(colnames(x), pos.words)
  pos.matches = !is.na(pos.matches)
  pos_words_freq = as.data.frame(apply(x, 2, sum)[pos.matches])
  colnames(pos_words_freq)<-"freq"
  pos_words_freq["word"] <- rownames(pos_words_freq)
  # Sorting the words in deceasing order of their frequency
  pos_words_freq <- pos_words_freq[order(pos_words_freq$freq,decreasing=T),]
  windows()
  ggplot(head(pos_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Positive words") + ylab("Frequency") +
    ggtitle("Most frequent positive words")
}
neg_words_bar_plot <- function(x){
  neg.matches = match(colnames(x), neg.words)
  neg.matches = !is.na(neg.matches)
  neg_words_freq = as.data.frame(apply(x, 2, sum)[neg.matches])
  colnames(neg_words_freq)<-"freq"
  neg_words_freq["word"] <- rownames(neg_words_freq)
  # Sorting the words in deceasing order of their frequency
  neg_words_freq <- neg_words_freq[order(neg_words_freq$freq,decreasing=T),]
  windows()
  ggplot(head(neg_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("words") + ylab("Frequency") +
    ggtitle("Most frequent negative words")
}


# --- func to make cluster dendograms --- #
clusdend = function(a){	# writing func clusdend() 	
  mydata.df = as.data.frame(inspect(a));	
  mydata1.df = mydata.df[, order(-colSums(mydata.df))];
  min1 = min(ncol(mydata.df), 40) 	# minimum dimn of dist matrix
  test = matrix(0,min1,min1)
  test1 = test
  for(i1 in 1:(min1-1)){ 
    for(i2 in i1:min1){
      test = sum(mydata1.df[ ,i1]-mydata1.df[ ,i2])^2
      test1[i1,i2] = test; test1[i2, i1] = test1[i1, i2] 	}
  }
  # making dissimilarity matrix out of the freq one
  test2 = test1
  rownames(test2) = colnames(mydata1.df)[1:min1]
  # now plot collocation dendogram
  d <- dist(test2, method = "euclidean") # distance matrix
  fit <- hclust(d, method="ward")
  windows()
  plot(fit) # display dendogram
} # clusdend() func ends

# lOADING +VE AND -VE words  
pos.words=scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words=scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
stopwdrds = readLines(file.choose())

# Loading the text file to be analysed 
x = readLines(file.choose())

# Preparing corpus from the text document 
x1 = Corpus(VectorSource(x))  	# Constructs a source for a vector as input

x1 = tm_map(x1, stripWhitespace) 	# removes white space

x1 = tm_map(x1, tolower)		# converts to lower case
x1 = tm_map(x1, removePunctuation)	# removes punctuation marks
x1 = tm_map(x1, removeNumbers)		# removes numbers in the documents
x1 = tm_map(x1, removeWords, c(stopwords("english"),stopwdrds))

# Term document frequency matrix
tdm0 <- TermDocumentMatrix(x1)

# Term document matrix with inverse frequency 
tdm1 <- TermDocumentMatrix(x1,control = list(weighting = function(p) weightTfIdf(p,normalize = T)))#,stemming=T))
inspect(tdm1)

# getting the indexes of documents having count of words = 0
a0 <- NULL
a1 <- NULL

for (i1 in 1:ncol(tdm0))
{ if (sum(tdm0[, i1]) == 0) {a0 = c(a0, i1)} }
for (i1 in 1:ncol(tdm1))
{ if (sum(tdm1[, i1]) == 0) {a1 = c(a1, i1)} }

#checking a0 & a1
a0
a1
# Removing empty docs 
tdm0 <- tdm0[,-a0]
tdm1 <- tdm1[,-a1]

# Document term matrix 
dtm0 <- t(tdm0)
dtm1 <- t(tdm1)

# Word cloud - TF - Uni gram
makewordc(tdm0)
title(sub = "UNIGRAM - Wordcloud using TF")

# Frequency Bar plot - TF - Uni gram
words_bar_plot(tdm0)

# Word cloud - TFIDF - Unigram
makewordc(tdm1)

# Frequency Barplot - TFIDF - Unigram
words_bar_plot(tdm1)

# Positive word cloud - TF - Unigram
makeposwordc(tdm0)

title(sub = "UNIGRAM - POSITIVE Wordcloud using TF")

# Frequency Barplot - Positive words - Unigram
pos_words_bar_plot(dtm0)

# Positive word cloud - Unigram - TFIDF
makeposwordc(tdm1)
title(sub = "UNIGRAM - POSITIVE Wordcloud using TFIDF")

# Frequency Barplot - Positive words - TFIDF - Unigram
pos_words_bar_plot(dtm1)

# Negative word cloud - TF - unigam
makenegwordc(tdm0) # doubts doubt 
title(sub = "UNIGRAM - NEGATIVE Wordcloud using TF")

# Frequency Barplot -negative words - Unigram - TF
neg_words_bar_plot(dtm0)

# Negative word cloud - Unigram - TFIDF
makenegwordc(tdm1) # doubts doubt 
title(sub = "UNIGRAM - NEGATIVE Wordcloud using TFIDF")

# Frequency Barplot - Negative words - TFIDF
neg_words_bar_plot(dtm1)

# Bi gram word clouds
library(quanteda)
library(Matrix)

dtm0_2 <- dfm(unlist(x1),ngrams=2,verbose = F)

tdm0_2 <- t(dtm0_2)

a0 = NULL
for (i1 in 1:ncol(tdm0_2)){ if (sum(tdm0_2[, i1]) == 0) {a0 = c(a0, i1)} }
length(a0)		# no. of empty docs in the corpus
if (length(a0) >0) { tdm0_2 = tdm0_2[, -a0]} else {tdm0_2 = tdm0_2};	dim(tdm0_2)	# under TF weighing
a0 <- NULL;i1 <- NULL

dtm0_2 <- t(tdm0_2)
# Bi gram word cloud
makewordc(tdm0_2) # We have too see warnings to edit few words
title(sub = "BIGRAM - Wordcloud using TF")