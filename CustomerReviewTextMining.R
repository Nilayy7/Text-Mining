#Extract reviews of any product from ecommerce website like snapdeal and amazon
#Perform sentimental analysis
#Load the libraries
library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(syuzhet)
library(wordcloud2)
library(lubridate)
library(scales)
library(ggplot2)
library(reshape2)
library(dplyr)

#####Amazon Reviews########
aurl <- "https://www.amazon.in/gp/customer-reviews/RNGFFG6QIDH5/ref=cm_cr_dp_d_rvw_ttl?ie=UTF8&ASIN=B07DJ8K2KT"
amazon_reviews <- NULL
for (i in 1:20){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
length(amazon_reviews)
setwd("C:/Users/hp/Downloads/Text Mining")
write.table(amazon_reviews,"7TPro.txt",row.names = F)
Lap <- read.delim('7TPro.txt')
str(Lap)

# Build Corpus and DTM/TDM
library(tm)
corpus <- Lap[-1,]
head(corpus)
class(corpus)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean the text 
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeNumbers)
corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])

cleanset<-tm_map(corpus,removeWords, stopwords('english'))
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset<-tm_map(cleanset,removeWords, c('laptop','can'))
cleanset <- tm_map(cleanset, gsub,pattern = 'computer', replacement = 'machine')
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,stripWhitespace)

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

#Bar Plot
w <- rowSums(tdm)  
w <- subset(w, w>= 25) 
barplot(w, las = 2, col = rainbow(50))

# Word Cloud :
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)
library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, minSize = 1)


# Read File 
oneplusreviews <- read.delim('7TPro.txt')
reviews <- as.character(oneplusreviews[-1,])
class(reviews)

#Obtain Sentiment scores 
s <- get_nrc_sentiment(reviews)
head(s)
get_nrc_sentiment('Love')
get_nrc_sentiment('glaring')
barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for One Plus 7T Pro')
