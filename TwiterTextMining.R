#Extract tweets for any user (try choosing a user who has more tweets)
#Perform sentimental analysis on the tweets extracted from the above
#Load the libraries
library(twitteR)
library("ROAuth")
library(base64enc)
library(httpuv)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

cred <- OAuthFactory$new(consumerKey='9fdyRJxWFjLYQrgFGMirnCOvR',
                         consumerSecret='WQsiV5u5IEojqSMUwgj0lAW7d0A7E6CgSkWwBCGrDdIq9INlBs',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")

setup_twitter_oauth("9fdyRJxWFjLYQrgFGMirnCOvR", 
                    "WQsiV5u5IEojqSMUwgj0lAW7d0A7E6CgSkWwBCGrDdIq9INlBs",
                    "1241046683162521600-0lkgYidoIK9BSK7gHwBottw6ND3qtc", # Access token
                    "XHAww03kkG4SvISgJ7azm8UNpu7MH6UXv5su6lWsvDR6Z")  # Access token secret key
Tweets <- userTimeline('MKBHD', n = 2000)

TweetsDF <- twListToDF(Tweets)
write.csv(TweetsDF, "MKBHD.csv")
getwd()

View(MKBHD)
str(MKBHD)

# Build Corpus and DTM/TDM
corpus <- MKBHD$text
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
# Clean the text 
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeNumbers)
corpus_clean<-tm_map(corpus,stripWhitespace)
cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])
cleanset<-tm_map(cleanset,removeWords, c('MKBHD','can'))

cleanset <- tm_map(cleanset, gsub,pattern = 'pages', replacement = 'page')
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]
# Bar Plot 
w <- rowSums(tdm)
w <- subset(w, w>= 25)
barplot(w, las = 2, col = rainbow(50))

# Word Cloud :

w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)
##Read File
data <- read.csv(file.choose(), header = TRUE)
tweets <- as.character(data$text)
class(tweets)
s <- get_nrc_sentiment(tweets)
head(s)
# Barplot 

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Tweets Of Marques Brownie')
