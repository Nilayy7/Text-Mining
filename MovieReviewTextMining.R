#Extracted the Customer reviews from IMDB on the movie "SHUTTER ISLAND" and performed wordcloud and # Sentimental analysis on the same.
#Load the libraries
library(rvest)
library(wordcloud)
library(XML)
library(tm)
library(magrittr)
library(ggplot2)
library(lubridate)
library(scales)
library(dplyr)
install.packages("devtools")
library(devtools)

#########IMDB Reviews#######
aurl <- "https://www.imdb.com/review/rw2211895/?ref_=tt_urv"
IMDB_reviews <- NULL
for(i in 1:20){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)
getwd()
setwd("C:/Users/hp/Downloads/Text Mining")

write.table(IMDB_reviews,"ShutterIsland.txt",row.names = F)
ShutterIsland <- read.delim('ShutterIsland.txt')
str(ShutterIsland)
View(ShutterIsland)

# Build Corpus and DTM/TDM
library(tm)
corpus <- ShutterIsland[-1,]
head(corpus)
class(corpus)

corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#Clean the Text
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])

#Remove Punctuation
corpus <- tm_map(corpus,removePunctuation)

#Remove Numbers
corpus <- tm_map(corpus,removeNumbers)

#Remove Whitespace
corpus_clean<-tm_map(corpus,stripWhitespace)

cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset<-tm_map(cleanset,removeWords, c('can','film'))
cleanset<-tm_map(cleanset,removeWords, c('movie','movies'))

# Removing the word movie and movies on similar grounds - as unnecessary.
cleanset <- tm_map(cleanset, gsub,pattern = 'character', replacement = 'characters')
inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm
#Convert into matrix
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

#Visualize the data
# Bar Plot 
w <- rowSums(tdm)
w <- subset(w,w>=50)
barplot(w,las=2,col=rainbow(50))

#Wordcloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

install.packages("wordcloud2")
library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, 
           minSize = 1)

# lettercloud 
letterCloud(w,word = 'A',frequency(5), size=1)

#Sentiment Analysis
install.packages("syuzhet")
library(syuzhet)
library(scales)
library(ggplot2)
library(dplyr)
library(lubridate)

# Read File 
IMDB_reviews <- read.delim('ShutterIsland.txt')
reviews <- as.character(IMDB_reviews[-1,])
class(reviews)
# Obtain Sentiment scores 
s <- get_nrc_sentiment(reviews)
head(s)
get_nrc_sentiment('splendid')
get_nrc_sentiment('no words')
# barplot 

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for IMDB Reviews
        for Shutter Island')
