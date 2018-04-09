library(RCurl)
library(bitops)
library(twitteR)
library(tm)
library(NLP)
library(wordcloud)
library(RColorBrewer)
library(textclean)

consumerKey <- "eIaUSZ0CW1baBIawv6aO3yGhA"
consumerSecret <- "U8ceWsFc8lymqRtfhK1b1QQR1DFvDUPUORbBNkYy4ziOiOhk3a"
accessToken <- "978170993544806400-lb7cs4vPkCtbHqwCO6vynaMceKRs0Ig"
accessSecret <- "ewPYmyyGWce0ZgOpITkqJxJ8hVbKzkSZqecSG6Tjx6wD4"

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessSecret)

nba_tweets <- searchTwitter("NBA", n = 1000, lang= "en")

nba_tweets_text <- sapply(nba_tweets, function(x) x$getText())
str(nba_tweets_text)
replace_emoji(nba_tweets_text)

for(i in seq(nba_tweets_text)){
  nba_tweets_text[[i]]<-gsub('[[:punct:]]', '', nba_tweets_text[[i]])
  nba_tweets_text[[i]]<-gsub("¡A"," ", nba_tweets_text[[i]])
  nba_tweets_text[[i]]<-gsub("-"," ", nba_tweets_text[[i]])
  nba_tweets_text[[i]]<-gsub("U+1F525"," ", nba_tweets_text[[i]])
}


nba_corpus <- Corpus(VectorSource(nba_tweets_text))

toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))}
)

nba_corpus_clean <- tm_map(nba_corpus, removePunctuation)
for(i in seq(nba_corpus_clean)){
  nba_corpus_clean[[i]]<-gsub('[[:punct:]]', '', nba_corpus_clean[[i]])
  nba_corpus_clean[[i]]<-gsub("¡A"," ", nba_corpus_clean[[i]])
  nba_corpus_clean[[i]]<-gsub("-"," ", nba_corpus_clean[[i]])
  nba_corpus_clean[[i]]<-gsub("U+1F525"," ", nba_corpus_clean[[i]])
}
nba_corpus_clean <- tm_map(nba_corpus_clean, content_transformer(tolower))
nba_corpus_clean <- tm_map(nba_corpus_clean, removeWords, stopwords("english"))
nba_corpus_clean <- tm_map(nba_corpus_clean, removeNumbers)
nba_corpus_clean <- tm_map(nba_corpus_clean, stripWhitespace)
nba_corpus_clean <- tm_map(nba_corpus_clean, removeWords, "nba")
nba_corpus_clean <- tm_map(nba_corpus_clean, toSpace, "nba")

wordcloud(nba_corpus_clean)
wordcloud(nba_corpus_clean, random.order = F, max.words = 40, scale = c(8, 0.5), colors = rainbow(50))