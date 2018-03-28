library(RCurl)
library(bitops)
library(twitteR)
library(tm)
library(NLP)
library(wordcloud)
library(RColorBrewer)
install.packages("grep")

consumerKey <- "eIaUSZ0CW1baBIawv6aO3yGhA"
consumerSecret <- "U8ceWsFc8lymqRtfhK1b1QQR1DFvDUPUORbBNkYy4ziOiOhk3a"
accessToken <- "978170993544806400-lb7cs4vPkCtbHqwCO6vynaMceKRs0Ig"
accessSecret <- "ewPYmyyGWce0ZgOpITkqJxJ8hVbKzkSZqecSG6Tjx6wD4"

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessSecret)

nba_tweets <- searchTwitter("NBA", n = 1000, lang= "en")

nba_tweets_text <- sapply(nba_tweets, function(x) x$getText())
str(nba_tweets_text)

gsub('\\p{So}|\\p{Cn}', '', nba_tweets_text, perl = TRUE)

nba_corpus <- Corpus(VectorSource(nba_tweets_text))

inspect(nba_corpus[1])
inspect(nba_corpus_clean[5])

nba_corpus_clean <- tm_map(nba_corpus, removePunctuation)
nba_corpus_clean <- tm_map(nba_corpus_clean, content_transformer(tolower))
nba_corpus_clean <- tm_map(nba_corpus_clean, removeWords, stopwords("english"))
nba_corpus_clean <- tm_map(nba_corpus_clean, removeNumbers)
nba_corpus_clean <- tm_map(nba_corpus_clean, stripWhitespace)
nba_corpus_clean <- tm_map(nba_corpus_clean, removeWords, c("nba"))

wordcloud(nba_corpus_clean)
wordcloud(nba_corpus_clean, random.order = F, max.words = 40, scale = c(8, 0.5), colors = rainbow(50))