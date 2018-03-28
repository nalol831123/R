install.packages("twitteR")
install.packages("ROAuth")

library(twitteR)
library(ROAuth)

setup_twitter_oauth( "eIaUSZ0CW1baBIawv6aO3yGhA", 	"U8ceWsFc8lymqRtfhK1b1QQR1DFvDUPUORbBNkYy4ziOiOhk3a", 
"978170993544806400-lb7cs4vPkCtbHqwCO6vynaMceKRs0Ig", "ewPYmyyGWce0ZgOpITkqJxJ8hVbKzkSZqecSG6Tjx6wD4")

tweets <- userTimeline("RDataMining", n = 3200)

url <- "http://www.rdatamining.com/data/RDataMining-Tweets-20160212.rds"
download.file(url, destfile = "RDataMining-Tweets-20160212.rds")

tweets <- readRDS("RDataMining-Tweets-20160212.rds")

(n.tweet <- length(tweets))

tweets.df <- twListToDF(tweets)

tweets.df[190, c("id", "created", "screenName", "replyToSN",
                 "favoriteCount", "retweetCount", "longitude", "latitude", "text")]

writeLines(strwrap(tweets.df$text[190], 60))

install.packages("tm")
install.packages("NLP")
library(NLP)
library(tm)

myCorpus <- Corpus(VectorSource(tweets.df$text))
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                 "use", "see", "used", "via", "amp")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

myCorpus <- tm_map(myCorpus, stripWhitespace)

myCorpusCopy <- myCorpus

myCorpus <- tm_map(myCorpus, stemDocument) # stem words
writeLines(strwrap(myCorpus[[190]]$content, 60))

stemCompletion2 <- function(x, dictionary) f
x <- unlist(strsplit(as.character(x), " "))
x <- x[x != ""]
x <- stemCompletion(x, dictionary=dictionary)
x <- paste(x, sep="", collapse=" ")
PlainTextDocument(stripWhitespace(x))
g
myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
myCorpus <- Corpus(VectorSource(myCorpus))
writeLines(strwrap(myCorpus[[190]]$content, 60))