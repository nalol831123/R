data <- list()
title <- list()


for( i in c(6757:6910)){
  tmp <- paste(i, '.html', sep='')
  url <- paste('www.ptt.cc/bbs/Baseball/index', tmp, sep='')
  html <- htmlParse(GET(url),encoding = "UTF-8")
  title.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlValue)
  url.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlAttrs)
  data <- rbind(data, as.matrix(paste('www.ptt.cc', url.list, sep='')))
  title <- rbind(title, as.matrix(title.list))
}




data <- unlist(data)
title <- unlist(title)

o <- c()
o.url <- c()


o1 <- grep("大谷", title)

o <- c(o,title[o1])

o.url <- c(o.url, data[o1])


message <- list()
cc = worker()
oTDF <- data.frame()


for(i in c(1:length(o))){
  html <- htmlParse(GET(o.url[i]),encoding = "UTF-8")
  message.list <- xpathSApply(html, "//div[@class='push']/span[@class='f3 push-content']", xmlValue)
  message <- unlist(message.list)
  

  d.corpus <- VCorpus( VectorSource(message) )
  d.corpus <- tm_map(d.corpus, removePunctuation)
  d.corpus <- tm_map(d.corpus, removeNumbers)
  d.corpus <- tm_map(d.corpus, function(word) {
    gsub("[A-Za-z0-9]", "", word)
  })

  abc <- data.frame(table(cc[as.character(d.corpus)]))
  colnames(abc) <- c("word", as.character(i))
  

  if(i == 1){
    oTDF <- abc}
  else{
    oTDF <- merge(oTDF, abc, by = "word", all = T)}
}

oTDF[is.na(oTDF)] <- 0

library(knitr)
kable(head(oTDF))


n <- length(o)
tf <- apply(as.matrix(oTDF[,2:(n+1)]), 2, sum)
#print(tf)
library(Matrix)
idfCal <- function(word_doc)
{ 
  log2( n / nnzero(word_doc) ) 
}
idf <- apply(as.matrix(oTDF[,2:(n+1)]), 1, idfCal)

#print(ncol(oTDF))
doc.tfidf <- oTDF
for(x in 1:nrow(oTDF))
{
  for(y in 2:ncol(oTDF))
  {
    doc.tfidf[x,y] <- (doc.tfidf[x,y] / tf[y-1]) * idf[x]
  }
}


topwords <- subset(head(doc.tfidf[order(doc.tfidf[2], decreasing = TRUE), ]), select = c(word,`1`))
for (i in c(3:ncol(doc.tfidf))){
  topwords <- cbind(topwords, head(doc.tfidf[order(doc.tfidf[i], decreasing = TRUE),])[1])
  topwords <- cbind(topwords, head(doc.tfidf[order(doc.tfidf[i], decreasing = TRUE),])[i])
}
kable(topwords)
