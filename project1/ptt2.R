library(bitops)
library(httr)
library(RCurl)
library(XML)
library(NLP)
library(tm)
library(tmcn)
library(jiebaRD)
library(jiebaR)
library(dplyr)
```

### 2.爬PTT台綜版
####抓取三月以來PTT台綜版的文章
```{r}
from <- 485
to   <- 501

prefix = "https://www.ptt.cc/bbs/TW_Entertain/index"

data <- list()
title <- list()

for (id in c(from:to))
{
  url <- paste0(prefix, as.character(id), ".html" )
  html<- htmlParse(GET(url))
  url.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlAttrs)
  title.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlValue)
  data <- rbind(data, as.matrix(paste("www.ptt.cc", url.list, sep='')))
  title <- rbind(title, as.matrix(title.list))
}
data <- unlist(data)
title <- unlist(title)

head(data)
head(title)

```

### 3. 找出兩個所需的關鍵字
####找出綜藝大熱門與小明星大跟班
```{r}
Hito <- c()
Hito.url <- c()

HitoT <- grep("綜藝大熱門", title)

Hito <- c(Hito,title[HitoT])
Hito.url <- c(Hito.url, data[HitoT])

SB <- c()
SB.url <- c()

SBT <- grep("小明星大跟班", title)

SB <- c(SB,title[SBT])
SB.url <- c(SB.url, data[SBT])

```

### 4. 留言內容擷取 分類 文本清理和建立文本矩陣 TermDocumentMatrix
```{r}
message <- list()
HitoTDF <- data.frame()
SBTDF <- data.frame()
jie = worker()


for(i in c(1:length(Hito))){
  html <- htmlParse(GET(Hito.url[i]),encoding = "UTF-8")
  message.list <- xpathSApply(html, "//div[@class='push']/span[@class='f3 push-content']", xmlValue)
  message <- unlist(message.list)
  
  d.corpus <- VCorpus( VectorSource(message) )
  d.corpus <- tm_map(d.corpus, removePunctuation)
  d.corpus <- tm_map(d.corpus, removeNumbers)
  d.corpus <- tm_map(d.corpus, function(word) {
    gsub("[A-Za-z0-9]", "", word)
  })
  
  abc <- data.frame(table(jie[as.character(d.corpus)]))
  colnames(abc) <- c("word", as.character(i))
  
  if(i == 1){
    HitoTDF <- abc}
  else{
    HitoTDF <- merge(HitoTDF, abc, by = "word", all = T)}
  
}


```

```{r}
for(i in c(1:length(SB))){
  html <- htmlParse(GET(SB.url[i]),encoding = "UTF-8")
  message.list <- xpathSApply(html, "//div[@class='push']/span[@class='f3 push-content']", xmlValue)
  message <- unlist(message.list)
  
  
  
  
  d.corpus <- VCorpus( VectorSource(message) )
  d.corpus <- tm_map(d.corpus, removePunctuation)
  d.corpus <- tm_map(d.corpus, removeNumbers)
  d.corpus <- tm_map(d.corpus, function(word) {
    gsub("[A-Za-z0-9]", "", word)
  })
  
  
  abc <- data.frame(table(jie[as.character(d.corpus)]))
  colnames(abc) <- c("word", as.character(i))
  
  
  if(i == 1){
    SBTDF <- abc}
  else{
    SBTDF <- merge(HitoTDF, abc, by = "word", all = T)}
}
```

```{r}
HitoTDF[is.na(HitoTDF)] <- 0
SBTDF[is.na(SBTDF)] <- 0


library(knitr)

```

```{r}
kable(head(HitoTDF))
kable(tail(HitoTDF))
```

```{r}
kable(head(SBTDF))
kable(tail(SBTDF))
```

###5. TDM 轉成 TF-IDF

```{r}
tf <- apply(as.matrix(HitoTDF[,2:(n+1)]), 2, sum)

library(Matrix)
idfCal1 <- function(word_doc)
{
  log2 (n/nnzero(word_doc))
}
idf1 <-apply(as.matrix(HitoTDF[,2 : (n+1)]), 1, idfCal1)

doc1.tfidf <- HitoTDF

tempY = matrix(rep(c(as.matrix(tf)), each = length(idf1)), nrow = length(idf1))
tempX = matrix(rep(c(as.matrix(idf)), each = length(tf)), nrow = length(tf), byrow = TRUE)

stopLine = rowSums(doc1.tfidf[,2:(n+1)])
delID = which(stopLine == 0)

kable(head(doc1.tfidf[delID,1]))
```

```{r}
n <- length(SB)
tf2 <- apply(as.matrix(SBTDF[,2:(n+1)]), 2, sum)

idfCal2 <- function(word_doc)
{ 
  log2( n / nnzero(word_doc) ) 
}
idf2 <- apply(as.matrix(SBTDF[,2:(n+1)]), 1, idfCal2)

doc2.tfidf <- SBTDF

for(x in 1:nrow(SBTDF))
{
  for(y in 2:ncol(SBTDF))
  {
    doc2.tfidf[x,y] <- (doc2.tfidf[x,y] / tf2[y-1]) * idf2[x]
  }
}
```