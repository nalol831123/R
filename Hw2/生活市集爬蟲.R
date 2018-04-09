library(httr)
library(bitops)
library(RCurl)    
library(XML) 
library(proto)
library(gsubfn)   

targeturl <- "https://www.buy123.com.tw/site"

res <- getURL(targeturl, encoding="utf-8")
res <- htmlParse(res)
res

xpath <- '//*[@id="container"]/div[4]/section[2]//a'
urls <- xpathSApply(res, path=xpath, xmlGetAttr, 'href')
urls

tmp = "https://www.buy123.com.tw"
urls <- paste0(tmp, urls)
urls


webpage_parser <- function(x, xpath){
  res_tmp <- getURL(x, encoding="utf-8")
  res_tmp <- htmlParse(res_tmp, encoding="UTF-8")
  textdata <- xpathSApply(res_tmp, path = xpath, xmlValue)
}