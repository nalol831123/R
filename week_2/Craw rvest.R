library(xml2)
library(rvest)

read_html("https://news.pts.org.tw/list/1")
doc <- read_html("https://news.pts.org.tw/list/1")

doc %>% html_nodes(".list-news-title a")

doc %>% html_nodes(".list-news-title a") %>% html_attr("href")
doc %>% html_nodes(".list-news-title a") %>% html_text()
