library(xml2)
library(rvest)
page.source <- read_html("https://en.wikipedia.org/wiki/R_(programming_language)")

version.block <- html_nodes(page.source, ".wikitable th+ td , th:nth-child(2) , .wikitable th:nth-child(1)")
head(version.block)

content <- html_text(version.block)
content