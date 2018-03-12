library(XML)
library(xml2)
library(rvest)

url <- "http://www.gomaji.com/index.php?city=Taipei&ref=396&gclid=EAIaIQobChMI--Ozs8vf2QIVhpS9Ch04BAAlEAAYASAAEgKSE_D_BwE&page=2"

read <- read_html(url, encoding = "UTF-8")

title4 <- html_text(html_nodes(read, "#lb_deal .ref_name_2"))

title4