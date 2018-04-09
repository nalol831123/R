---
title: "GomajiCraw"
output: html_document
---

載入所需套件
```{r}
library(xml2)
library(rvest)
library(XML)
library(selectr)
```

下載網址

```{r}
url <- "http://www.gomaji.com/index.php?city=Taipei&ref=396&gclid=EAIaIQobChMI--Ozs8vf2QIVhpS9Ch04BAAlEAAYASAAEgKSE_D_BwE&page=2"
```

用read_html讀入網址

```{r}
read <- read_html(url, encoding = "UTF-8")
```

在網站上找到標題的css用html_text抓出

```{r}
title4 <- html_text(html_nodes(read, "#lb_deal .ref_name_2"))
```

```{r}
title4
```