##載入套件
library(rvest)

##一開始到綜藝大熱門的頁面使用過去學的方法爬，但是都跑不了，也不知道為什麼
##推測是youtube本身的限制，就像FB、TWITTER那樣
##花了很多時間研究都還是沒辦法，在網路上找到的大部分方也也都是用Python
##正當開始研究如何用Python爬時，看到一篇python的教學是從youtube的搜尋頁面上搜尋然後爬下來
##就改用R試試看這個方法，結果竟然可以，=
##於是用FOR迴圈開始搜尋爬，因為有些日子是重播加上六日沒有播，就分開一個月一個月處理
##不過這也造成一個悲劇，爬幾個月後就會被YOUTUBE擋下來，說流量異常，然後就不能爬了
##換了三個IP才爬完XD

##爬取2018五月收視率
viewlist05 <- list()
for( i in c(20180501:20180524)){
  date <- i
  url <- paste('https://www.youtube.com/results?search_query=%E7%B6%9C%E8%97%9D%E5%A4%A7%E7%86%B1%E9%96%80', i, sep='')
  res <- read_html(url)
  view <- html_text(html_nodes(res, ".yt-lockup-meta-info"))
  view1 <- view[1]
  viewlist05 <- rbind(viewlist05, as.matrix(view1))
}
viewlist05 <- unlist(viewlist05)
##扣掉六日與重播天數(剩下1,2,3,7,8,9,10,14,15,16,17,21,22,23,24)
viewlist05 <- c(viewlist05[1],viewlist05[2],viewlist05[3],viewlist05[7],viewlist05[8],viewlist05[9],viewlist05[10],viewlist05[14],viewlist05[15],viewlist05[16],viewlist05[17],viewlist05[21],viewlist05[22],viewlist05[23],viewlist05[24])

##爬取2018四月收視率
viewlist04 <- list()
for( i in c(20180401:20180430)){
  date <- i
  url <- paste('https://www.youtube.com/results?search_query=%E7%B6%9C%E8%97%9D%E5%A4%A7%E7%86%B1%E9%96%80', i, sep='')
  res <- read_html(url)
  view <- html_text(html_nodes(res, ".yt-lockup-meta-info"))
  view1 <- view[1]
  viewlist04 <- rbind(viewlist04, as.matrix(view1))
}
viewlist04 <- unlist(viewlist04)
##扣掉六日與重播天數(剩下2,3,4,5,9,10,11,12,16,17,18,19,23,24,25,26)
viewlist04 <- c(viewlist04[2],viewlist04[3],viewlist04[4],viewlist04[5],viewlist04[9],viewlist04[10],viewlist04[11],viewlist04[12],viewlist04[16],viewlist04[17],viewlist04[18],viewlist04[19],viewlist04[23],viewlist04[24],viewlist04[25],viewlist04[26])

##爬取2018三月收視率
viewlist03 <- list()
for( i in c(20180301:20180331)){
  date <- i
  url <- paste('https://www.youtube.com/results?search_query=%E7%B6%9C%E8%97%9D%E5%A4%A7%E7%86%B1%E9%96%80', i, sep='')
  res <- read_html(url)
  view <- html_text(html_nodes(res, ".yt-lockup-meta-info"))
  view1 <- view[1]
  viewlist03 <- rbind(viewlist03, as.matrix(view1))
}
viewlist03 <- unlist(viewlist03)
##扣掉六日與重播天數(剩下5,6,7,8,12,13,14,15,19,20,21,22,26,27,28,29)
viewlist03 <- c(viewlist03[5],viewlist03[6],viewlist03[7],viewlist03[8],viewlist03[12],viewlist03[13],viewlist03[14],viewlist03[15],viewlist03[19],viewlist03[20],viewlist03[21],viewlist03[22],viewlist03[26],viewlist03[27],viewlist03[28],viewlist03[29])

##爬取2018二月收視率
viewlist02 <- list()
for( i in c(20180201:20180230)){
  date <- i
  url <- paste('https://www.youtube.com/results?search_query=%E7%B6%9C%E8%97%9D%E5%A4%A7%E7%86%B1%E9%96%80', i, sep='')
  res <- read_html(url)
  view <- html_text(html_nodes(res, ".yt-lockup-meta-info"))
  view1 <- view[1]
  viewlist02 <- rbind(viewlist02, as.matrix(view1))
}
viewlist02 <- unlist(viewlist02)
##扣掉六日與重播天數(剩下1.5.6.7.8.12.13.14.16.21.22.26.27.28)
viewlist02 <- c(viewlist02[1],viewlist02[5],viewlist02[6],viewlist02[7],viewlist02[8],viewlist02[12],viewlist02[13],viewlist02[14],viewlist02[16],viewlist02[21],viewlist02[22],viewlist02[26],viewlist02[27],viewlist02[28])

##爬取2018一月收視率
viewlist01 <- list()
for( i in c(20180101:20180131)){
  date <- i
  url <- paste('https://www.youtube.com/results?search_query=%E7%B6%9C%E8%97%9D%E5%A4%A7%E7%86%B1%E9%96%80', i, sep='')
  res <- read_html(url)
  view <- html_text(html_nodes(res, ".yt-lockup-meta-info"))
  view1 <- view[1]
  viewlist01 <- rbind(viewlist01, as.matrix(view1))
}
viewlist01 <- unlist(viewlist01)
##扣掉六日與重播天數(剩下1,2,3,4,8,9,10,11,15,16,17,18,22,23,24,25,29,30,31)
viewlist01 <- c(viewlist01[1],viewlist01[2],viewlist01[3],viewlist01[4],viewlist01[8],viewlist01[9],viewlist01[10],viewlist01[11],viewlist01[15],viewlist01[16],viewlist01[17],viewlist01[18],viewlist01[22],viewlist01[23],viewlist01[24],viewlist01[25],viewlist01[29],viewlist01[30],viewlist01[31])

##爬取2017十二月收視率
viewlist1712 <- list()
for( i in c(20171201:20171231)){
  date <- i
  url <- paste('https://www.youtube.com/results?search_query=%E7%B6%9C%E8%97%9D%E5%A4%A7%E7%86%B1%E9%96%80', i, sep='')
  res <- read_html(url)
  view <- html_text(html_nodes(res, ".yt-lockup-meta-info"))
  view1 <- view[1]
  viewlist1712 <- rbind(viewlist1712, as.matrix(view1))
}
viewlist1712 <- unlist(viewlist1712)
##扣掉六日與重播天數(剩下4,5,6,7,11,12,13,14,18,19,20,21,25,26,27,28)
viewlist1712 <- c(viewlist1712[4],viewlist1712[5],viewlist1712[6],viewlist1712[7],viewlist1712[11],viewlist1712[12],viewlist1712[13],viewlist1712[14],viewlist1712[18],viewlist1712[19],viewlist1712[20],viewlist1712[21],viewlist1712[25],viewlist1712[26],viewlist1712[27],viewlist1712[28])

##爬取2017十一月收視率
viewlist1711 <- list()
for( i in c(20171101:20171130)){
  date <- i
  url <- paste('https://www.youtube.com/results?search_query=%E7%B6%9C%E8%97%9D%E5%A4%A7%E7%86%B1%E9%96%80', i, sep='')
  res <- read_html(url)
  view <- html_text(html_nodes(res, ".yt-lockup-meta-info"))
  view1 <- view[1]
  viewlist1711 <- rbind(viewlist1711, as.matrix(view1))
}
viewlist1711 <- unlist(viewlist1711)
##扣掉六日與重播天數(剩下1,2,6,7,8,9,13,14,15,16,20,21,22,23,27,28,29,30)
viewlist1711 <- c(viewlist1711[1],viewlist1711[2],viewlist1711[6],viewlist1711[7],viewlist1711[8],viewlist1711[9],viewlist1711[13],viewlist1711[14],viewlist1711[15],viewlist1711[16],viewlist1711[20],viewlist1711[21],viewlist1711[22],viewlist1711[23],viewlist1711[27],viewlist1711[28],viewlist1711[29],viewlist1711[30])

##爬取2017十月收視率
viewlist1710 <- list()
for( i in c(20171001:20171031)){
  date <- i
  url <- paste('https://www.youtube.com/results?search_query=%E7%B6%9C%E8%97%9D%E5%A4%A7%E7%86%B1%E9%96%80', i, sep='')
  res <- read_html(url)
  view <- html_text(html_nodes(res, ".yt-lockup-meta-info"))
  view1 <- view[1]
  viewlist1710 <- rbind(viewlist1710, as.matrix(view1))
}
viewlist1710 <- unlist(viewlist1710)
##扣掉六日與重播天數(剩下2,3,4,5,9,10,11,12,16,17,18,19,23,24,25,26,30,31)
viewlist1710 <- c(viewlist1710[2],viewlist1710[3],viewlist1710[4],viewlist1710[5],viewlist1710[9],viewlist1710[10],viewlist1710[11],viewlist1710[12],viewlist1710[16],viewlist1710[17],viewlist1710[18],viewlist1710[19],viewlist1710[23],viewlist1710[24],viewlist1710[25],viewlist1710[26],viewlist1710[30],viewlist1710[31])

##爬取2017九月收視率
viewlist1709 <- list()
for( i in c(20170901:20170930)){
  date <- i
  url <- paste('https://www.youtube.com/results?search_query=%E7%B6%9C%E8%97%9D%E5%A4%A7%E7%86%B1%E9%96%80', i, sep='')
  res <- read_html(url)
  view <- html_text(html_nodes(res, ".yt-lockup-meta-info"))
  view1 <- view[1]
  viewlist1709 <- rbind(viewlist1709, as.matrix(view1))
}
viewlist1709 <- unlist(viewlist1709)
##扣掉六日與重播天數(剩下4,5,6,7,11,12,13,14,18,19,20,21,25,26,27,28)
viewlist1709 <- c(viewlist1709[4],viewlist1709[5],viewlist1709[6],viewlist1709[7],viewlist1709[11],viewlist1709[12],viewlist1709[13],viewlist1709[14],viewlist1709[18],viewlist1709[19],viewlist1709[20],viewlist1709[21],viewlist1709[25],viewlist1709[26],viewlist1709[27],viewlist1709[28])

##爬取2017八月收視率
viewlist1708 <- list()
for( i in c(20170801:20170831)){
  date <- i
  url <- paste('https://www.youtube.com/results?search_query=%E7%B6%9C%E8%97%9D%E5%A4%A7%E7%86%B1%E9%96%80', i, sep='')
  res <- read_html(url)
  view <- html_text(html_nodes(res, ".yt-lockup-meta-info"))
  view1 <- view[1]
  viewlist1708 <- rbind(viewlist1708, as.matrix(view1))
}
viewlist1708 <- unlist(viewlist1708)
##扣掉六日與重播天數(剩下1,2,3,7,8,9,10,14,15,16,17,21,22,23,24,28,29,30,31)
viewlist1708 <- c(viewlist1708[1],viewlist1708[2],viewlist1708[3],viewlist1708[7],viewlist1708[8],viewlist1708[9],viewlist1708[10],viewlist1708[14],viewlist1708[15],viewlist1708[16],viewlist1708[17],viewlist1708[21],viewlist1708[22],viewlist1708[23],viewlist1708[24],viewlist1708[28],viewlist1708[29],viewlist1708[30],viewlist1708[31])

##爬取2017七月收視率
viewlist1707 <- list()
for( i in c(20170701:20170731)){
  date <- i
  url <- paste('https://www.youtube.com/results?search_query=%E7%B6%9C%E8%97%9D%E5%A4%A7%E7%86%B1%E9%96%80', i, sep='')
  res <- read_html(url)
  view <- html_text(html_nodes(res, ".yt-lockup-meta-info"))
  view1 <- view[1]
  viewlist1707 <- rbind(viewlist1707, as.matrix(view1))
}
viewlist1707 <- unlist(viewlist1707)
##扣掉六日與重播天數(剩下3,4,5,6,10,11,12,13,17,18,19,20,24,25,26,27,31)
viewlist1707 <- c(viewlist1707[3],viewlist1707[4],viewlist1707[5],viewlist1707[6],viewlist1707[10],viewlist1707[11],viewlist1707[12],viewlist1707[13],viewlist1707[17],viewlist1707[18],viewlist1707[19],viewlist1707[20],viewlist1707[24],viewlist1707[25],viewlist1707[26],viewlist1707[27],viewlist1707[31])

##爬取2017六月收視率
viewlist1706 <- list()
for( i in c(20170601:20170630)){
  date <- i
  url <- paste('https://www.youtube.com/results?search_query=%E7%B6%9C%E8%97%9D%E5%A4%A7%E7%86%B1%E9%96%80', i, sep='')
  res <- read_html(url)
  view <- html_text(html_nodes(res, ".yt-lockup-meta-info"))
  view1 <- view[1]
  viewlist1706 <- rbind(viewlist1706, as.matrix(view1))
}
viewlist1706 <- unlist(viewlist1706)
##扣掉六日與重播天數(剩下1,5,6,7,8,12,13,14,15,19,20,21,22,26,27,28,29)
viewlist1706 <- c(viewlist1706[1],viewlist1706[5],viewlist1706[6],viewlist1706[7],viewlist1706[8],viewlist1706[12],viewlist1706[13],viewlist1706[14],viewlist1706[15],viewlist1706[19],viewlist1706[20],viewlist1706[21],viewlist1706[22],viewlist1706[26],viewlist1706[27],viewlist1706[28],viewlist1706[29])

##爬取2017五月收視率
viewlist1705 <- list()
for( i in c(20170501:20170522)){
  date <- i
  url <- paste('https://www.youtube.com/results?search_query=%E7%B6%9C%E8%97%9D%E5%A4%A7%E7%86%B1%E9%96%80', i, sep='')
  res <- read_html(url)
  view <- html_text(html_nodes(res, ".yt-lockup-meta-info"))
  view1 <- view[1]
  viewlist1705 <- rbind(viewlist1705, as.matrix(view1))
}
viewlist1705 <- unlist(viewlist1705)
##扣掉六日與重播天數(剩下1,2,3,4,8,9,10,11,15,16,17,18,22)
viewlist1705 <- c(viewlist1705[1],viewlist1705[2],viewlist1705[3],viewlist1705[4],viewlist1705[8],viewlist1705[9],viewlist1705[10],viewlist1705[11],viewlist1705[15],viewlist1705[16],viewlist1705[17],viewlist1705[18],viewlist1705[22])

##把2017年五月到2018年五月的資料合併
youtubeview <- c(viewlist1705, viewlist1706, viewlist1707, viewlist1708, viewlist1709, viewlist1710, viewlist1711, viewlist1712, viewlist01, viewlist02, viewlist03, viewlist04, viewlist05)

##output
write.csv(youtubeview,file="youtubeview.csv",row.names = F)