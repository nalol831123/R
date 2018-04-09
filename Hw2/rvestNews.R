library(rvest)

nba <-read_html("https://nba.udn.com/nba/cate/6754/6780")


title <- html_nodes(nba,"#news_list_body h3")

NbaTitle <- html_text(title)
NbaTitle

