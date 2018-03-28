library(httr)

token  = "EAACEdEose0cBAKcSZBCZCYM38DlKBbwPZCZCRvFT8i5QX00Vj59vRH2hnxV1KT1THq2ZBsuREhswBvn2dMwqIEGvs2NCRBKVEzOqVDKO8b3YPCo0Y7fuOjRSyK8bvgYOkbZC2hCUMKaHo0NauFeZCbF3rTtT57otKDVpLT1dwEGT1qBysnGg1oZCoCqzmm5mkfTT6fVXpZCZBhDzbr9jCTZBti1"
prefex = "https://graph.facebook.com/v2.12/DoctorKoWJ/?fields=posts&access_token="
url    = paste0(prefex, token)
res    = httr::GET(url)
posts  = content(res)

res = POST("https://graph.facebook.com/v2.12/me/feed",
           body=list(message=sprintf("[TEST Posting Message] %s At %s","httr 皜祈岫",Sys.time()),
                     access_token=token))
postId = content(res)$id


url = sprintf("https://graph.facebook.com/v2.12/%s?access_token=%s", postId, token)
res = DELETE(url)
content(res)