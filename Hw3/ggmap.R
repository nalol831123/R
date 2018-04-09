install.packages("ggmap")
install.packages("mapproj")
install.packages("ggplot2")
install.packages("maps")


library(ggmap)
library(mapproj)

map <- get_map(location = c(lon = 121.5581794, lat = 25.0270206),zoom = 10, language = "zh-TW",maptype = "toner-lite")

ggmap(map)
ggmap(map, darken = c(0.5, "white"))
ggmap(map, darken = 0.5)

uv <- read.csv("UV_20180320220329.csv.csv")

lon.deg <- sapply((strsplit(as.character(uv$WGS84Lon), ",")), as.numeric)
uv$lon <- lon.deg[1, ] + lon.deg[2, ]/60 + lon.deg[3, ]/3600
lat.deg <- sapply((strsplit(as.character(uv$WGS84Lat), ",")), as.numeric)
uv$lat <- lat.deg[1, ] + lat.deg[2, ]/60 + lat.deg[3, ]/3600


library(ggmap)
map <- get_map(location = 'Taiwan', zoom = 7)
ggmap(map) + geom_point(aes(x = lon, y = lat, size = UVI), data = uv)