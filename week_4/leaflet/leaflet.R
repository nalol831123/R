install.packages("leaflet")

library(leaflet)

map <- leaflet() %>%
  addTiles() %>%  # 加上預設的地圖資料
  addMarkers(lng=120.239, lat=22.992, popup="訊息方塊的文字說明")
map  # 繪製地圖

map <- leaflet() %>%
  addTiles() %>%  # 加上預設的地圖資料
  addMarkers(lng=121.560, lat=25.027, popup="訊息方塊的文字說明")
map  # 繪製地圖

m <- leaflet() %>%
  addTiles() %>%
  setView(121.560, 25.027, zoom = 18)
m

m %>% fitBounds(121.560, 25.027, 121.5628952, 25.0257748)

set.seed(3)
point.df <- data.frame(
  Lat = 22.992 + rnorm(10)/800,
  Long = 120.239 + rnorm(10)/800
)
m <- leaflet(point.df) %>%
  addTiles() %>%
  setView(lng = 120.239, lat = 22.992, zoom = 17)
m %>% addCircles()

set.seed(3)
point.df <- data.frame(
  lat = 22.97 + rnorm(100)/800,
  long = 120.23 + rnorm(100)/800,
  size = runif(100, 5, 20),
  color = sample(colors(), 100)
)
m <- leaflet(point.df) %>%
  addTiles() %>%
  setView(lng = 120.23, lat = 22.97, zoom = 17)
m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)


m <- leaflet() %>% setView(lng=121.560, lat=25.027, zoom = 20)
m %>% addTiles()