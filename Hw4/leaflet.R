---
title: "leaflet"
output: html_document
---
###leaflet

```{r}
library(leaflet)
```
繪製地圖，加上預設的地圖資料
```{r}
map <- leaflet() %>%
  addTiles() %>%  
  addMarkers(lng=121.538, lat=25.0171, popup="國立台灣大學")
map
```
ZOOM IN
```{r}
m <- leaflet() %>%
  addTiles() %>%
  setView(121.538, 25.0171, zoom = 18)
m
```
自訂範圍
```{r}
m %>% fitBounds(121.538, 25.0171, 121.533, 25.016)
```
隨機標點
```{r}
set.seed(3)
point.df <- data.frame(
  Lat = 25.0171 + rnorm(10)/800,
  Long = 121.538 + rnorm(10)/800
)
m <- leaflet(point.df) %>%
  addTiles() %>%
  setView(lng = 121.538, lat = 25.0171, zoom = 17)
m %>% addCircles()

```

調點的大小
```{r}
set.seed(3)
point.df <- data.frame(
  lat = 25.0171 + rnorm(100)/800,
  long = 121.538 + rnorm(100)/800,
  size = runif(100, 5, 20),
  color = sample(colors(), 100)
)
m <- leaflet(point.df) %>%
  addTiles() %>%
  setView(lng = 121.538, lat = 25.0171, zoom = 17)
m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)
```

```{r}
m <- leaflet() %>% setView(lng=121.538, lat=25.0171, zoom = 20)
m %>% addTiles()
```
