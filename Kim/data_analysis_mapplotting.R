install.packages("leaflet")
library(leaflet)

setwd("/Users/DK/Documents/programming/Github/Regression Analysis/rawdata//")
data <- read.csv("kc_house_data.csv", header = TRUE)

# 집 위치 경도/위도 추출
lati <- data$lat
long <- data$long

# leaflet패키지를 이용하여 지도에 map plotting
map <- leaflet() %>% 
    addTiles() %>% 
    addCircleMarkers(lng = long, lat = lati, radius = 0.01, clusterOptions = TRUE) %>% markerClusterOptions(showCoverageOnHover = TRUE, spiderLegPolylineOptions = list(weight = 1.5, color = "#222", opacity = 0.5))
