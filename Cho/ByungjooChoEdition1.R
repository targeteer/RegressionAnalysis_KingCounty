library(corrplot) # 상관계수를 도표로 나타내기
library(car)
library(gvlma)
library(lm.beta)
library(nortest)
library(dplyr)
library(shiny)
library(leaflet)
library(RColorBrewer)


setwd("C:/Users/Master/Documents/rds/RegressionAnalysis_KingCounty/Cho")
data <- read.csv("kc_house_data.csv")
par(mfrow = c(1,1))
options(scipen = 10000)


###################### 변수의 변환 ###############

glimpse(data.train)
car::powerTransform(data$sqft_lot15)
summary(car::powerTransform(data$sqft_lot15))

data$zipcode

data$price <- log(data$price) 
data$sqft_living <- sqrt(data$sqft_living)
data$sqft_lot <- log(data$sqft_lot)
data$sqft_above <- log(data$sqft_above)

max(data$yr_built)-min(data$yr_built)
data$yr_built <- ((data$yr_built-1899)/116)*100 # 완공년도를 1~100으로 환산

min(data$yr_renovated)
max(data$yr_renovated)
data$yr_renovated <- ifelse(data$yr_renovated == 0, 0, 1) # 리모델링을 한적 있으면 1, 없으면 0

data$sqft_living15 <- log(data$sqft_living15)
data$sqft_lot15 <- log(data$sqft_lot15)






glimpse(data)
View(data)

###########################
# test, train 셋으로 나누기
data.index <- caret::createDataPartition(data$price, p = 0.8)
data.train <- data[unlist(data.index) , ]
data.test  <- data[-unlist(data.index) ,]

data.test$price <- log(data.test$price)
data.train$date = substr(data.train$date, 1, 6)
data.test$date = substr(data.test$date, 1, 6)

# 일단date를 숫자로바꿔야 상관성 확인가능
data.test$date <- as.numeric(data.test$date)
data.train$date <- as.numeric(data.train$date)
data.test$id <- NULL
data.train$id <- NULL

corr <- cor(data.train[, 1:20])

corrplot::corrplot(corr, method = "color", cl.pos = 'n', rect.col = "black",  tl.col = "black", addCoef.col = "black", number.digits = 2, number.cex = 0.50, tl.cex = 0.9, cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))

# 상관계수 0.2 미만 변수는 삭제
data.test$sqft_lot <- NULL 
data.test$condition <- NULL
data.test$yr_built <- NULL
data.test$yr_renovated <- NULL
data.test$sqft_lot15 <- NULL
data.test$sqft_basement <- NULL
data.test$date <- NULL



data.train$sqft_lot <- NULL 
data.train$condition <- NULL
data.train$yr_built <- NULL
data.train$yr_renovated <- NULL
data.train$sqft_lot15 <- NULL
data.train$sqft_basement <- NULL
data.train$date <- NULL



# 상관계수 0.2이상 변수 전부를 포함한 모델 생성
housesales.lm <- lm(price ~ bedrooms + bathrooms + sqft_living + floors + waterfront + view + grade + sqft_above + sqft_living15 + lat + , data = data.train)

# 상관계수 0.2이상 변수 전부 및 zipcode 집가격을 등급화하여 더비변수 처리한 모델
housesales.lm <- lm(price ~ bedrooms + bathrooms + sqft_living + floors + waterfront + view + grade + sqft_above + sqft_living15 + lat + zipcode.MidGrade + zipcode.GoodGrade + zipcode.ThebestGrade, data = data.train)

# 상관계수 0.2이상 변수 전부 및 zipcode를 집가격을 등급화하고, 위도를 나눈 더비변수 처리한 모델
housesales.lm <- lm(price ~  bathrooms + sqft_living + floors + waterfront + view + grade  + sqft_living15 + lat.mid + lat.north + zipcode.MidGrade + zipcode.GoodGrade + zipcode.ThebestGrade, data = data.train)



summary(housesales.lm) 
#결정계수 : 0.734
View(data)

# 다중공선성
car::vif(housesales.lm) # 전부 10미만, 문제없음


# 변수선택
# housesales.forward <-  step(housesales.lm, direction = "forward")
# housesales.backward <- step(housesales.lm, direction = "backward")
# housesales.stepwise <- step(housesales.lm, direction = "both")
# summary(housesales.stepwise)
# 
# housesales.final.lm <- lm(price ~ bedrooms + sqft_living + floors + waterfront + view + grade + lat, data = data.train)
# summary(housesales.final.lm)

# 이전 모델이 성능이 더 좋음.

####################

# 수정전 검정 ----
y_obs <- data.test$price
yhat_lm <- predict(housesales.lm, newdata = data.test)


# RMSE 값 확인 ----
exp(rmse(y_obs, yhat_lm)) # exp 처음 price에 자연로그를 씌어줬으므로....

# RMSE 확인함수
rmse <- function(yi, yhat_i){
  sqrt(mean((yi-yhat_i)^2))
}



# 에러에 대한 가정
# 1) 정규성 분석 
ad.test(housesales.lm$residuals) # 정규성 깨짐
# 2) 독립성 가정
car::durbinWatsonTest(housesales.lm)
# lag Autocorrelation D-W Statistic p-value
# 1     0.002659342      1.994613   0.738
# 3) 등분산성 가정
car::ncvTest(housesales.lm) # 이분산이다.
# 4) 에러에 대한 전반적인 가정에 검정
summary(gvlma::gvlma(housesales.lm))
# Value     p-value                   Decision
# Global Stat        427.06456 0.000000000 Assumptions NOT satisfied!
#   Skewness           128.08766 0.000000000 Assumptions NOT satisfied!
#   Kurtosis           277.38664 0.000000000 Assumptions NOT satisfied!
#   Link Function        0.08129 0.775556367    Assumptions acceptable.
# Heteroscedasticity  21.50898 0.000003522 Assumptions NOT satisfied!


data.predict <- predict(housesales.lm, newdata = data.test)
summary(data.predict)
summary(data.test$price)
par(mfrow = c(2,2))
plot(housesales.lm)


## 위도지역 북부,중부,남부 나누기

max(data$lat)
min(data$lat)
quantile(data$lat)

a <- dplyr::filter(data, data$lat1 == "남부") # 3
mean(a$price)

b <- dplyr::filter(data, data$lat1 == "중부") # 1
mean(b$price)

c <- dplyr::filter(data, data$lat1 == "북부") # 2
mean(c$price)



nrow(data)/3
sort(data$lat, decreasing = FALSE)[7204] # 위도 하위33%기(남부)
(nrow(data)/3)*2
sort(data$lat, decreasing = FALSE)[14409]#  위도 하위 66%(중부)

data$lat1 <- ifelse(data$lat >= 47.1559 & data$lat <= 47.5139, "남부",
                        ifelse(data$lat > 47.5139 & data$lat <= 47.6463, "중부",
                               ifelse(data$lat > 47.6463 & data$lat <= 47.7776, "북부", "해당없음")))


data$lat.mid <- ifelse(data$lat1 == "중부", 1, 0)
data$lat.north <- ifelse(data$lat1 == "북부", 1, 0)



View(data)

##################################################
# 집값 min~1사분위는 1점
# 1~median은 2점
# median~3은 3점
# 3~max는 4점

# data2 <- data
# data2$zipcode <- as.factor(data2$zipcode)
# levels(data2$zipcode)
# 
# View(data2$price)
# tapply(data2$price,data2$zipcode,mean) # 각 우편번호동네에 평균 집값
# 
# View(data2)
# 
# summary(data2$price)
# 
# data2$zipcode <- ifelse(data2$price >= 11.23 & data2$price <= 12.68, 1,
#                       ifelse(data2$price > 12.68 & data2$price <= 13.02,  2,
#                              ifelse(data2$price > 13.02 & data2$price <= 13.38, 3, 4)))
# 
# quantile(data2$price)
# 
# View(data2$zipcode)
# colnames(data)
#################################

# 9. zipcode
boxplot(data.train[, "price"] ~ data.train[,"zipcode"], main = "Price vs zipcode")
# zipcode마다 차이가 있어 factor로 저장
data.train$zipcode <- as.factor(data.train$zipcode)
# 70 여개
table(data.train$zip) # 데이터갯수가 너무 많거나 적게 분포되어있지 않아 그대로 쓰자

##################################


data$zipcode1<-1:nrow(data)
for(i in 1:nrow(data)){
  data$zipcode1[i]<-mean((data %>% filter(data$zipcode[i]==zipcode))$price)
}

View(data)

data$zipcode1 <- round(data$zipcode1,digits = 3)
quantile(data$zipcode1)

data$zipcode2 <- ifelse(data$price >= 12.338 & data$price <= 12.723, "하",
                        ifelse(data$price > 12.723 & data$price <= 13.074,  "중",
                               ifelse(data$price > 13.074 & data$price <= 13.283, "상", "최상")))


data$zipcode.MidGrade <- ifelse(data$zipcode2 == "중", 1, 0)
data$zipcode.GoodGrade <- ifelse(data$zipcode2 == "상", 1, 0)
data$zipcode.ThebestGrade <- ifelse(data$zipcode2 == "최상", 1, 0)

View(data)


data.test$zipcode1<-1:nrow(data.test)
for(i in 1:nrow(data.test))
{
  data.test$zipcode1[i]<-mean((data.train %>% filter(data.test$zipcode[i]==zipcode))$price)
}

data.train$zipcode1<-1:nrow(data.train)
for(i in 1:nrow(data.train))
{
  data.train$zipcode1[i]<-mean((data.train %>% filter(data.train$zipcode[i]==zipcode))$price)
}
View(data.train)








###############################################지도 출력
library(shiny)
library(leaflet)
library(RColorBrewer)


coords <- data[,c(3,18,19)]



ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Price", min(coords$price), max(coords$price),
                            value = range(coords$price), step = 100000
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    coords[coords$price >= input$range[1] & coords$price <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, coords$price)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(coords) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~price/10000, weight = 1, color = "#777777",
                 fillColor = ~pal(price), fillOpacity = 0.7, popup = ~paste(price)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = coords)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~price
      )
    }
  })
}

shinyApp(ui, server)

