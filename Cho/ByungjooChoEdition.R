library(corrplot)
library(rgl)
library(car)
library(ggplot2)
library(psych)
library(writexl)
library(caret)
library(dplyr)
library(car)
library(gvlma)
library(lm.beta)
library(nortest)
library(dplyr)
setwd("C:/Users/Master/Documents/rds/RegressionAnalysis_KingCounty/Cho")
data <- read.csv("kc_house_data.csv")
par(mfrow = c(1,1))
options(scipen = 10000)



###################### 변수의 변환 ###############

glimpse(data.train)
car::powerTransform(data$sqft_lot15)
summary(car::powerTransform(data$sqft_lot15))



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
housesales.lm <- lm(price ~ bedrooms + bathrooms + sqft_living + floors + waterfront + view + grade + sqft_above + lat + sqft_living15, data = data.train)
summary(housesales.lm) 
#결정계수 : 0.734


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



