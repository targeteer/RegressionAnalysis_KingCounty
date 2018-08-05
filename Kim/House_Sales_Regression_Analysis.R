install.packages("GGally")
install.packages("corrplot")
install.packages("caret")
install.packages("car")
install.packages("lm.beta")
install.packages("nortest")
install.packages("gvlma")

library(gvlma)
library(nortest)
library(lm.beta)
library(car)
library(caret)
library(dplyr)
library(car)
library(corrploy)
library(GGally)
library(pacman)
pacman:: p_load(Metrics, car, corrplot, caTools, ggplot2, DAAG)

rm(list = ls())

# 데이터 불러오기
setwd("/Users/DK/Documents/programming/Github/Regression Analysis/rawdata/")
data <- read.csv("kc_house_data.csv")

# test, train 셋으로 나누기
data.index <- caret::createDataPartition(data$price, p = 0.8)
data.train <- data[unlist(data.index) , ]
data.test  <- data[-unlist(data.index) ,]

# price 변수 로그화 (안하면 결과가 엉망진창)
data.train$price <- log(data.train$price)
data.test$price <- log(data.test$price)

data.train$date = substr(data.train$date, 1, 6)
data.test$date = substr(data.test$date, 1, 6)

# 일단 data를 숫자로바꿔야 상관성 확인가능
data.train$date = as.numeric(as.character(data.train$date))
data.test$date = as.numeric(as.character(data.test$date))

# id 삭제(어차피 필요없음)
data.train$id <- NULL
data.test$id <- NULL

corr = cor(data.train[, 1:20])
corrplot::corrplot(corr, method = "color", cl.pos = 'n', rect.col = "black",  tl.col = "black", addCoef.col = "black", number.digits = 2, number.cex = 0.50, tl.cex = 0.9, cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))
# 상관계수 0.5이상
# bathrooms, sqft_living, grde, sqft_abve, sqft_living 15
# bathrooms : 0.55
# sqft_living : 0.7
# grade : 0.7
# sqft_above : 0.6
# sqft_living15 : 0.62

# 상관계수 0.1이하 삭제
data.train$sqft_lot = NULL
data.train$condition = NULL
data.train$long = NULL
data.train$sqft_lot15 = NULL

data.test$sqft_lot = NULL
data.test$condition = NULL
data.test$long = NULL
data.test$sqft_lot15 = NULL
# 위도와 우편번호는 factor 와 corplot사용해보자(위도는 상관성이 조금있고 우편번호는 지역을 나타내니 지역분류를 위해 일단 냅두자)

# 상관계수 높은 변수로 모델 생성
housesales.lm <- lm(price ~ bathrooms + sqft_living + grade + sqft_above + sqft_living15, data = data.train)
summary(housesales.lm)

# 각 변수 별 R^2 검사
housesales.lm.all <- lm(price ~ ., data= data.train)
summary(housesales.lm.all)

# bedrooms -  0.118 
# bathrooms - 0.3034
# sqft_living - 0.4835
# sqft_lot - 0.009879
# floors - 0.0964
# waterfront - 0.030
# view  - 0.12
# condition - 0.0015
# grade - 0.4951
# sqft_above - 0.3621
# sqft_basement - 0.1004
# yr_built = 0.0065
# yr_renovated = 0.013
# zipcode= 0.0014
# lat = 0.2017
# long = 0.00244
# sqft_living15 = 0.3855
# sqft_lot 0.008343
housesales.lm.all <- lm(price ~ ., data=data.train)

# R^2 0.3이상
housesales.lm.all <- lm(price ~ bathrooms + sqft_living + grade + sqft_living15, data=data.train)
summary(housesales.lm.all)


# VIFs between two variables
# bathrooms sqft_living 
# 2.322987    2.322987 
# bathrooms     grade 
# 1.792763  1.792763 
# bathrooms sqft_above 
# 1.885705   1.885705 
# bathrooms sqft_living15 
# 1.477858      1.477858 
# sqft_living       grade 
# 2.390732    2.390732
# sqft_living  sqft_above 
# 4.318192    4.318192 
# sqft_living sqft_living15 
# 2.337386      2.337386 
# grade sqft_above 
# 2.333284   2.333284 
# grade sqft_living15 
# 2.035239      2.035239 
# sqft_above sqft_living15 
# 2.153474      2.153474 

# 위 결과로 sqft_above 제거(가장 R^2높은 sqft_living의 vif를 줄이기 위해, 그리고 상관성이 높은 변수 배제)
vif(housesales.lm.all)

# factor화를 통해 변수 사용여부 확인
colnames(data.train)

# 1. bedrooms(keep it as factor)
par(mfrow=c(1,1))
boxplot(data.train[ ,"price"] ~ data.train[,"bedrooms"], main = "Price vs. Bedrooms")
# bedrooms 11, 13 제거
print(subset(data.train, data.train$bedrooms > 10))
# bedrooms에서 이상치 제거
data.train <- data.train[data.train$bedrooms <= 10,]
data.train$bedrooms <- as.factor(data.train$bedrooms)

data.test <- data.test[data.test$bedrooms <= 10,]
data.test$bedrooms <- as.factor(data.test$bedrooms)

# 2. floors(keep it as factor)
boxplot(data.train[,"price"] ~ data.train[,"floors"], main = "Price vs floors")
data.train$floors <- as.factor(data.train$floors)

data.test$floors <- as.factor(data.test$floors)


# 3. waterfront
boxplot(data.train[,"price"] ~ data.train[,"waterfront"], main = "Price vs waterfront")
print(subset(data.train, data.train$waterfront==0)) #2만여개
print(subset(data.train, data.train$waterfront==1)) #101개

data.train[data.train$waterfront==0, ]
data.train %>% 
    filter(waterfront==0) %>% 
    select(price) %>% unlist() %>% mean()
data.train %>% 
    filter(waterfront==1) %>% 
    select(price) %>% unlist() %>% mean()
# 평균값의 차이가 크니 factor로 저장
data.train$waterfront <- as.factor(data.train$waterfront)

data.test$waterfront <- as.factor(data.test$waterfront)

# 4. view
boxplot(data.train[,"price"] ~ data.train[,"view"], main = "Price vs floor")
for(i in 0:4){
    data.train %>% 
        filter(view == i) %>% 
        select(price) %>% unlist() %>% mean() %>% print()
}
# 0 : 12.99029
# 1 : 13.46064
# 2 : 13.43744
# 3 : 13.63316
# 4 : 14.01928
# factor로 저장
data.train$view <- as.factor(data.train$view)

data.test$view <- as.factor(data.test$view)

# 5. sqft_above
boxplot(data.train[,"price"] ~ data.train[,"sqft_above"],main = "Price vs sqft_above")
# factor하기에 변수의 폭이 넓고 위에서 다중공산성 문제가 있었으므로 제외
data.train$sqft_above = NULL

data.test$sqft_above = NULL

# 6. sqft_basement
boxplot(data.train[,"price"] ~ data.train[,"sqft_basement"], main = "Price vs sqft_basement")
# price에 영향이 없으므로 제외
data.train$sqft_basement = NULL

data.test$sqft_basement = NULL


# 7. yr_built
boxplot(data.train[,"price"] ~ data.train[,"yr_built"], main = "Price vs yr.built")
# price에 영향이 없으므로 제외
data.train$yr_built = NULL

data.test$yr_built = NULL


# 8. yr_renovated
boxplot(data.train[,"price"] ~ data.train[,"yr_renovated"], main = "Price vs yr.renovated")
# price에 영향이 없으므로 제외
data.train$yr_renovated = NULL

data.test$yr_renovated = NULL

# 9. zipcode
boxplot(data.train[, "price"] ~ data.train[,"zipcode"], main = "Price vs zipcode")
# zipcode마다 차이가 있어 factor로 저장
data.train$zipcode <- as.factor(data.train$zipcode)
# 70 여개
table(data.train$zip) # 데이터갯수가 너무 많거나 적게 분포되어있지 않아 그대로 쓰자

data.test$zipcode <- as.factor(data.test$zipcode)

  
# 10. lat
boxplot(data.train[, "price"] ~ data.train[, "lat"], main = "Price vs lat")
# 복잡해서 그냥 뺌(zipcode가 대신)
data.train$lat = NULL

data.test$lat = NULL

# 11. date 제외
data.train$date = NULL

data.test$date = NULL

colnames(data.train)
housesales.lm.final <- lm(price ~ ., data= data.train)

summary(housesales.lm.final)
# 1. 회귀분석은 타당한가(타당함)
# F-statistic:  1558 on 93 and 21517 DF,  p-value: < 2.2e-16
# 결론: 타당함

# 2. 독립변수별 영향력 확인?(스킵)

# 3. prediction(예측은 나중에 밑에서)

# 변수선택 : (사실 이게 잘...여기서는 안쓰이는듯)
# 1) FSB
housesales.fsb <- step(housesales.lm.final, direction = "forward")

# 2) BEM
housesales.bem <- step(housesales.lm.final, direction = "backward")

# 3) SSM
housesales.ssm <- step(housesales.lm.final, direction = "both")

summary(housesales.ssm)

car::vif(housesales.lm.final)

# 독립변수들의 영향력 확인
lm.beta(housesales.lm.final)

par(mfrow = c(2,2))
plot(housesales.lm.final)
# QQplot에서 점들이 직선에 가깝게 만족함

# 1. 잔차의 정규성 가정
ad.test(housesales.lm.final$residuals)
# A = 116.89, p-value < 2.2e-16

# 2. 독립성 가정 (이거 돌리면 r 꺼지니까 돌리지마세요)
car::durbinWatsonTest(housesales.lm.final)
 # lag Autocorrelation D-W Statistic p-value
# 1      0.01152357      1.976888   0.076

# 3. 등분산성 검정
car::ncvTest(housesales.lm.final)
# Chisquare = 1.420945    Df = 1     p = 0.2332479 

# 4. 에러에 대한 전반적인 가정에 검정.(예 뭐..결과가 이런데 어쩌겠음..ㅠ)
summary(gvlma::gvlma(housesales.lm.final))
# Value   p-value                   Decision
# Global Stat        5662.46 0.0000000 Assumptions NOT satisfied!
#     Skewness            183.29 0.0000000 Assumptions NOT satisfied!
#     Kurtosis           5254.03 0.0000000 Assumptions NOT satisfied!
#     Link Function       214.09 0.0000000 Assumptions NOT satisfied!
#     Heteroscedasticity   11.04 0.0008903 Assumptions NOT satisfied!


# 예측
data.predict <- predict(housesales.lm.final, newdata = data.test)

summary(data.predict)
summary(data.test$price)

#아직 정확성 예측 안했습니다.
