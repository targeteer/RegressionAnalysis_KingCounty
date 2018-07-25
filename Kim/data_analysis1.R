install.packages("parsedate")
install.packages("lubridate")

library(readxl)
library(dplyr)
library(psych)
library(parsedate)
library(lubridate)
library(RColorBrewer)

data <- read.csv("kc_house_data.csv", header = TRUE)


# 개인별 분석 범위
analysis <- data[,c(2,3:8)]


# 거래 날짜 parse
analysis$date.parsed <- parsedate::parse_date(analysis$date)

# 거래 날짜를 년,월,일,날로 구분
analysis$date.year <- lubridate::year(analysis$date.parsed)
analysis$date.month <- lubridate::month(analysis$date.parsed)
analysis$date.day <- lubridate::day(analysis$date.parsed)
analysis$date.ofweek <- lubridate::wday(analysis$date.parsed, label = TRUE)

analysis$date.yearmonth <- paste0(analysis$date.year,"-",analysis$date.month)

par(mfrow =c(1,1))

barplot(sort(table(analysis$date.year), decreasing = TRUE),
        main = "House Sales in King County by year",
        xlab = "year",
        ylab = "number of sales")

barplot(table(analysis$date.month),
        main = "House Sales in King County",
        xlab = "month",
        ylab = "number of sales")

# RcolorBrewer
color.palette <- RColorBrewer::brewer.pal(n=9, name = "PuBu")

#년월과 거래량의 관계 (여름~가을에 집중)
barplot(table(analysis$date.yearmonth),
        col = color.palette,
        main = "House Sales in King County by year",
        xlab = "Year-Month",
        ylab = "number of sales")


# 화장실 갯수와 방의 갯수 합
analysis$totalrooms <- analysis$bathrooms + analysis$bedrooms

# 방당 평균 가격 
analysis$priceperroom <- analysis$price / analysis$totalrooms

# 방의 평균가격의 5% 절사 평균
roomaverage <- mean(analysis$priceperroom, trim = 0.05)

# 방 갯수별 분류(방법 찾아볼 것)
analysis$priceperroom.group <- cut(analysis$totalrooms, breaks = c(0, 3, 7, 10, 35), right=FALSE)
levels(analysis$priceperroom.group) <- c("소형", "중형", "대형", "초대형")

sum(analysis$priceperroom.group == "소형")
sum(analysis$priceperroom.group == "중형")
sum(analysis$priceperroom.group == "대형")
sum(analysis$priceperroom.group == "초대형")

# 1층집, 복층집 분류
analysis$hasfloors.group <- cut(analysis$floors, breaks = c(0,1,5), right = TRUE)
levels(analysis$hasfloors.group) <- c("1층집", "복층집")

sum(analysis$hasfloors.group == "1층집")
sum(analysis$hasfloors.group == "복층집")

sum(analysis$hasfloors.group == "1층집" & analysis$priceperroom.group == "소형")
sum(analysis$hasfloors.group == "1층집" & analysis$priceperroom.group == "중형")
sum(analysis$hasfloors.group == "1층집" & analysis$priceperroom.group == "대형")
sum(analysis$hasfloors.group == "1층집" & analysis$priceperroom.group == "초대형")

# 방 1개당 가격 평균 및 층 갯수별 가격 평균
by(analysis$price, analysis$priceperroom.group, mean)
by(analysis$price, analysis$hasfloors.group, mean)

sum(analysis$hasfloors.group == "복층집" & analysis$priceperroom.group == "소형")
sum(analysis$hasfloors.group == "복층집" & analysis$priceperroom.group == "중형")
sum(analysis$hasfloors.group == "복층집" & analysis$priceperroom.group == "대형")
sum(analysis$hasfloors.group == "복층집" & analysis$priceperroom.group == "초대형")

mean(analysis$price[analysis$hasfloors.group == "1층집" & analysis$priceperroom.group == "중형"])
mean(analysis$price[analysis$hasfloors.group == "복층집" & analysis$priceperroom.group == "중형"])

# 주택 크기별 가격 평균(주택 크기 분류는 임의로 정함)
XLmeanprice <- mean(analysis[analysis$priceperroom.group == "초대형", "price"])
Lmeanprice <- mean(analysis[analysis$priceperroom.group == "대형", "price"])
Mmeanprice <- mean(analysis[analysis$priceperroom.group == "중형", "price"])
Smeanprice <- mean(analysis[analysis$priceperroom.group == "소형", "price"])

mean(analysis$price)

# vector for house sizes
housesizes <- c(XLmeanprice, Lmeanprice, Mmeanprice, Smeanprice)

#다른 데이터별 barplot(데이터 정제 필요)
par(mfrow = c(2,2))
for(i in 3:7){
    barplot(table(analysis[  ,i]),
         main = paste0("Barplot of ", colnames(analysis)[i] ))
}

