#1) histogram은 hist(table(x))여야 그려지고 2) correlation matrix는 cor(data)를 넣으시면 되고, 3) 상관분석#(correlation test)는 cor.test(y,x, method="pearson")입니다!
setwd("c:/R/FastCampus/project/")


library(nortest) # 데이터가 5000개 이상.
library(dplyr)

# 파일 읽기
par(mfrow=c(2,2))


###################################################################################
house <- read.csv(file   = "c:/R/FastCampus/project/kc_house_data.csv",
                 header = TRUE)
portion <- house[,c(3,9:12)]
###################################################################################

# 각 variable별로 histogram 작성

variables <- c("waterfront","view","condition","grade")
for( i in 1:4 ){
  hist(table(portion[,i]), main = paste("Histogram of", variables[i]))
}
###################################################################################

# 귀무가설 : View(waterfront)의 유무와 상관없이 price에는 차이가 없다.(mu1 = mu2)
# 대립가설 : View(waterfront) 유무가 price에 영향을 미친다.(mu1>mu2)


house$view.group <- ifelse(house$view == 0,
                             "0", 
                             "1")

by(house$price, house$view.group, ad.test) # 정규성x

wilcox.test(house$price ~ house$view.group, # p-value : 1 이므로 대립가설 채택
            alternative = "greater")

# waterfront 관련


house$waterfront.group <- ifelse(house$waterfront == 0,
                           "0", 
                           "1")
by(house$price, house$waterfront.group, ad.test) # 정규성x

wilcox.test(house$price ~ house$waterfront.group, # p-value : 1 이므로 대립가설 채택
            alternative = "greater")




# condition, grade 높을 수록 좋음.
house$condition.group <- ifelse(house$condition == 1,
                                 "1", 
                                 "2")
by(house$price, house$condition.group, ad.test) # 정규성x

wilcox.test(house$price ~ house$condition.group, # p-value : 1 이므로 대립가설 채택
            alternative = "greater")


house$grade.group <- ifelse(house$grade == 7,
                                "7", 
                                "8")

by(house$price, house$grade.group, ad.test) # 정규성x

wilcox.test(house$price ~ house$grade.group, # p-value : 1 이므로 대립가설 채택
            alternative = "greater")


# waterfront랑 view가 0이 아닌 값들을 모아서 히스토그램
par(mfrow=c(2,1))

###################################################################################

is_waterfront <- dplyr::select(house, waterfront) %>%
  dplyr::filter(waterfront != 0)
hist(table(is_waterfront))



is_view <- dplyr::select(house, view) %>%
  dplyr::filter(view != 0)
hist(table(is_view))

table(house$waterfront)
###################################################################################

# scatterplot matrix
scatterplot_matrix <- pairs(portion)

## 보면 sqft_above와 sqft_basement와 price는 어느 정도의 선형관계를 보인다. (Y와 X간)
## sqft_above와 sqft_basement도 어느 정도의 선형관계를 보인다.

# correlation matrix
correlation_matrix <- cor(portion)

# correlation test
cor.test(portion$waterfront, portion$view, method="pearson") # 유의하다(significant)
cor.test(portion$view, portion$price, method="pearson") # 유의하다(significant)

# 회귀분석은 계수(coefficient) 정확한 추정을 위해 OLS를 가정하는데, 이 때의 가정 중
# 하나는 독립변수(X)들끼리 독립이라는 것이다. 이로 볼 때, yr_built와 sqft_above가
# 상관성이 있다는 것은 multicollinearity(다중공선성) 문제가 발생할 수 있다.