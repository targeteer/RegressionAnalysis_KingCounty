#1) histogram은 hist(table(x))여야 그려지고 2) correlation matrix는 cor(data)를 넣으시면 되고, 3) 상관분석#(correlation test)는 cor.test(y,x, method="pearson")입니다!
setwd("C:/Users/Master/Documents/rds/RegressionAnalysis_KingCounty/Cho")

library(nortest) # 데이터가 5000개 이상.
library(dplyr)
library(nparcomp)
library(ggplot2)
library(car)
library(lm.beta)
# 파일 읽기
par(mfrow=c(1,1))


###################################################################################
house <- read.csv(file   = "kc_house_data.csv",
                 header = TRUE)
portion <- house[,c(3,9:12)]
View(portion)
################################################################################

#### 변수

# - 종속변수 : "price"
# - 독립변수 
# 질적자료 : "waterfront", "condition", "grade" 
# 양적자료 : "view"
# 특이사항 : "condition"과 "grade"는 질적자료와 양적자료 성질을 동시에 갖음.
#################################################################################


## 질적변수 빈도와 백분율, 막대그래프와 원그래프
# waterfront
table(portion$waterfront) # 빈도
prop.table(table(portion$waterfront)) #백분율
barplot(table(portion$waterfront), # 막대그래프
        main = "waterfont freq",
        ylim = c(0,25000))
pie(table(portion$waterfront), # 원그래프
    radius = 1,
    clockwise = TRUE,
    init.angle = 90)

# condition
portion$condition.add <- ifelse(portion$condition == 1,
                             "매우나쁨", 
                              ifelse(portion$condition == 2,
                                     "나쁨",
                                     ifelse(portion$condition == 3,
                                            "보통",
                                            ifelse(portion$condition == 4,
                                                   "좋음",
                                                   ifelse(portion$condition == 5,
                                                      "매우좋음", "해당없음")))))



sort(table(portion$condition), decreasing = TRUE) # 빈도
prop.table(table(portion$condition)) #백분율
barplot(table(portion$condition), # 막대그래프
        main = "condition freq",
        ylim = c(0,15000))


a <- sort(table(portion$condition),decreasing = TRUE)
pct <- round(a/sum(a)*100,1)
lab1 <- c("보통", "좋음", "매우좋음", "나쁨", "매우나쁨")
lab2 <- paste(lab1,"\n",pct,"%")

pie(a, # 원그래프
    radius = 1,
    clockwise = TRUE,
    init.angle = 90,
    labels = lab2)



# grade
portion$grade.add <- portion$grade


table(portion$grade) # 빈도
prop.table(table(portion$grade)) #백분율
barplot(table(portion$grade), # 막대그래프
        main = "grade freq",
        ylim = c(0,10000))

pie(table(portion$grade), # 원그래프
    radius = 1,
    clockwise = TRUE,
    init.angle = 90)

portion$grade.add <- ifelse(portion$grade== 1,
                                "1점", 
                                ifelse(portion$grade == 2,
                                "2점",
                                ifelse(portion$grade == 3,
                                "3점",
                                ifelse(portion$grade == 4,
                                "4점",
                                ifelse(portion$grade == 5,
                                "5점",
                                ifelse(portion$grade == 6,
                                "6점",
                                ifelse(portion$grade == 7,
                                "7점",
                                ifelse(portion$grade == 8,
                                "8점",
                                ifelse(portion$grade == 9,
                                "9점",
                                ifelse(portion$grade == 10,
                                "10점",
                                ifelse(portion$grade == 11,
                                "11점",
                                ifelse(portion$grade == 12,
                                "12점",
                                ifelse(portion$grade == 13,
                                "13점","해당없음")))))))))))))

View(portion)

## 양적변수 histogram 및 상자그림(이상치)
# price구간 나누기
interval.count <- 1 + 3.3*log10(length(house$price))
price.range <- diff(range(house$price))
interval.wide <- price.range/interval.count
min(house$price)
max(house$price)

# price

hist(portion$price,
     ylim = c(0, 14000),
     breaks = seq(from = 0, to = 800000, by = 500000))

mode(portion$waterfront)
portion$waterfront <- ifelse(portion$waterfront == 0,
                             "No-waterfront", 
                             "Yes-waterfront")
boxplot(portion$price)
boxplot(portion$price ~ portion$waterfront) # price와 waterfront의 상자그림

# view
hist(portion$view)
boxplot(portion$view)




variables <- c("waterfront","view","condition","grade")
for( i in 1:4 ){
  hist(table(portion[,i]), main = paste("Histogram of", variables[i]))
}


###################################################################################
# Price와 waterfront(0,1) TwoSample Test


# 귀무가설 : waterfront의 유무와 상관없이 price에는 차이가 없다.(mu1 = mu2)
# 대립가설 : waterfront 유무가 price에 영향을 미친다.(mu1>mu2)


by(portion$price, portion$waterfront, ad.test) # 정규성x

wilcox.test(portion$price ~ portion$waterfront, # p-value : 1 이므로 귀무가설 채택
            alternative = "greater")

# waterfront의 유무는 price에 통계적으로 유의한 차이는 없는 것으로 나타났다.

###################################################################################
# Price와 Condition(1~5) Anova Test


# 귀무가설 : 집 상태에 따라(Condition) price는 차이가 없다.
# 대립가설 :집 상태에 따라(Condition) price는 차이가 있다.


by(portion$price, portion$condition, ad.test) # 정규성x


kruskal.test(price ~ condition, data = portion) # p-value : 0.000 이므로 대립가설 채택

# condition에따라 Price 통계적으로 유의한 차이가 있는 것으로 나타났다.
#(chi-squared = 260.85, p<0.001)

 # nparcomp::nparcomp(price ~ condition, ---- 에러
 #                    data = portion,
 #                    type = "Tukey")

###################################################################################
# Price와 grade(1~13) Anova Test


# 귀무가설 : 등급에 따라(grade) price는 차이가 없다.
# 대립가설 : 등급에 따라(grade) price는 차이가 있다.

nortest::ad.test(portion$price) 
nortest::ad.test(portion$grade) # 정규성x

kruskal.test(price ~ grade, data = portion) # p-value : 0.00 이므로 대립가설 채택

# grade는 price에 통계적으로 유의한 차이는 있는 것으로 나타났다.
#(chi-squared = 9694.8, P<0.001)

   
str(portion)

                                                                      
  # nparcomp::nparcomp(price ~ grade, 
  #                  data = portion,
  #                  type = "Tukey") ---- 에러

  
  
  
###################################################################################
# Price와 view(1~4) CorrelationAnalysis ---- 과연 정확한 분석은??

# 산점도

ggplot2::ggplot(data = portion,
                mapping = aes(x = view, y = price)) + 
  geom_point(aes(col = "red")) +
  theme_classic() +
  labs(title = "Scatter Plot of view & price",
       x     = "sum of view",
       y     = "price of house") +
  theme(plot.title = element_text(size = 20), # 제목들 크기
        axis.title.x    = element_text(size = 10),
        axis.title.y   = element_text(size = 10))
# 결론 : price와 view간의 연관성은 없어 보인다.

# 두 변수의 상관계수
cor(portion$price, portion$view, method = "kendall")
# 결론 : 0.238, 약한관련성이 있다.



# 상관분석
# 귀무가설 : price와 view 간에 관련성이 없다.
# 대립가설 : price와 view 간에 관련성이 있다.

ad.test(portion$price)
ad.test(portion$view) # 정규분포를 따르지 않음

cor.test(portion$price, portion$view, method = "kendall")
cor.test(portion$price, portion$view, method = "spearman")

# 결론 : 유의확률이 0.000 이므로 유의수준 0.05에서
# price와 view 간에는 통계적으로 유의한
# 양의 상관관계가 있는 것으로 나타났다.

###################################################################################
# 선형회귀분석(price와 view가 양적 변수 2개 존재)

PriceView.lm <- lm(price ~ view, data = portion)
PriceView.lm
summary(PriceView.lm)

# 1단계 : 회귀모형은 타당
# F-statistic: 4050 on 1 and 21611 DF,  p-value: 1.49e-12
# 결론 : 유의확률이 0.000 이므로 유의수준 0.05에서
# 회귀모형은 통계적으로 타당하다.

# 2단계 : 독립변수는 종속변수에게 영향을 주는가?
# view는 price에 통계적으로 유의한 영향을 주는 것으로 나타났다.
# ( t = 63.64, p<0.001)

# 3단계 : 독립변수는 종속변수에게 어떠한 영향을 주는가?
#          Estimate     
# view     190335
# 회귀계수(beta1 : Coefficient of Regression) : 190335
# view가 1개씩 증가하면 price는 약 190335 정도 증가시킨다

# 4단계 : 회귀모형의 설명력
# Multiple R-squared:  0.1578
# view가 price의 다름을 약 15.78% 정도 설명하고 있다.


# 5단계 : 예측(Prediction)
predict(PriceView.lm, newdata = data.frame(view = 1))
predict(PriceView.lm, newdata = data.frame(view = c(1,2,3,4)))


###################################################################################
# condition과 grade의 관련성(Chi-Square)
table(portion$condition, portion$grade) # 교차표

chisq.test(table(portion$condition, portion$grade))
# 결론 : 유의확률이 0.000이므로 유의수준 0.05에서
# condition은 grade에 통계적으로 유의한 관련성이 있는 것으로 나타났다.

###################################################################################

## waterfront, condition, grade 더미변수로 변환(질적자료 => 양적자료)

portion$waterfront.Yes <- ifelse(portion$waterfront == 1, 1, 0)

portion$condition.2 <- ifelse(portion$condition == 2, 1, 0)
portion$condition.3 <- ifelse(portion$condition == 3, 1, 0)
portion$condition.4 <- ifelse(portion$condition == 4, 1, 0)
portion$condition.5 <- ifelse(portion$condition == 5, 1, 0)

portion$grade.2 <- ifelse(portion$grade == 2, 1, 0)
portion$grade.3 <- ifelse(portion$grade == 3, 1, 0)
portion$grade.4 <- ifelse(portion$grade == 4, 1, 0)
portion$grade.5 <- ifelse(portion$grade == 5, 1, 0)
portion$grade.6 <- ifelse(portion$grade == 6, 1, 0)
portion$grade.7 <- ifelse(portion$grade == 7, 1, 0)
portion$grade.8 <- ifelse(portion$grade == 8, 1, 0)
portion$grade.9 <- ifelse(portion$grade == 9, 1, 0)
portion$grade.10 <- ifelse(portion$grade == 10, 1, 0)
portion$grade.11 <- ifelse(portion$grade == 11, 1, 0)
portion$grade.12 <- ifelse(portion$grade == 12, 1, 0)
portion$grade.13 <- ifelse(portion$grade == 13, 1, 0)

View(portion2)

portion2 <- dplyr::select(portion, c(1,3,8:24))
portion.model <- lm(price ~ ., data = portion2)
summary(portion.model)

###################################################################################

## 다중선형 회귀분석
# 종속변수 : "price"
# 독립변수 : "waterfront", "view", "condition", "grade"
portion2.lm <- lm(rating ~., data = attitude)
portion2.lm <- lm(price ~., data = portion2)
summary(portion2.lm)

# 1단계 : 회귀모형은 타당한가?
# 귀무가설 : 회귀모형은 타당하지 않다.
# 대립가설 : 회귀모형은 타당하다.
# F-statistic:  1845 on 17 and 21595 DF,  p-value: 0.000
# 결론 : 유의확률이 0.000 이므로 유의수준 0.05에서
# 회귀모형은 타당하다. SSR 자유도 : 17, SSE 자유도 : 21595


# 2단계 : 각각의 독립변수는 종속변수에게 영향을 주는가?
# 귀무가설 : 각각의 독립변수는 종속변수에게 영향을 주지 않는다.
# 대립가설 : 각각의 독립변수는 종속변수에게 영향을 준다.

#                 Estimate Std.Error  t value  Pr(>|t|)    
# view              73435       2361  31.107   < 2e-16 ***
# waterfront.Yes   567550      20180  28.124   < 2e-16 ***
# condition.2      -35284      47214  -0.747   0.454881    
# condition.3      -28605      43917  -0.651   0.514831    
# condition.4       28520      43952   0.649   0.516410    
# condition.5      123167      44198   2.787   0.005329 ** 
# grade.2              NA         NA      NA       NA    
# grade.3           43907     274383   0.160   0.872866    
# grade.4           71324     242271   0.294   0.768457    
# grade.5           73932     238768   0.310   0.756837    
# grade.6          140742     238598   0.590   0.555283    
# grade.7          248355     238593   1.041   0.297926    
# grade.8          384739     238604   1.612   0.106877    
# grade.9          606105     238635   2.540   0.011096 *  
# grade.10         881105     238696   3.691   0.000224 ***
# grade.11        1267153     238892   5.304   1.14e-07 ***
# grade.13        3451860     247340  13.956   < 2e-16 ***
# grade.12        1876022     239902   7.820   5.53e-15 ***

# view, waterfront, condition5, grade9 ~ 13 : 대립가설

# 3단계 : 각각의 독립변수는 종속변수에게 어떠한 영향을 주는가? 위에 Estimate 참조

# 4단계 : 회귀모형의 설명력 = 독립변수들의 설명력
# Multiple R-squared:  0.5923,	Adjusted R-squared:  0.592


# 다중선형 회귀분석에서 고려해야 할 사항
# (1) 변수선택
# 최종 모형에 어떤 독립변수들로 구성할 것인가?
portion2.forward <-  step(portion2.lm, direction = "forward") # AIC = 534519.3
portion2.backward <- step(portion2.lm, direction = "backward") # AIC = 534509.9
portion2.stepwise <- step(portion2.lm, direction = "both") # AIC = 534509.9
summary(portion2.stepwise)

View(portion3) <- dplyr::select(portion2, c(1,2,3,6,7,12:19))

portion2.final.model <- lm(price ~ ., data = portion3)

summary(portion2.final.model)
# 결과 : 약 59.23%로 설명하고 있다.

###########################################################

# (2) 다중공선성(Multicollinearity)
# 독립변수들 간에 상관관계가 있는 것
# 회귀분석의 가정 : 다중공선성이 없어야 함.
car::vif(portion2.final.model)
# grade.7, grade.8 공산성 존재

###########################################################
# (3) 독립변수들의 영향력 비교
# 최종 모형에서 2개의 이상의 독립변수가 종속변수에게
# 영향을 준다면, 어떤 독립변수가 가장 큰 영향을 줄까?

lm.beta::lm.beta(portion2.final.model) # grade 10점이 가장 큰 영향을 줌





