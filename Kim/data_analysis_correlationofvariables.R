# data_analysis2
# analysis on correlation of all variable regarding house price
# reference to https://www.kaggle.com/amitdhakre13/eda-linear-regression-k-fold-cv-adj-r2-0-87/notebook



# Quick display of two cabapilities of GGally, to assess the distribution and correlation of variables 
install.packages("GGally")

library(GGally)
library(pacman)
pacman:: p_load(Metrics, car, corrplot, caTools, ggplot2, DAAG)

## correlation of all variables
setwd("/Users/DK/Documents/programming/Github/Regression Analysis/rawdata/")
data <- read.csv("kc_house_data.csv")

# normalized skewed price data
data$price <- log(data$price)

data$date = substr(data$date, 1, 6)
# Converting it to numeric as we can only use numeric values for corrleation
data$date = as.numeric(as.character(data$date))

# delete id column
data$id <- NULL

corr = cor(data[, 1:20])
corrplot(corr, method = "color", cl.pos = 'n', rect.col = "black",  tl.col = "black", addCoef.col = "black", number.digits = 2, number.cex = 0.50, tl.cex = 0.9, cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))
# Corrleations higher than 0.5
# bathrooms, sqft_living, grde, sqft_abve, sqft_living 15

