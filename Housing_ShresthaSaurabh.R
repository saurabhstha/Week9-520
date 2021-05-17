[# Assignment: Housing Dataset
# Name: Shrestha, Saurabh
# Date: 2021-05-14
install.packages("dplyr")
library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(outliers)
library(lmtest)
library(car)

# Set working directory
setwd("C:/Users/Saurabh/Desktop/DSC 520/Week5")

# Load the housing dataset
housing_data = read_excel("week-6-housing.xlsx")
head(housing_data)

# 3b
# i.


# GroupBy, Summarize, Mutate, Filter, Select, and Arrange
colnames(housing_data)[2] <- "Sale_Price"
housing_data %>% group_by(Sale_Price)
housing_data %>% group_by(Sale_Price) %>% summarize(mean(square_feet_total_living))

# Mutate
housing_data %>% mutate(price_per_square_ft = Sale_Price/square_feet_total_living)

housing_data %>% summarize(mean(Sale_Price))

# Filter 
housing_data %>% filter(Sale_Price < 300000)

# Select 
housing_data %>% select(`Sale Date`,Sale_Price,square_feet_total_living,bedrooms)

# Arrange
housing_data %>% arrange(Sale_Price, square_feet_total_living, bedrooms)

library(purrr)
# purr functions
zip_data <- compact(housing_data$zip5)
low_price <- housing_data$Sale_Price %>% keep(function(x) x < 250000)
head(zip_data)
head(low_price)

# cbind

baths <- cbind(housing_data$bath_full_count, housing_data$bath_half_count,
               housing_data$bath_3qtr_count)
head(baths)

#rbind

expensive_house <- housing_data %>% filter(Sale_Price >= 250000)
less_expensive_house <- housing_data %>% filter(Sale_Price < 250000)
# joined these two dfs to one
house_range<- rbind(expensive_house,less_expensive_house)
head(house_range)

# string concatenation(concatenating addr_full, postalctyn and zip5)
library(stringr)

address_categories <- str_split(string = housing_data$addr_full,pattern = ' ')
housing_data$full_address <- paste(housing_data$addr_full,housing_data$postalctyn,housing_data$zip5,sep = ',')
head(housing_data %>% select(`Sale Date`, Sale_Price,full_address))

# ii. Create two variables; one that will contain the variables...
sale_price_lot_square_ft <- lm(Sale_Price ~ sq_ft_lot, data = housing_data)

sale_price_lot_square_ft
housing_indicators <-lm(Sale_Price ~ sq_ft_lot + sale_reason + building_grade + square_feet_total_living + bedrooms,
    data = housing_data)
housing_indicators


# iii. Execute a summary() function on two variables...
summary(sale_price_lot_square_ft)
summary(housing_indicators)

# iv. Considering the parameters...
housing_beta <- data.frame(housing_indicators$coefficients)
housing_beta
# The values indicate that each variable creates changes to the independent variable.

# v. Calculate the confidence intervals for the parameters ...
con_int <- confint(housing_indicators, level = 0.95)
# The confidence interval shows that our model will fall 95 percent of the time withinb the given range.

# vi. Assess the improvement of the new model...
anova(sale_price_lot_square_ft, housing_indicators)


# vii. Perform casewise diagnostics to identify outliers...
housing_data$residuals <- resid(housing_indicators)
housing_data$standardized.residuals <- rstandard(housing_indicators)
housing_data$studentized.residulas <- rstudent(housing_indicators)
housing_data$leverage <- hatvalues(housing_indicators)


# viii.Calculate the standardized residuals...
housing_data$large.residual <- housing_data$standardized.residuals >2 | housing_data$standardized.residuals < -2


# ix. Use the appropriate function..
sum(housing_data$large.residual)

# x. Which specific variables have large residuals...
large_residual <- housing_data %>% 
  group_by(large.residual) %>% 
  filter(large.residual == T)

# xi. Investigate further by calculating the leverage,...
housing_data$cooks.distance <- cooks.distance(housing_indicators)
housing_data$cooks.distance
housing_data$leverage <- hatvalues(housing_indicators)
housing_data$covaraince.ratios <- covratio(housing_data)

# xii. Perform the necessary calculations to assess...
independence <- dwt(housing_indicators)


# xiii. Perform the necessary calculations to assess the...
multi_coll <- vif(housing_indicators)
1/vif(housing_indicators)

# xiv. Visually check the assumptions related to the residuals...
plot(housing_indicators) 
# It is around normal distribution and could be said that model could 
# be accurate for general population.
ggplot(housing_data, aes(x = housing_data$dffit, y = housing_data$standardized.residuals)) + geom_point() + xlab("Fitted Values") + ylab("Standardized Residuals") + ggtitle("Fitted vs Standardized Residuals")

ggplot(housing_data, aes(x = standardized.residuals)) + geom_histogram(binwidth = 0.30, color = "Blue", fill = "Red") + xlab("Standardized Residuals") + ggtitle("Residuals Histogram")
# xv. Overall, is this regression model unbiased?...
# There doesn't seem to be any multi collinearity and this sample could be likely to be accurate for general population. 
