# Linear Regressions with Danicia and Tom
## Severe thunderstorm watch edition
### Earth Day 4/22/2023 Evening.
#### Author: Tom McKeon, MPH

#Load Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)

install.packages("ggfortify")
library(ggfortify)

## Loading csv files.
income<- read.csv("C:/Users/ureka/OneDrive/Documents/Stata 2022/Danicia_stats/COMPLETE_income50_75k.csv", header = TRUE)
poverty<- read.csv("C:/Users/ureka/OneDrive/Documents/Stata 2022/Danicia_stats/COMPLETE_povertytable.csv", header = TRUE)
homeown<- read.csv("C:/Users/ureka/OneDrive/Documents/Stata 2022/Danicia_stats/COMPLETE_homeowner_occupied.csv", header = TRUE)
foodstamps<- read.csv("C:/Users/ureka/OneDrive/Documents/Stata 2022/Danicia_stats/COMPLETE_percentfoodstamps.csv", header = TRUE)
mobility<- read.csv("C:/Users/ureka/OneDrive/Documents/Stata 2022/Danicia_stats/mobility.csv", header = TRUE)
insurance<- read.csv("C:/Users/ureka/OneDrive/Documents/Stata 2022/Danicia_stats/healthinsurance.csv", header = TRUE)

## Check it out.
income
poverty
homeown
foodstamps
mobility
insurance

plot(wbindex$index)
typeof(wbindex$percentPoverty)

## This merges each table one at a time. With each iteration only keeping observations that exisit in BOTH tables, while excluding those that only exist in one. 
merged_table <- merge(income, poverty, by = "GEO_ID")
merged_table <- merge(merged_table, homeown, by = "GEO_ID")
merged_table <- merge(merged_table, foodstamps, by = "GEO_ID")
merged_table <- merge(merged_table, mobility, by = "GEO_ID")
merged_table <- merge(merged_table, insurance, by = "GEO_ID")

wbindex <- merged_table 

wbindex$percentPoverty <- as.numeric(wbindex$percentPoverty)
wbindex$income <- as.numeric(wbindex$income)
wbindex$owneroccupied  <- as.numeric(wbindex$owneroccupied)
wbindex$foodstamps <- as.numeric(wbindex$foodstamps)
wbindex$mobility <- as.numeric(wbindex$mobility)
wbindex$healthinsurance <- as.numeric(wbindex$healthinsurance)

## Creating outcome variable
wbindex$index <- (wbindex$income + wbindex$percentPoverty + wbindex$owneroccupied + wbindex$foodstamps)/ 4


## Linear regression
model <- lm(index ~ mobility, data = wbindex)

# View summary of the model with R-squared value included
rsq <- summary(model)$r.squared
summary_text <- paste0("R-squared: ", round(rsq, 2))
summary(model)
mtext(summary_text, side = 1, line = -2)

# Check model assumptions and residuals in a grid of plots
par(mfrow = c(2, 2))
plot(model)

# Create a scatter plot of the residuals with regression line and R-squared value
resid_df <- data.frame(Expected = fitted(model), Residuals = residuals)
ggplot(resid_df, aes(x = Expected, y = Residuals)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Residuals vs. Fitted", subtitle = summary_text, x = "Fitted values", y = "Residuals")
