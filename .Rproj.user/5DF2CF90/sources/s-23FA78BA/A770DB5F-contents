# Author: Tan Nguyen

clean <- timelines_10_countries_en
library(dplyr)
# Replace `NA` for double columns
# https://stackoverflow.com/questions/10139284/set-na-to-0-in-r
# https://stackoverflow.com/questions/8161836/how-do-i-replace-na-values-with-zeros-in-an-r-dataframe
clean <- clean %>% mutate(across(where(is.double), ~ ifelse(is.na(.), 0, .)))

# http://www.sthda.com/english/wiki/f-test-compare-two-variances-in-r
res <- var.test(clean$`Total cases`, clean$`Total deaths`)
res$p.value
# Notes: I don't understand others information in res

# Correlation coefficient
# http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r


# Regression
a <- clean$`New cases`
b <- clean$`New deaths`
m1 <- lm(a ~ b)
summary(m1)
library(car)
crPlots(m1)
residualPlots(m1)