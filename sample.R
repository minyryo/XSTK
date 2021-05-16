options(max.print = 10)
library(xlsx)
library(openxlsx)
library(dplyr)
colnames(realEstate)
colnames$... <- NULL #remove a col

colnames(realEstate)[2] = "X2_age"
colnames(realEstate)[3] = "X3_MRT_nearest"
colnames(realEstate)[4] = "X4_CVS"
colnames(realEstate)[5] = "X5_lat"
colnames(realEstate)[6] = "X6_long"
colnames(realEstate)[7] = "Y_price"

tapply(realEstate$X3_MRT_nearest, summary)
tapply(realEstate$X3_MRT_nearest, sd)

library(psych)
 # describeBy(realEstate ~ X3_MRT_nearest) liet ke theo nhom
describe(realEstate$X3_MRT_nearest) # liet ke
# sapply(realEstate, median) neu co thieu bien nao
table(realEstate$X4_CVS)

hist(realEstate$Y_price, main = "Histogram of Y_price", xlab = "price")

boxplot(realEstate$Y_price~realEstate$X4_CVS, outline = FALSE,
        main = "Boxplot of CVS ~ price", xlab = "CVS", ylab = "price")

pairs(realEstate) # prepare for lm()

# price and CVS has no linearity relationship so will be excluded from model
age <- realEstate$X2_age
mrt <- realEstate$X3_MRT_nearest
lat <- realEstate$X5_lat
long <- realEstate$X6_long 
price <- realEstate$Y_price # avoid error for predict cause of using "$"
m1 <- lm(price ~ age)
m2 <- lm(price ~ mrt)
m3 <- lm(price ~ lat)
m4 <- lm(price ~ long)
m_fin <- lm(price ~ age + mrt + lat + long)
m_comp <- lm(price ~ age + mrt + lat) # want to exclude long due to its non-significant
anova(m_fin, m_comp) # result show: no!!?

summary(...)

library(car)
crPlots(m_fin)
residualPlots(m_fin)
# plot(...) will show more graphs

test = data.frame(age = c(20, 30), mrt = c(100, 110), lat = c(24.99102, 25.44122), long = c(121.4443, 121.5643))
testMean = data.frame(age = mean(realEstate$X2_age), 
                      mrt = mean(realEstate$X3_MRT_nearest),
                      lat = mean(realEstate$X5_lat),
                      long = mean(realEstate$X6_long))
testMax = data.frame(age = max(realEstate$X2_age), 
                      mrt = max(realEstate$X3_MRT_nearest),
                      lat = max(realEstate$X5_lat),
                      long = max(realEstate$X6_long))
predict(m_fin, test, interval = "confidence")
