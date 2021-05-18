newFlights <- select(flights, "carrier", "origin", "dep_time", "arr_time", "dep_delay", "arr_delay")

colSums(is.na(df))

order(newFlights$carrier)


describeBy(newFlights, group = TRUE, mat = TRUE)
groups <- split(newFlights, (seq(nrow(newFlights))-1) %/% 10000)

tapply(clean_newFlights$dep_delay, clean_newFlights$carrier, summary)
tapply(clean_newFlights$dep_delay, clean_newFlights$carrier, sd)

boxplot(clean_newFlights$dep_delay~clean_newFlights$carrier)
boxplot(clean_newFlights$dep_delay~clean_newFlights$carrier, outline = FALSE, main = "Boxplot of Delay for each carrier", xlab = "Carrier", ylab = "Delay")

library("dplyr")
library("ggpubr")
carrierPDX <- select(clean_newFlights, "carrier", "origin", "dep_delay")
carrierPDX <- subset(carrierPDX, origin == "PDX")
carrierPDX <- select(carrierPDX, "carrier", "dep_delay")
# carrierPDX$dep_delay <- abs(carrierPDX$dep_delay)
ggdensity(carrierPDX$dep_delay, main = "Normality test using Density", xlab = "Delay")
ggqqplot(carrierPDX$dep_delay, main = "Normality test using Q-Q plot", xlab = "Delay")
shapiro.test(carrierPDX$dep_delay) # error bcs > 5000 values
library(nortest)
ad.test(carrierPDX$dep_delay)$p.value 
# ks.test(carrierPDX$dep_delay, y = "pnorm", alternative = "two.sided") # for continuous dis.
# all test above equivalent to shapiro
# p > 0.1 => accept H0 (H0: distribution is normal)
# 0.05 < p < 0.1 => can be assumed

library(car)
leveneTest(carrierPDX$dep_delay, as.factor(carrierPDX$carrier), center = mean)

library("ggpubr")
anovaRes <- aov(carrierPDX$dep_delay ~ as.factor(carrierPDX$carrier))
summary(anovaRes)
