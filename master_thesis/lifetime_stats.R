library(data.table)
library(stargazer)
library(lfe)

lifetime <- fread(
  "C:/Users/T-Reio/Master_thesis/master_thesis/input/f_lifetime_2000PA.csv", 
  header = T, sep = ",")

H <- lifetime$H
hist(H, breaks = seq(300,4300,25), main = "Base-hits lifetime")

HR <- lifetime$HR
hist(HR, breaks = seq(0,900,10))

lifetime_3000PA <- subset(lifetime, lifetime$PA >= 3000)

AVG <- lifetime_3000PA$AVG
hist(AVG, breaks = seq(0.210, 0.345, 0.001), main = "Carrer AVG", xlab = "Batting-Average")

BL <- sum(stats_57_200$AVG == 0.299)
AB <- sum(stats_57_200$AVG == 0.300)

prop.test(c(BL, AB), c(17788,17788), alternative = "less", conf.level = 0.95)

summary(AVG)
