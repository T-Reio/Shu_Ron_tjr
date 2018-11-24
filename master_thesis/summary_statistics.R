library(stargazer)
library(data.table)
library(lfe)
library(rdd)
library(rddensity)
stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/input/fstats_5718.csv"
               , header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats_1 <- subset(stats, stats$Season <= 1975)
stats_2 <- subset(stats, stats$Season >= 1976 & stats$Season <= 1994)
stats_3 <- subset(stats, stats$Season >= 1995 & stats$Season <= 2003)
stats_4 <- subset(stats, stats$Season >= 2004)

AVG_1 <- stats_1$AVG
AVG_2 <- stats_2$AVG
AVG_3 <- stats_3$AVG
AVG_4 <- stats_4$AVG

AVG_mean <- list(c(mean(AVG_1), mean(AVG_2), mean(AVG_3), mean(AVG_4)))

summary(AVG_1)
summary(AVG_2)
summary(AVG_3)
summary(AVG_4)
