library(data.table)
library(stargazer)
library(lfe)

stats_57_all <- fread("C:/Users/T-Reio/Master_thesis/master_thesis/output/battingstats_since57.csv", header = T, sep = ",")


H_57 <- stats_57_all$H
hist(H_57, breaks = seq(0,300,1), main = "Base-Hits for full sample")

PA <- stats_57_all$PA
hist(PA, breaks = seq(0, 780, 10), main = "Plate-Appearance")

PA.fac <- as.factor(PA)
summary(PA.fac)

#--打席以上

stats_57 <- subset(stats_57_all, stats_57_all$PA >= 90)

H_57 <- stats_57$H
hist(H_57, breaks = seq(0,300,1), main = "Base-Hits for at least 90PA"
     , xlab = "Base-Hit")

HR_57 <- stats_57$HR
summary(HR_57)
hist(HR_57, breaks = seq(0,75,1), main = "Homeruns for at least 90PA"
     , xlab = "HR")

stats_SB <- subset(stats_57_all, stats_57_all$SB >=5)
SB <- stats_SB$SB
summary(SB)
hist(SB, breaks = seq(3,133,1), main = "SB for at least 90PA"
     , xlab = "Stolen-Bases")


PA_90 <- stats_57$PA
summary(PA_90)
hist(PA_90, breaks = seq(0, 780, 10), main = "Plate-Appearance")

AB200 <- sum(stats_57$PA >= 200 & stats_57$PA < 210)
BL200 <- sum(stats_57$PA >= 190 & stats_57$PA < 200)

prop.test(c(BL200, AB200), c(25569, 25569), alternative = "less")

RBI_57 <- stats_57$RBI 

summary(RBI_57)

hist(RBI_57, breaks = seq(0, 165, 1), main = "RBI for at least 90PA", xlab = 
       "Runs-Batted-in")

BL100 <- sum(RBI_57 >= 96 & RBI_57 < 100)
AB100 <- sum(RBI_57 >= 100 & RBI_57 < 104)

prop.test(c(BL100, AB100), c(23762, 23762), alternative = "less")

#200打席以上

stats_57_200 <- subset(stats_57_all, stats_57_all$PA >= 200)

AVG_200 <- stats_57_200$AVG
hist(AVG_200, breaks = seq(0.120, 0.400, 0.001), main = "AVG for at least 200 PA")

prop.test(c(65,201), c(17788,17788))

summary(AVG_hist$xname)

AVG_200_f <- as.factor(AVG_200)
summary(AVG_200_f)

BL <- sum(stats_57_200$AVG == 0.299)
AB <- sum(stats_57_200$AVG == 0.300)

BL/17788
AB/17788

BL <- sum(stats_57_200$AVG >= 0.298 & stats_57_200$AVG < 0.300)
AB <- sum(stats_57_200$AVG >= 0.300 & stats_57_200$AVG < 0.302)

prop.test(c(BL, AB), c(17788,17788), alternative = "less", conf.level = 0.95)

154/17788

OBP <- stats_57_200$OBP
OPS <- stats_57_200$OPS

summary(OPS)
hist(OPS, breaks = seq(0.380, 1.425, 0.001), main = "OPS for at least 200PA")



#FA前

stats_bffa <- subset(stats_57_200, stats_57_200$Season <= 1975)
AVG_bffa <- stats_bffa$AVG
hist(AVG_bffa, breaks = seq(0.120, 0.400, 0.001), main = "AVG before FA",
     xlab = "Batting-Average")

BL_bffa <- sum(stats_bffa$AVG == 0.299)
AB_bffa <- sum(stats_bffa$AVG == 0.300)

prop.test(c(BL_bffa, AB_bffa), c(4292,4292), alternative = "less", conf.level = 0.95)

BL <- sum(stats_bffa$AVG >= 0.298 & stats_bffa$AVG < 0.300)
AB <- sum(stats_bffa$AVG >= 0.300 & stats_bffa$AVG < 0.302)

prop.test(c(BL, AB), c(4292,4292), alternative = "less", conf.level = 0.95)

B250 <- sum(stats_bffa$AVG == 0.249)
A250 <- sum(stats_bffa$AVG == 0.250)

prop.test(c(B250, A250), c(4292,4292), alternative = "less", conf.level = 0.95)

#FA～スト

stats_fast <- subset(stats_57_200, stats_57_200$Season >= 1977 & stats_57_200$Season <= 1994)
AVG_fast <- stats_fast$AVG
hist(AVG_fast, breaks = seq(0.120, 0.400, 0.001), main = "AVG FA to Strike",
     xlab = "Batting-Average")

BL_fast <- sum(stats_fast$AVG == 0.299)
AB_fast <- sum(stats_fast$AVG == 0.300)

prop.test(c(BL_fast, AB_fast), c(5331, 5331), alternative = "less", conf.level = 0.95)

BL <- sum(stats_fast$AVG >= 0.298 & stats_fast$AVG < 0.300)
AB <- sum(stats_fast$AVG >= 0.300 & stats_fast$AVG < 0.302)

prop.test(c(BL, AB), c(5331, 5331), alternative = "less", conf.level = 0.95)


#スト～Moneyball

stats_stmb <- subset(stats_57_200, stats_57_200$Season >= 1996 & stats_57_200$Season <= 2001)
AVG_stmb <- stats_stmb$AVG
hist(AVG_stmb, breaks = seq(0.120, 0.400, 0.001), main = "AVG Strike to MB",
     xlab = "Batting-average")

BL_stmb <- sum(stats_stmb$AVG == 0.299)
AB_stmb <- sum(stats_stmb$AVG == 0.300)

prop.test(c(BL_stmb, AB_stmb), c(2028,2028), alternative = "less", conf.level = 0.95)

BL <- sum(stats_stmb$AVG >= 0.298 & stats_stmb$AVG < 0.300)
AB <- sum(stats_stmb$AVG >= 0.300 & stats_stmb$AVG < 0.302)

prop.test(c(BL, AB), c(2028,2028), alternative = "less", conf.level = 0.95)



#Moneyball以降

stats_afmb <- subset(stats_57_200, stats_57_200$Season >= 2002)
AVG_afmb <- stats_afmb$AVG
hist(AVG_afmb, breaks = seq(0.120, 0.400, 0.001), main = "AVG after MB",
     xlab = "Batting-Average")

BL_afmb <- sum(stats_afmb$AVG == 0.299)
AB_afmb <- sum(stats_afmb$AVG == 0.300)

prop.test(c(BL_afmb, AB_afmb), c(5555,5555), alternative = "less", conf.level = 0.95)

BL <- sum(stats_afmb$AVG >= 0.298 & stats_afmb$AVG < 0.300)
AB <- sum(stats_afmb$AVG >= 0.300 & stats_afmb$AVG < 0.302)

prop.test(c(BL, AB), c(5555,5555), alternative = "less", conf.level = 0.95)

OBP_afmb <- stats_afmb$OBP
hist(OPS, breaks = seq(0., 1.425, 0.001), main = "OBP after Moneyball")

#z検定





