library(data.table)
library(lfe)
library(rdd)
library(rddensity)
stats <- fread("C:/Users/T-Reio/Master_thesis/master_thesis/output/stats_sal_fa_since87.csv",
               header = T, sep = ",")

##Sample Rextriction
#200打席以上
stats <- subset(stats, stats$PA >= 200)
#90打席以上
stats <- subset(stats, stats$PA >= 90)
#FA有資格者のみ
stats <- subset(stats, stats$FA == 1)
#3割前後2分
stats <- subset(stats, AVG < 0.320 & AVG >= 0.280)
#同1分
stats <- subset(stats, AVG < 0.310 & AVG >= 0.290)
#前2分 後ろ2分に分割
stats <- subset(stats, AVG < 0.3 & AVG >= 0.280)
stats <- subset(stats, AVG < 0.320 & AVG >= 300)

Sal_real <- stats$`AVG ANNUAL`
Sal <- stats$`Log AVG ANNUAL`
Sal_dev <- stats$`Dev Log salary`
AVG <- stats$AVG
AGE <- stats$Age
AGE_sq <- AGE^2
fWAR <- stats$WAR
Ab_300 <- stats$AVG_above
G <- stats$G
WPA <- stats$WPA/G
nWPA <- -stats$`-WPA`/G
ID <- stats$playerid
BATTING <- stats$Bat
FIELDING <- stats$Fld
BaseRun <- stats$BsR
POS <- stats$POS
TEAM <- stats$TEAM_nextyr
rWARp <- stats$WAR3

RDD1 <- RDestimate(Sal ~ AVG, stats, cutpoint = 0.3)
RDD2 <- RDestimate(Sal_dev ~ AVG, stats, cutpoint = 0.3)
RDD3 <- RDestimate(Sal_dev ~ fWAR + Ab_300, stats, cutpoint = 0.3)
RDD4 <- RDestimate(Sal_dev ~ AVG | AGE + AGE_sq + FIELDING + BaseRun, cutpoint = 0.25)
summary(RDD1)
summary(RDD2)
summary(RDD3)
summary(RDD4)

#推定式
M1 <- lm(Sal_dev ~ BATTING * Ab_300 + FIELDING + BaseRun + AGE + AGE_sq + WPA + nWPA)
M2 <- lm(Sal_dev ~ fWAR * Ab_300 + FIELDING + BaseRun + AGE + AGE_sq + WPA + nWPA)

summary(M1)
summary(M2)


#FA専用
M3 <- lm(Sal_dev ~ rWARp * Ab_300 + FIELDING + BaseRun + AGE + AGE_sq + WPA + nWPA)
summary(M3)

#3割前後で分けた:交差項無し
M1 <- lm(Sal_dev ~ BATTING + FIELDING + BaseRun + AGE + AGE_sq + WPA + nWPA)
M2 <- lm(Sal_dev ~ fWAR + FIELDING + BaseRun + AGE + AGE_sq + WPA + nWPA)
M3 <- lm(Sal_dev ~ rWARp + FIELDING + BaseRun + AGE + AGE_sq + WPA + nWPA)
summary(M1)
summary(M2)
summary(M3)

cor(fWAR, AVG)
cor(BATTING, AVG)

cor.test(fWAR, AVG)

summary(Sal)

hist(Sal_real)
hist(Sal)

plot(AVG, Sal)
