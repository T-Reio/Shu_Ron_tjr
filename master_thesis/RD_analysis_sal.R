library(data.table)
library(lfe)
library(rdd)
library(rddensity)
stats <- fread("C:/Users/T-Reio/Master_thesis/master_thesis/output/stats_salary_87to17.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
Sal <- stats$`Log AVG ANNUAL`
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

hist(Sal)

RDD1 <- RDestimate(Sal ~ AVG, stats, cutpoint = 0.3)
summary(RDD1)

RDD2 <- RDestimate(Sal ~ AVG | AGE + AGE_sq, cutpoint = 0.300)
summary(RDD2)

hist(AVG, breaks = seq(0.130, 0.400, 0.001))

RDD3 <- RDestimate(Sal ~ AVG | AGE + AGE_sq, cutpoint = 0.250)
summary(RDD3)

RDD4 <- RDestimate(Sal ~ AVG | AGE + AGE_sq, cutpoint = 0.280)
summary(RDD3)

RDD5 <- RDestimate(Sal ~ AVG | AGE + AGE_sq, cutpoint = 0.290)
summary(RDD5)

#Full Sample

M1 <- lm(Sal ~ fWAR * Ab_300 + AGE + AGE_sq + WPA + nWPA)
M2 <- felm(Sal ~ BATTING * Ab_300 + FIELDING + BaseRun + AGE + AGE_sq + WPA + nWPA | POS + TEAM)


summary(M1)
summary(M2)


#サンプルをSwitching Point前後に限定

hist(Sal)

plot(AVG, Sal, xlim=c(0.10, 0.40))

stats_rng.020 <- subset(stats, AVG < 0.320 & AVG >= 0.280)
Sal <- stats_rng.020$`Log AVG ANNUAL`
AVG <- stats_rng.020$AVG
AGE <- stats_rng.020$Age
AGE_sq <- AGE^2
fWAR <- stats_rng.020$WAR
Ab_300 <- stats_rng.020$AVG_above
G <- stats_rng.020$G
WPA <- stats_rng.020$WPA/G
nWPA <- stats_rng.020$`-WPA`/G
ID <- stats_rng.020$playerid
BATTING <- stats_rng.020$Bat
FIELDING <- stats_rng.020$Fld
BaseRun <- stats_rng.020$BsR
POS <- stats_rng.020$POS

M3 <- lm(Sal ~ fWAR * Ab_300 + AGE + AGE_sq + WPA + nWPA)
M4 <- felm(Sal ~ BATTING * Ab_300 + FIELDING + BaseRun + AGE + AGE_sq + WPA + nWPA | POS)

summary(M3)
summary(M4)

stats_rng.010 <- subset(stats, AVG < 0.310 & AVG >= 0.290)
Sal <- stats_rng.010$`Log AVG ANNUAL`
AVG <- stats_rng.010$AVG
AGE <- stats_rng.010$Age
AGE_sq <- AGE^2
fWAR <- stats_rng.010$WAR
Ab_300 <- stats_rng.010$AVG_above
G <- stats_rng.010$G
WPA <- stats_rng.010$WPA/G
nWPA <- stats_rng.010$`-WPA`/G
ID <- stats_rng.010$playerid
BATTING <- stats_rng.010$Bat
FIELDING <- stats_rng.010$Fld
BaseRun <- stats_rng.010$BsR
POS <- stats_rng.010$POS

M5 <- lm(Sal ~ fWAR * Ab_300 + AGE + AGE_sq + WPA + nWPA)
M6 <- felm(Sal ~ BATTING * Ab_300 + FIELDING + BaseRun + AGE + AGE_sq + WPA + nWPA | POS)

summary(M5)
summary(M6)
