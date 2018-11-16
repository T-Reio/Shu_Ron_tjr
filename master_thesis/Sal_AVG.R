library(data.table)
library(lfe)
library(rdd)
library(rddensity)
library(stargazer)

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, AVG < 0.320 & AVG >= 0.280)
Sal_real <- stats$`AVG ANNUAL`
Sal <- stats$`Log AVG ANNUAL`
Sal_dev <- stats$`Dev Log salary`
AVG <- stats$AVG
AGE <- stats$Age
AGE_sq <- AGE^2
fWAR <- stats$WAR
G <- stats$G
PA <- stats$PA
WPA <- stats$`+WPA`/stats$PA
nWPA <- -stats$`-WPA`/stats$PA
Clutch <- stats$Clutch
ID <- stats$playerid
BAT <- stats$Bat
FLD <- stats$Fld
BsR <- stats$BsR
POS <- stats$POS
TEAM <- stats$TEAM_nextyr
ERA <- stats$Era
OBP <- stats$OBP
HR <- stats$HR
RBI <- stats$RBI
SB <- stats$SB
OPS <- stats$OPS
H <- stats$H
Yr <- as.factor(stats$Season)
AVG_300 <- stats$AVG_300
FA <- stats$FA

m1 <- lm(Sal ~ AVG + AVG_300)
m2 <- lm(Sal ~ AVG + AVG_300 + FLD + BsR)
m3 <- lm(Sal ~ AVG + AVG_300 + FLD + BsR + AGE + AGE_sq)
m4 <- lm(Sal ~ AVG + AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)
m5 <- lm(Sal ~ AVG + AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
m6 <- felm(Sal ~ AVG + AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
m7 <- felm(Sal ~ AVG + AVG_300 + FLD + BsR + WPA + nWPA + FA | ID)
m8 <- felm(Sal ~ AVG + AVG_300 + FLD + BsR + WPA + nWPA + FA + Yr | POS)

summary(m1)
summary(m8)

d1 <- lm(Sal ~ AVG * AVG_300)
d2 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR)
d3 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)
d5 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- felm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d7 <- felm(Sal ~ AVG * AVG_300 + FLD + BsR + WPA + nWPA + FA | ID)
d8 <- felm(Sal ~ AVG * AVG_300 + FLD + BsR + WPA + nWPA + FA + Yr | POS)

summary(d1)
summary(d7)

e1 <- lm(Sal ~ BAT * AVG_300)
e2 <- lm(Sal ~ BAT * AVG_300 + FLD + BsR)
e3 <- lm(Sal ~ BAT * AVG_300 + FLD + BsR + AGE + AGE_sq)
e4 <- lm(Sal ~ BAT * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)
e5 <- lm(Sal ~ BAT * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- felm(Sal ~ BAT * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
e7 <- felm(Sal ~ BAT * AVG_300 + FLD + BsR + WPA + nWPA + FA | ID)
e8 <- felm(Sal ~ BAT * AVG_300 + FLD + BsR + WPA + nWPA + FA + Yr| POS )

summary(e1)
summary(e8)