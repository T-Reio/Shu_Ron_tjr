library(data.table)
library(lfe)
library(rdd)
library(rddensity)
stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, stats$Season >= 1995 & stats$Season <= 2003)

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
FA <- stats$FA

r2 <- RDestimate(Sal ~ AVG | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr,
                 cutpoint = 0.3)
summary(r2)

r2 <- RDestimate(Sal ~ AVG | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr,
                 cutpoint = 0.25)
summary(r2)

r2 <- RDestimate(Sal ~ HR | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 20)
summary(r2)

r2 <- RDestimate(Sal ~ OBP | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr,
                 cutpoint = 0.35)

summary(r2)


stats <- subset(stats, stats$FA == 1)
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
FA <- stats$FA

r2 <- RDestimate(Sal ~ AVG | FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr,
                 cutpoint = 0.3)
summary(r2)

r2 <- RDestimate(Sal ~ AVG | FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr,
                 cutpoint = 0.25)
summary(r2)

r2 <- RDestimate(Sal ~ HR | FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr,
                 cutpoint = 20)
summary(r1)
summary(r2)

r2 <- RDestimate(Sal ~ OBP | FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr,
                 cutpoint = 0.35)