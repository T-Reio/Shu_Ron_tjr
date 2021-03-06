library(data.table)
library(lfe)
library(rdd)
library(rddensity)
stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
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

r1 <- RDestimate(Sal ~ AVG, cutpoint = 0.3, se.type = "HC2")
r2 <- RDestimate(Sal ~ AVG | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr,
                 cutpoint = 0.3, se.type = "HC2")
summary(r1)
summary(r2)

r1 <- RDestimate(Sal ~ HR, cutpoint = 20, se.type = "HC2")
r2 <- RDestimate(Sal ~ HR | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 20, se.type = "HC2")
summary(r1)
summary(r2)

r1 <- RDestimate(Sal ~ OBP, cutpoint = 0.35, se.type = "HC2")
r2 <- RDestimate(Sal ~ OBP | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr,
                 cutpoint = 0.35, se.type = "HC2")
summary(r1)
summary(r2)

r1 <- RDestimate(Sal ~ RBI, cutpoint = 100, se.type = "HC2")
r2 <- RDestimate(Sal ~ RBI | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr,
                 cutpoint = 100)
summary(r1)
summary(r2)
r2$bw
r2$obs
r2$est
r2$se
r2$z

r1 <- RDestimate(Sal ~ H, cutpoint = 200)
r2 <- RDestimate(Sal ~ H | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr,
                 cutpoint = 200)
summary(r1)
summary(r2)


r1 <- RDestimate(Sal ~ SB, cutpoint = 30, se.type = "HC2")
r2 <- RDestimate(Sal ~ SB | FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA + Yr,
                 cutpoint = 30)

summary(r1)
summary(r2)

r1 <- RDestimate(Sal ~ SB, cutpoint = 40, se.type = "HC2")
r2 <- RDestimate(Sal ~ SB | FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA + Yr,
                 cutpoint = 40)
summary(r1)
summary(r2)

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
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

r1 <- RDestimate(Sal ~ AVG, cutpoint = 0.3)
r2 <- RDestimate(Sal ~ AVG | FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr,
                 cutpoint = 0.3)
summary(r1)
summary(r2)

r1 <- RDestimate(Sal ~ AVG, cutpoint = 0.25)
r2 <- RDestimate(Sal ~ AVG | FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr,
                 cutpoint = 0.25)
summary(r1)
summary(r2)

r1 <- RDestimate(Sal ~ HR, cutpoint = 20)
r2 <- RDestimate(Sal ~ HR | FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr,
                 cutpoint = 20)
summary(r1)
summary(r2)

r1 <- RDestimate(Sal ~ OBP, cutpoint = 0.35)
r2 <- RDestimate(Sal ~ OBP | FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr,
                 cutpoint = 0.35)
summary(r1)
summary(r2)

r1 <- RDestimate(Sal ~ RBI, cutpoint = 100)
r2 <- RDestimate(Sal ~ RBI | FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr,
                 cutpoint = 100)
summary(r1)
summary(r2)

r1 <- RDestimate(Sal ~ SB, cutpoint = 30)
r2 <- RDestimate(Sal ~ SB | FLD + BAT + AGE + AGE_sq + WPA + nWPA + Yr,
                 cutpoint = 30)

summary(r1)
summary(r2)

r1 <- RDestimate(Sal ~ SB, cutpoint = 40 )
r2 <- RDestimate(Sal ~ SB | FLD + BAT + AGE + AGE_sq + WPA + nWPA + Yr,
                 cutpoint = 40)
summary(r1)
summary(r2)

r1 <- RDestimate(Sal ~ H, cutpoint = 100)
r2 <- RDestimate(Sal ~ H | FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr,
                 cutpoint = 100)
summary(r1)
summary(r2)
