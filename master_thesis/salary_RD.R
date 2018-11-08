library(data.table)
library(lfe)
library(rdd)
library(rddensity)

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised.csv",
               header = T, sep = ",")

#Common Sort
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, stats$PA >= 90)
stats <- subset(stats, stats$SB >= 5)

#Sort with Eras
stats <- subset(stats, stats$Season >= 1987 & stats$Season <= 1994)
stats <- subset(stats, stats$Season >= 1995 & stats$Season <= 2003)
stats <- subset(stats, stats$Season >= 2004)

#Sort with charactaristics
stats <- subset(stats, stats$FA == 1)
stats <- subset(stats, stats$FA == 0)

#For local linear analysis
stats <- subset(stats, AVG < 0.320 & AVG >= 0.280)
stats <- subset(stats, AVG < 0.310 & AVG >= 0.290)

stats <- subset(stats, AVG < 0.320 & AVG >= 0.280)
stats <- subset(stats, AVG < 0.310 & AVG >= 0.290)

stats <- subset(stats, OBP < 0.370 & OBP >= 0.330)
stats <- subset(stats, OBP < 0.360 & OBP >= 0.340)

stats <- subset(stats, AVG < 0.320 & AVG >= 0.280)
stats <- subset(stats, AVG < 0.310 & AVG >= 0.290)

stats <- subset(stats, AVG < 0.320 & AVG >= 0.280)
stats <- subset(stats, AVG < 0.310 & AVG >= 0.290)

stats <- subset(stats, AVG < 0.320 & AVG >= 0.280)
stats <- subset(stats, AVG < 0.310 & AVG >= 0.290)

#Variables
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
Yr <- stats$Season

AVG_300 <- stats$AVG_300
OBP_350 <- stats$OBP_350
HR_20 <- stats$HR_20
RBI_100 <- stats$RBI_100
SB_30 <- stats$SB_30
SB_40 <- stats$SB_40
OPS_1000 <- stats$OPS_1000
PA_500 <- stats$PA_500
H_200 <- stats$H_200

FA <- stats$FA

#Valid only FA players
MLS <- stats$Yrs
rWARp <- stats$WAR3

#AVG
#linear regression
m1 <- lm(Sal ~ AVG + AVG_300)
m2 <- lm(Sal ~ AVG + AVG_300 + FLD + BsR)
m3 <- lm(Sal ~ AVG + AVG_300 + FLD + BsR + AGE + AGE_sq)
m4 <- lm(Sal ~ AVG + AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

m5 <- lm(Sal ~ AVG + AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
m6 <- lm(Sal ~ AVG + AVG_300 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
m7 <- felm(Sal ~ AVG + AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
m8 <- felm(Sal ~ AVG + AVG_300 + FLD + BsR + WPA + nWPA + FA | ID)
m9 <- felm(Sal ~ AVG + AVG_300 + FLD + BsR + WPA + nWPA + FA | Yr)

m5 <- lm(Sal ~ AVG + AVG_300 + FLD + BsR + AGE + AGE_sq + Clutch)
m6 <- felm(Sal ~ AVG + AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
m7 <- felm(Sal ~ AVG + AVG_300 + FLD + BsR + WPA + nWPA | ID)
m9 <- felm(Sal ~ AVG + AVG_300 + FLD + BsR + WPA + nWPA | Yr)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)
summary(m9)

m1 <- lm(Sal_dev ~ AVG + AVG_300)
m2 <- lm(Sal_dev ~ AVG + AVG_300 + FLD + BsR)
m3 <- lm(Sal_dev ~ AVG + AVG_300 + FLD + BsR + AGE + AGE_sq)
m4 <- lm(Sal_dev ~ AVG + AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

m5 <- lm(Sal_dev ~ AVG + AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
m6 <- lm(Sal_dev ~ AVG + AVG_300 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
m7 <- felm(Sal_dev ~ AVG + AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
m8 <- felm(Sal_dev ~ AVG + AVG_300 + FLD + BsR + WPA + nWPA + FA | ID)

m5 <- lm(Sal_dev ~ AVG + AVG_300 + FLD + BsR + AGE + AGE_sq + Clutch)
m6 <- felm(Sal_dev ~ AVG + AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
m7 <- felm(Sal_dev ~ AVG + AVG_300 + FLD + BsR + WPA + nWPA | ID)


summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)



#Regression discontinuity
r1 <- RDestimate(Sal ~ AVG, cutpoint = 0.3, se.type = "HC2")
r2 <- RDestimate(Sal ~ AVG | FLD + BsR, cutpoint = 0.3, se.type = "HC2")
r3 <- RDestimate(Sal ~ AVG | FLD + BsR + AGE + AGE_sq , cutpoint = 0.3, se.type = "HC2")
r4 <- RDestimate(Sal ~ AVG | FLD + BsR + AGE + AGE_sq + WPA + nWPA ,
                 cutpoint = 0.3, se.type = "HC2")

r5 <- RDestimate(Sal ~ AVG | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 0.3, se.type = "HC2")
r6 <- RDestimate(Sal ~ AVG | FLD + BsR + AGE + AGE_sq + Clutch + FA,
                 cutpoint = 0.3, se.type = "HC2")

r6 <- RDestimate(Sal ~ AVG | FLD + BsR + AGE + AGE_sq + Clutch,
                 cutpoint = 0.3, se.type = "HC2")
summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)

r1 <- RDestimate(Sal_dev ~ AVG, cutpoint = 0.3, se.type = "HC2")
r2 <- RDestimate(Sal_dev ~ AVG | FLD + BsR, cutpoint = 0.3, se.type = "HC2")
r3 <- RDestimate(Sal_dev ~ AVG | FLD + BsR + AGE + AGE_sq , cutpoint = 0.3, se.type = "HC2")
r4 <- RDestimate(Sal_dev ~ AVG | FLD + BsR + AGE + AGE_sq + WPA + nWPA ,
                 cutpoint = 0.3, se.type = "HC2")

r5 <- RDestimate(Sal_dev  ~ AVG | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 0.3, se.type = "HC2")
r6 <- RDestimate(Sal_dev ~ AVG | FLD + BsR + AGE + AGE_sq + Clutch + FA,
                 cutpoint = 0.3, se.type = "HC2")

r6 <- RDestimate(Sal_dev ~ AVG | FLD + BsR + AGE + AGE_sq + Clutch,
                 cutpoint = 0.3, se.type = "HC2")

summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)

#DID
d1 <- lm(Sal ~ AVG * AVG_300)
d2 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR)
d3 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal ~ AVG * AVG_300 + FLD + BsR + WPA + nWPA + FA | ID)
d9 <- felm(Sal ~ AVG * AVG_300 + FLD + BsR + WPA + nWPA + FA | Yr)


d5 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal ~ AVG * AVG_300 + FLD + BsR + WPA + nWPA | ID)
d8 <- felm(Sal ~ AVG * AVG_300 + FLD + BsR + WPA + nWPA | Yr)

summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)
summary(d9)

d1 <- lm(Sal ~ BAT * AVG_300)
d2 <- lm(Sal ~ BAT * AVG_300 + FLD + BsR)
d3 <- lm(Sal ~ BAT * AVG_300 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal ~ BAT * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal ~ BAT * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal ~ BAT * AVG_300 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal ~ BAT * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal ~ BAT * AVG_300 + FLD + BsR + WPA + nWPA + FA | ID)
d9 <- felm(Sal ~ BAT * AVG_300 + FLD + BsR + WPA + nWPA + FA | Yr)

d5 <- lm(Sal ~ BAT * AVG_300 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal ~ BAT * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal ~ BAT * AVG_300 + FLD + BsR + WPA + nWPA | ID)
d8 <- felm(Sal ~ BAT * AVG_300 + FLD + BsR + WPA + nWPA | Yr)

summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)
summary(d9)

d1 <- lm(Sal_dev ~ BAT * AVG_300)
d2 <- lm(Sal_dev ~ BAT * AVG_300 + FLD + BsR)
d3 <- lm(Sal_dev ~ BAT * AVG_300 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal_dev ~ BAT * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal_dev ~ BAT * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal_dev ~ BAT * AVG_300 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal_dev ~ BAT * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal_dev ~ BAT * AVG_300 + FLD + BsR + WPA + nWPA + FA | ID)

d5 <- lm(Sal_dev ~ BAT * AVG_300 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal_dev ~ BAT * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal_dev ~ BAT * AVG_300 + FLD + BsR + WPA + nWPA | ID)


summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)

#DIDID
e1 <- lm(Sal ~ AVG * AVG_300 * ERA)
e2 <- lm(Sal ~ AVG * AVG_300 * ERA + FLD + BsR)
e3 <- lm(Sal ~ AVG * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq)
e4 <- lm(Sal ~ AVG * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

e5 <- lm(Sal ~ AVG * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- lm(Sal ~ AVG * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq + Clutch + FA)
e7 <- felm(Sal ~ AVG * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
e8 <- felm(Sal ~ AVG * AVG_300 * ERA + FLD + BsR + WPA + nWPA + FA | ID)

e5 <- lm(Sal ~ AVG * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq + Clutch)
e6 <- felm(Sal ~ AVG * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA| POS)
e7 <- felm(Sal ~ AVG * AVG_300 * ERA + FLD + BsR + WPA + nWPA | ID)

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)

e1 <- lm(Sal_dev ~ AVG * AVG_300 * ERA)
e2 <- lm(Sal_dev ~ AVG * AVG_300 * ERA + FLD + BsR)
e3 <- lm(Sal_dev ~ AVG * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq)
e4 <- lm(Sal_dev ~ AVG * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

e5 <- lm(Sal_dev ~ AVG * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- lm(Sal_dev ~ AVG * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq + Clutch + FA)
e7 <- felm(Sal_dev ~ AVG * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
e8 <- felm(Sal_dev ~ AVG * AVG_300 * ERA + FLD + BsR + WPA + nWPA + FA | ID)

e5 <- lm(Sal_dev ~ AVG * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq + Clutch)
e6 <- felm(Sal_dev ~ AVG * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA| POS)
e7 <- felm(Sal_dev ~ AVG * AVG_300 * ERA + FLD + BsR + WPA + nWPA | ID)

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)

e1 <- lm(Sal_dev ~ BAT * AVG_300 * ERA)
e2 <- lm(Sal_dev ~ BAT * AVG_300 * ERA + FLD + BsR)
e3 <- lm(Sal_dev ~ BAT * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq)
e4 <- lm(Sal_dev ~ BAT * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

e5 <- lm(Sal_dev ~ BAT * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- lm(Sal_dev ~ BAT * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq + Clutch + FA)
e7 <- felm(Sal_dev ~ BAT * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
e8 <- felm(Sal_dev ~ BAT * AVG_300 * ERA + FLD + BsR + WPA + nWPA + FA | ID)

e5 <- lm(Sal_dev ~ BAT * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq + Clutch)
e6 <- felm(Sal_dev ~ BAT * AVG_300 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA| POS)
e7 <- felm(Sal_dev ~ BAT * AVG_300 * ERA + FLD + BsR + WPA + nWPA | ID)

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)

#HR
#linear regression
m1 <- lm(Sal ~ HR + HR_20)
m2 <- lm(Sal ~ HR + HR_20 + FLD + BsR)
m3 <- lm(Sal ~ HR + HR_20 + FLD + BsR + AGE + AGE_sq)
m4 <- lm(Sal ~ HR + HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

m5 <- lm(Sal ~ HR + HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
m6 <- lm(Sal ~ HR + HR_20 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
m7 <- felm(Sal ~ HR + HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
m8 <- felm(Sal ~ HR + HR_20 + FLD + BsR + WPA + nWPA + FA | ID)
m9 <- felm(Sal ~ HR + HR_20 + FLD + BsR + WPA + nWPA + FA | Yr)

m5 <- lm(Sal ~ HR + HR_20 + FLD + BsR + AGE + AGE_sq + Clutch)
m6 <- felm(Sal ~ HR + HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
m7 <- felm(Sal ~ HR + HR_20 + FLD + BsR + WPA + nWPA | ID)
m9 <- felm(Sal ~ HR + HR_20 + FLD + BsR + WPA + nWPA | Yr)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)
summary(m9)

m1 <- lm(Sal_dev ~ HR + HR_20)
m2 <- lm(Sal_dev ~ HR + HR_20 + FLD + BsR)
m3 <- lm(Sal_dev ~ HR + HR_20 + FLD + BsR + AGE + AGE_sq)
m4 <- lm(Sal_dev ~ HR + HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

m5 <- lm(Sal_dev ~ HR + HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
m6 <- lm(Sal_dev ~ HR + HR_20 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
m7 <- felm(Sal_dev ~ HR + HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
m8 <- felm(Sal_dev ~ HR + HR_20 + FLD + BsR + WPA + nWPA + FA | ID)

m5 <- lm(Sal_dev ~ HR + HR_20 + FLD + BsR + AGE + AGE_sq + Clutch)
m6 <- felm(Sal_dev ~ HR + HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
m7 <- felm(Sal_dev ~ HR + HR_20 + FLD + BsR + WPA + nWPA | ID)


summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)



#Regression discontinuity
r1 <- RDestimate(Sal ~ HR, cutpoint = 20, se.type = "HC2")
r2 <- RDestimate(Sal ~ HR | FLD + BsR, cutpoint = 20, se.type = "HC2")
r3 <- RDestimate(Sal ~ HR | FLD + BsR + AGE + AGE_sq , cutpoint = 20, se.type = "HC2")
r4 <- RDestimate(Sal ~ HR | FLD + BsR + AGE + AGE_sq + WPA + nWPA ,
                 cutpoint = 20, se.type = "HC2")

r5 <- RDestimate(Sal ~ HR | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 20, se.type = "HC2")
r6 <- RDestimate(Sal ~ HR | FLD + BsR + AGE + AGE_sq + Clutch + FA,
                 cutpoint = 20, se.type = "HC2")

r6 <- RDestimate(Sal ~ HR | FLD + BsR + AGE + AGE_sq + Clutch,
                 cutpoint = 20, se.type = "HC2")
summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)

r1 <- RDestimate(Sal_dev ~ HR, cutpoint = 20, se.type = "HC2")
r2 <- RDestimate(Sal_dev ~ HR | FLD + BsR, cutpoint = 20, se.type = "HC2")
r3 <- RDestimate(Sal_dev ~ HR | FLD + BsR + AGE + AGE_sq , cutpoint = 20, se.type = "HC2")
r4 <- RDestimate(Sal_dev ~ HR | FLD + BsR + AGE + AGE_sq + WPA + nWPA ,
                 cutpoint = 20, se.type = "HC2")

r5 <- RDestimate(Sal_dev  ~ HR | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 20, se.type = "HC2")
r6 <- RDestimate(Sal_dev ~ HR | FLD + BsR + AGE + AGE_sq + Clutch + FA,
                 cutpoint = 20, se.type = "HC2")

r6 <- RDestimate(Sal_dev ~ HR | FLD + BsR + AGE + AGE_sq + Clutch,
                 cutpoint = 20, se.type = "HC2")

summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)

#DID
d1 <- lm(Sal ~ HR * HR_20)
d2 <- lm(Sal ~ HR * HR_20 + FLD + BsR)
d3 <- lm(Sal ~ HR * HR_20 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal ~ HR * HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal ~ HR * HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal ~ HR * HR_20 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal ~ HR * HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal ~ HR * HR_20 + FLD + BsR + WPA + nWPA + FA | ID)
d9 <- felm(Sal ~ HR * HR_20 + FLD + BsR + WPA + nWPA + FA | Yr)


d5 <- lm(Sal ~ HR * HR_20 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal ~ HR * HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal ~ HR * HR_20 + FLD + BsR + WPA + nWPA | ID)
d8 <- felm(Sal ~ HR * HR_20 + FLD + BsR + WPA + nWPA | Yr)

summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)
summary(d9)

d1 <- lm(Sal ~ BAT * HR_20)
d2 <- lm(Sal ~ BAT * HR_20 + FLD + BsR)
d3 <- lm(Sal ~ BAT * HR_20 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal ~ BAT * HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal ~ BAT * HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal ~ BAT * HR_20 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal ~ BAT * HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal ~ BAT * HR_20 + FLD + BsR + WPA + nWPA + FA | ID)
d9 <- felm(Sal ~ BAT * HR_20 + FLD + BsR + WPA + nWPA + FA | Yr)

d5 <- lm(Sal ~ BAT * HR_20 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal ~ BAT * HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal ~ BAT * HR_20 + FLD + BsR + WPA + nWPA | ID)
d8 <- felm(Sal ~ BAT * HR_20 + FLD + BsR + WPA + nWPA | Yr)

summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)
summary(d9)

d1 <- lm(Sal_dev ~ BAT * HR_20)
d2 <- lm(Sal_dev ~ BAT * HR_20 + FLD + BsR)
d3 <- lm(Sal_dev ~ BAT * HR_20 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal_dev ~ BAT * HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal_dev ~ BAT * HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal_dev ~ BAT * HR_20 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal_dev ~ BAT * HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal_dev ~ BAT * HR_20 + FLD + BsR + WPA + nWPA + FA | ID)

d5 <- lm(Sal_dev ~ BAT * HR_20 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal_dev ~ BAT * HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal_dev ~ BAT * HR_20 + FLD + BsR + WPA + nWPA | ID)


summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)

#DIDID
e1 <- lm(Sal ~ HR * HR_20 * ERA)
e2 <- lm(Sal ~ HR * HR_20 * ERA + FLD + BsR)
e3 <- lm(Sal ~ HR * HR_20 * ERA + FLD + BsR + AGE + AGE_sq)
e4 <- lm(Sal ~ HR * HR_20 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

e5 <- lm(Sal ~ HR * HR_20 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- lm(Sal ~ HR * HR_20 * ERA + FLD + BsR + AGE + AGE_sq + Clutch + FA)
e7 <- felm(Sal ~ HR * HR_20 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
e8 <- felm(Sal ~ HR * HR_20 * ERA + FLD + BsR + WPA + nWPA + FA | ID)

e5 <- lm(Sal ~ HR * HR_20 * ERA + FLD + BsR + AGE + AGE_sq + Clutch)
e6 <- felm(Sal ~ HR * HR_20 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA| POS)
e7 <- felm(Sal ~ HR * HR_20 * ERA + FLD + BsR + WPA + nWPA | ID)

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)

e1 <- lm(Sal_dev ~ HR * HR_20 * ERA)
e2 <- lm(Sal_dev ~ HR * HR_20 * ERA + FLD + BsR)
e3 <- lm(Sal_dev ~ HR * HR_20 * ERA + FLD + BsR + AGE + AGE_sq)
e4 <- lm(Sal_dev ~ HR * HR_20 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

e5 <- lm(Sal_dev ~ HR * HR_20 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- lm(Sal_dev ~ HR * HR_20 * ERA + FLD + BsR + AGE + AGE_sq + Clutch + FA)
e7 <- felm(Sal_dev ~ HR * HR_20 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
e8 <- felm(Sal_dev ~ HR * HR_20 * ERA + FLD + BsR + WPA + nWPA + FA | ID)

e5 <- lm(Sal_dev ~ HR * HR_20 * ERA + FLD + BsR + AGE + AGE_sq + Clutch)
e6 <- felm(Sal_dev ~ HR * HR_20 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA| POS)
e7 <- felm(Sal_dev ~ HR * HR_20 * ERA + FLD + BsR + WPA + nWPA | ID)

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)


#OBP
#linear regression
m1 <- lm(Sal ~ OBP + OBP_350)
m2 <- lm(Sal ~ OBP + OBP_350 + FLD + BsR)
m3 <- lm(Sal ~ OBP + OBP_350 + FLD + BsR + AGE + AGE_sq)
m4 <- lm(Sal ~ OBP + OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

m5 <- lm(Sal ~ OBP + OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
m6 <- lm(Sal ~ OBP + OBP_350 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
m7 <- felm(Sal ~ OBP + OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
m8 <- felm(Sal ~ OBP + OBP_350 + FLD + BsR + WPA + nWPA + FA | ID)
m9 <- felm(Sal ~ OBP + OBP_350 + FLD + BsR + WPA + nWPA + FA | Yr)

m5 <- lm(Sal ~ OBP + OBP_350 + FLD + BsR + AGE + AGE_sq + Clutch)
m6 <- felm(Sal ~ OBP + OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
m7 <- felm(Sal ~ OBP + OBP_350 + FLD + BsR + WPA + nWPA | ID)
m9 <- felm(Sal ~ OBP + OBP_350 + FLD + BsR + WPA + nWPA | Yr)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)
summary(m9)

m1 <- lm(Sal_dev ~ OBP + OBP_350)
m2 <- lm(Sal_dev ~ OBP + OBP_350 + FLD + BsR)
m3 <- lm(Sal_dev ~ OBP + OBP_350 + FLD + BsR + AGE + AGE_sq)
m4 <- lm(Sal_dev ~ OBP + OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

m5 <- lm(Sal_dev ~ OBP + OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
m6 <- lm(Sal_dev ~ OBP + OBP_350 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
m7 <- felm(Sal_dev ~ OBP + OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
m8 <- felm(Sal_dev ~ OBP + OBP_350 + FLD + BsR + WPA + nWPA + FA | ID)

m5 <- lm(Sal_dev ~ OBP + OBP_350 + FLD + BsR + AGE + AGE_sq + Clutch)
m6 <- felm(Sal_dev ~ OBP + OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
m7 <- felm(Sal_dev ~ OBP + OBP_350 + FLD + BsR + WPA + nWPA | ID)


summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)



#Regression discontinuity
r1 <- RDestimate(Sal ~ OBP, cutpoint = 0.35, se.type = "HC2")
r2 <- RDestimate(Sal ~ OBP | FLD + BsR, cutpoint = 0.35, se.type = "HC2")
r3 <- RDestimate(Sal ~ OBP | FLD + BsR + AGE + AGE_sq , cutpoint = 0.35, se.type = "HC2")
r4 <- RDestimate(Sal ~ OBP | FLD + BsR + AGE + AGE_sq + WPA + nWPA ,
                 cutpoint = 0.35, se.type = "HC2")

r5 <- RDestimate(Sal ~ OBP | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 0.35, se.type = "HC2")
r6 <- RDestimate(Sal ~ OBP | FLD + BsR + AGE + AGE_sq + Clutch + FA,
                 cutpoint = 0.35, se.type = "HC2")

r6 <- RDestimate(Sal ~ OBP | FLD + BsR + AGE + AGE_sq + Clutch,
                 cutpoint = 0.35, se.type = "HC2")
summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)

r1 <- RDestimate(Sal_dev ~ OBP, cutpoint = 0.35, se.type = "HC2")
r2 <- RDestimate(Sal_dev ~ OBP | FLD + BsR, cutpoint = 0.35, se.type = "HC2")
r3 <- RDestimate(Sal_dev ~ OBP | FLD + BsR + AGE + AGE_sq , cutpoint = 0.35, se.type = "HC2")
r4 <- RDestimate(Sal_dev ~ OBP | FLD + BsR + AGE + AGE_sq + WPA + nWPA ,
                 cutpoint = 0.35, se.type = "HC2")

r5 <- RDestimate(Sal_dev  ~ OBP | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 0.35, se.type = "HC2")
r6 <- RDestimate(Sal_dev ~ OBP | FLD + BsR + AGE + AGE_sq + Clutch + FA,
                 cutpoint = 0.35, se.type = "HC2")

r6 <- RDestimate(Sal_dev ~ OBP | FLD + BsR + AGE + AGE_sq + Clutch,
                 cutpoint = 0.35, se.type = "HC2")

summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)

#DID
d1 <- lm(Sal ~ OBP * OBP_350)
d2 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR)
d3 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + WPA + nWPA + FA | ID)
d9 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + WPA + nWPA + FA | Yr)


d5 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + WPA + nWPA | ID)
d8 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + WPA + nWPA | Yr)

summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)
summary(d9)

d1 <- lm(Sal ~ BAT * OBP_350)
d2 <- lm(Sal ~ BAT * OBP_350 + FLD + BsR)
d3 <- lm(Sal ~ BAT * OBP_350 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal ~ BAT * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal ~ BAT * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal ~ BAT * OBP_350 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal ~ BAT * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal ~ BAT * OBP_350 + FLD + BsR + WPA + nWPA + FA | ID)
d9 <- felm(Sal ~ BAT * OBP_350 + FLD + BsR + WPA + nWPA + FA | Yr)

d5 <- lm(Sal ~ BAT * OBP_350 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal ~ BAT * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal ~ BAT * OBP_350 + FLD + BsR + WPA + nWPA | ID)
d8 <- felm(Sal ~ BAT * OBP_350 + FLD + BsR + WPA + nWPA | Yr)

summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)
summary(d9)

d1 <- lm(Sal_dev ~ BAT * OBP_350)
d2 <- lm(Sal_dev ~ BAT * OBP_350 + FLD + BsR)
d3 <- lm(Sal_dev ~ BAT * OBP_350 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal_dev ~ BAT * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal_dev ~ BAT * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal_dev ~ BAT * OBP_350 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal_dev ~ BAT * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal_dev ~ BAT * OBP_350 + FLD + BsR + WPA + nWPA + FA | ID)

d5 <- lm(Sal_dev ~ BAT * OBP_350 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal_dev ~ BAT * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal_dev ~ BAT * OBP_350 + FLD + BsR + WPA + nWPA | ID)


summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)

#DIDID
e1 <- lm(Sal ~ OBP * OBP_350 * ERA)
e2 <- lm(Sal ~ OBP * OBP_350 * ERA + FLD + BsR)
e3 <- lm(Sal ~ OBP * OBP_350 * ERA + FLD + BsR + AGE + AGE_sq)
e4 <- lm(Sal ~ OBP * OBP_350 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

e5 <- lm(Sal ~ OBP * OBP_350 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- lm(Sal ~ OBP * OBP_350 * ERA + FLD + BsR + AGE + AGE_sq + Clutch + FA)
e7 <- felm(Sal ~ OBP * OBP_350 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
e8 <- felm(Sal ~ OBP * OBP_350 * ERA + FLD + BsR + WPA + nWPA + FA | ID)

e5 <- lm(Sal ~ OBP * OBP_350 * ERA + FLD + BsR + AGE + AGE_sq + Clutch)
e6 <- felm(Sal ~ OBP * OBP_350 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA| POS)
e7 <- felm(Sal ~ OBP * OBP_350 * ERA + FLD + BsR + WPA + nWPA | ID)

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)

e1 <- lm(Sal_dev ~ OBP * OBP_350 * ERA)
e2 <- lm(Sal_dev ~ OBP * OBP_350 * ERA + FLD + BsR)
e3 <- lm(Sal_dev ~ OBP * OBP_350 * ERA + FLD + BsR + AGE + AGE_sq)
e4 <- lm(Sal_dev ~ OBP * OBP_350 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

e5 <- lm(Sal_dev ~ OBP * OBP_350 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- lm(Sal_dev ~ OBP * OBP_350 * ERA + FLD + BsR + AGE + AGE_sq + Clutch + FA)
e7 <- felm(Sal_dev ~ OBP * OBP_350 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
e8 <- felm(Sal_dev ~ OBP * OBP_350 * ERA + FLD + BsR + WPA + nWPA + FA | ID)

e5 <- lm(Sal_dev ~ OBP * OBP_350 * ERA + FLD + BsR + AGE + AGE_sq + Clutch)
e6 <- felm(Sal_dev ~ OBP * OBP_350 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA| POS)
e7 <- felm(Sal_dev ~ OBP * OBP_350 * ERA + FLD + BsR + WPA + nWPA | ID)

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)


#SB=30
#linear regression
m1 <- lm(Sal ~ SB + SB_30)
m2 <- lm(Sal ~ SB + SB_30 + FLD + BAT)
m3 <- lm(Sal ~ SB + SB_30 + FLD + BAT + AGE + AGE_sq)
m4 <- lm(Sal ~ SB + SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA)

m5 <- lm(Sal ~ SB + SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA)
m6 <- lm(Sal ~ SB + SB_30 + FLD + BAT + AGE + AGE_sq + Clutch + FA)
m7 <- felm(Sal ~ SB + SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA | POS)
m8 <- felm(Sal ~ SB + SB_30 + FLD + BAT + WPA + nWPA + FA | ID)
m9 <- felm(Sal ~ SB + SB_30 + FLD + BAT + WPA + nWPA + FA | Yr)

m5 <- lm(Sal ~ SB + SB_30 + FLD + BAT + AGE + AGE_sq + Clutch)
m6 <- felm(Sal ~ SB + SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA | POS)
m7 <- felm(Sal ~ SB + SB_30 + FLD + BAT + WPA + nWPA | ID)
m9 <- felm(Sal ~ SB + SB_30 + FLD + BAT + WPA + nWPA | Yr)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)
summary(m9)

m1 <- lm(Sal_dev ~ SB + SB_30)
m2 <- lm(Sal_dev ~ SB + SB_30 + FLD + BAT)
m3 <- lm(Sal_dev ~ SB + SB_30 + FLD + BAT + AGE + AGE_sq)
m4 <- lm(Sal_dev ~ SB + SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA)

m5 <- lm(Sal_dev ~ SB + SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA)
m6 <- lm(Sal_dev ~ SB + SB_30 + FLD + BAT + AGE + AGE_sq + Clutch + FA)
m7 <- felm(Sal_dev ~ SB + SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA | POS)
m8 <- felm(Sal_dev ~ SB + SB_30 + FLD + BAT + WPA + nWPA + FA | ID)

m5 <- lm(Sal_dev ~ SB + SB_30 + FLD + BAT + AGE + AGE_sq + Clutch)
m6 <- felm(Sal_dev ~ SB + SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA | POS)
m7 <- felm(Sal_dev ~ SB + SB_30 + FLD + BAT + WPA + nWPA | ID)


summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)



#Regression discontinuity
r1 <- RDestimate(Sal ~ SB, cutpoint = 30, se.type = "HC2")
r2 <- RDestimate(Sal ~ SB | FLD + BAT, cutpoint = 30, se.type = "HC2")
r3 <- RDestimate(Sal ~ SB | FLD + BAT + AGE + AGE_sq , cutpoint = 30, se.type = "HC2")
r4 <- RDestimate(Sal ~ SB | FLD + BAT + AGE + AGE_sq + WPA + nWPA ,
                 cutpoint = 30, se.type = "HC2")

r5 <- RDestimate(Sal ~ SB | FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 30, se.type = "HC2")
r6 <- RDestimate(Sal ~ SB | FLD + BAT + AGE + AGE_sq + Clutch + FA,
                 cutpoint = 30, se.type = "HC2")

r6 <- RDestimate(Sal ~ SB | FLD + BAT + AGE + AGE_sq + Clutch,
                 cutpoint = 30, se.type = "HC2")
summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)

r1 <- RDestimate(Sal_dev ~ SB, cutpoint = 30, se.type = "HC2")
r2 <- RDestimate(Sal_dev ~ SB | FLD + BAT, cutpoint = 30, se.type = "HC2")
r3 <- RDestimate(Sal_dev ~ SB | FLD + BAT + AGE + AGE_sq , cutpoint = 30, se.type = "HC2")
r4 <- RDestimate(Sal_dev ~ SB | FLD + BAT + AGE + AGE_sq + WPA + nWPA ,
                 cutpoint = 30, se.type = "HC2")

r5 <- RDestimate(Sal_dev  ~ SB | FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 30, se.type = "HC2")
r6 <- RDestimate(Sal_dev ~ SB | FLD + BAT + AGE + AGE_sq + Clutch + FA,
                 cutpoint = 30, se.type = "HC2")

r6 <- RDestimate(Sal_dev ~ SB | FLD + BAT + AGE + AGE_sq + Clutch,
                 cutpoint = 30, se.type = "HC2")

summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)

#DID
d1 <- lm(Sal ~ SB * SB_30)
d2 <- lm(Sal ~ SB * SB_30 + FLD + BAT)
d3 <- lm(Sal ~ SB * SB_30 + FLD + BAT + AGE + AGE_sq)
d4 <- lm(Sal ~ SB * SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal ~ SB * SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal ~ SB * SB_30 + FLD + BAT + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal ~ SB * SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal ~ SB * SB_30 + FLD + BAT + WPA + nWPA + FA | ID)
d9 <- felm(Sal ~ SB * SB_30 + FLD + BAT + WPA + nWPA + FA | Yr)


d5 <- lm(Sal ~ SB * SB_30 + FLD + BAT + AGE + AGE_sq + Clutch)
d6 <- felm(Sal ~ SB * SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal ~ SB * SB_30 + FLD + BAT + WPA + nWPA | ID)
d8 <- felm(Sal ~ SB * SB_30 + FLD + BAT + WPA + nWPA | Yr)

summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)
summary(d9)

d1 <- lm(Sal ~ BsR * SB_30)
d2 <- lm(Sal ~ BsR * SB_30 + FLD + BAT)
d3 <- lm(Sal ~ BsR * SB_30 + FLD + BAT + AGE + AGE_sq)
d4 <- lm(Sal ~ BsR * SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal ~ BsR * SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal ~ BsR * SB_30 + FLD + BAT + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal ~ BsR * SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal ~ BsR * SB_30 + FLD + BAT + WPA + nWPA + FA | ID)
d9 <- felm(Sal ~ BsR * SB_30 + FLD + BAT + WPA + nWPA + FA | Yr)

d5 <- lm(Sal ~ BsR * SB_30 + FLD + BAT + AGE + AGE_sq + Clutch)
d6 <- felm(Sal ~ BsR * SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal ~ BsR * SB_30 + FLD + BAT + WPA + nWPA | ID)
d8 <- felm(Sal ~ BsR * SB_30 + FLD + BAT + WPA + nWPA | Yr)

summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)
summary(d9)

d1 <- lm(Sal_dev ~ BsR * SB_30)
d2 <- lm(Sal_dev ~ BsR * SB_30 + FLD + BAT)
d3 <- lm(Sal_dev ~ BsR * SB_30 + FLD + BAT + AGE + AGE_sq)
d4 <- lm(Sal_dev ~ BsR * SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal_dev ~ BsR * SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal_dev ~ BsR * SB_30 + FLD + BAT + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal_dev ~ BsR * SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal_dev ~ BsR * SB_30 + FLD + BAT + WPA + nWPA + FA | ID)

d5 <- lm(Sal_dev ~ BsR * SB_30 + FLD + BAT + AGE + AGE_sq + Clutch)
d6 <- felm(Sal_dev ~ BsR * SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal_dev ~ BsR * SB_30 + FLD + BAT + WPA + nWPA | ID)


summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)

#DIDID
e1 <- lm(Sal ~ SB * SB_30 * ERA)
e2 <- lm(Sal ~ SB * SB_30 * ERA + FLD + BAT)
e3 <- lm(Sal ~ SB * SB_30 * ERA + FLD + BAT + AGE + AGE_sq)
e4 <- lm(Sal ~ SB * SB_30 * ERA + FLD + BAT + AGE + AGE_sq + WPA + nWPA)

e5 <- lm(Sal ~ SB * SB_30 * ERA + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- lm(Sal ~ SB * SB_30 * ERA + FLD + BAT + AGE + AGE_sq + Clutch + FA)
e7 <- felm(Sal ~ SB * SB_30 * ERA + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA | POS)
e8 <- felm(Sal ~ SB * SB_30 * ERA + FLD + BAT + WPA + nWPA + FA | ID)

e5 <- lm(Sal ~ SB * SB_30 * ERA + FLD + BAT + AGE + AGE_sq + Clutch)
e6 <- felm(Sal ~ SB * SB_30 * ERA + FLD + BAT + AGE + AGE_sq + WPA + nWPA| POS)
e7 <- felm(Sal ~ SB * SB_30 * ERA + FLD + BAT + WPA + nWPA | ID)

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)

e1 <- lm(Sal_dev ~ SB * SB_30 * ERA)
e2 <- lm(Sal_dev ~ SB * SB_30 * ERA + FLD + BAT)
e3 <- lm(Sal_dev ~ SB * SB_30 * ERA + FLD + BAT + AGE + AGE_sq)
e4 <- lm(Sal_dev ~ SB * SB_30 * ERA + FLD + BAT + AGE + AGE_sq + WPA + nWPA)

e5 <- lm(Sal_dev ~ SB * SB_30 * ERA + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- lm(Sal_dev ~ SB * SB_30 * ERA + FLD + BAT + AGE + AGE_sq + Clutch + FA)
e7 <- felm(Sal_dev ~ SB * SB_30 * ERA + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA | POS)
e8 <- felm(Sal_dev ~ SB * SB_30 * ERA + FLD + BAT + WPA + nWPA + FA | ID)

e5 <- lm(Sal_dev ~ SB * SB_30 * ERA + FLD + BAT + AGE + AGE_sq + Clutch)
e6 <- felm(Sal_dev ~ SB * SB_30 * ERA + FLD + BAT + AGE + AGE_sq + WPA + nWPA| POS)
e7 <- felm(Sal_dev ~ SB * SB_30 * ERA + FLD + BAT + WPA + nWPA | ID)

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)

#SB=40
#linear regression
m1 <- lm(Sal ~ SB + SB_40)
m2 <- lm(Sal ~ SB + SB_40 + FLD + BAT)
m3 <- lm(Sal ~ SB + SB_40 + FLD + BAT + AGE + AGE_sq)
m4 <- lm(Sal ~ SB + SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA)

m5 <- lm(Sal ~ SB + SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA)
m6 <- lm(Sal ~ SB + SB_40 + FLD + BAT + AGE + AGE_sq + Clutch + FA)
m7 <- felm(Sal ~ SB + SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA | POS)
m8 <- felm(Sal ~ SB + SB_40 + FLD + BAT + WPA + nWPA + FA | ID)
m9 <- felm(Sal ~ SB + SB_40 + FLD + BAT + WPA + nWPA + FA | Yr)

m5 <- lm(Sal ~ SB + SB_40 + FLD + BAT + AGE + AGE_sq + Clutch)
m6 <- felm(Sal ~ SB + SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA | POS)
m7 <- felm(Sal ~ SB + SB_40 + FLD + BAT + WPA + nWPA | ID)
m9 <- felm(Sal ~ SB + SB_40 + FLD + BAT + WPA + nWPA | Yr)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)
summary(m9)

m1 <- lm(Sal_dev ~ SB + SB_40)
m2 <- lm(Sal_dev ~ SB + SB_40 + FLD + BAT)
m3 <- lm(Sal_dev ~ SB + SB_40 + FLD + BAT + AGE + AGE_sq)
m4 <- lm(Sal_dev ~ SB + SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA)

m5 <- lm(Sal_dev ~ SB + SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA)
m6 <- lm(Sal_dev ~ SB + SB_40 + FLD + BAT + AGE + AGE_sq + Clutch + FA)
m7 <- felm(Sal_dev ~ SB + SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA | POS)
m8 <- felm(Sal_dev ~ SB + SB_40 + FLD + BAT + WPA + nWPA + FA | ID)

m5 <- lm(Sal_dev ~ SB + SB_40 + FLD + BAT + AGE + AGE_sq + Clutch)
m6 <- felm(Sal_dev ~ SB + SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA | POS)
m7 <- felm(Sal_dev ~ SB + SB_40 + FLD + BAT + WPA + nWPA | ID)


summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)



#Regression discontinuity
r1 <- RDestimate(Sal ~ SB, cutpoint = 40, se.type = "HC2")
r2 <- RDestimate(Sal ~ SB | FLD + BAT, cutpoint = 40, se.type = "HC2")
r3 <- RDestimate(Sal ~ SB | FLD + BAT + AGE + AGE_sq , cutpoint = 40, se.type = "HC2")
r4 <- RDestimate(Sal ~ SB | FLD + BAT + AGE + AGE_sq + WPA + nWPA ,
                 cutpoint = 40, se.type = "HC2")

r5 <- RDestimate(Sal ~ SB | FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 40, se.type = "HC2")
r6 <- RDestimate(Sal ~ SB | FLD + BAT + AGE + AGE_sq + Clutch + FA,
                 cutpoint = 40, se.type = "HC2")

r6 <- RDestimate(Sal ~ SB | FLD + BAT + AGE + AGE_sq + Clutch,
                 cutpoint = 40, se.type = "HC2")
summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)

r1 <- RDestimate(Sal_dev ~ SB, cutpoint = 40, se.type = "HC2")
r2 <- RDestimate(Sal_dev ~ SB | FLD + BAT, cutpoint = 40, se.type = "HC2")
r3 <- RDestimate(Sal_dev ~ SB | FLD + BAT + AGE + AGE_sq , cutpoint = 40, se.type = "HC2")
r4 <- RDestimate(Sal_dev ~ SB | FLD + BAT + AGE + AGE_sq + WPA + nWPA ,
                 cutpoint = 40, se.type = "HC2")

r5 <- RDestimate(Sal_dev  ~ SB | FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 40, se.type = "HC2")
r6 <- RDestimate(Sal_dev ~ SB | FLD + BAT + AGE + AGE_sq + Clutch + FA,
                 cutpoint = 40, se.type = "HC2")

r6 <- RDestimate(Sal_dev ~ SB | FLD + BAT + AGE + AGE_sq + Clutch,
                 cutpoint = 40, se.type = "HC2")

summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)

#DID
d1 <- lm(Sal ~ SB * SB_40)
d2 <- lm(Sal ~ SB * SB_40 + FLD + BAT)
d3 <- lm(Sal ~ SB * SB_40 + FLD + BAT + AGE + AGE_sq)
d4 <- lm(Sal ~ SB * SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal ~ SB * SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal ~ SB * SB_40 + FLD + BAT + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal ~ SB * SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal ~ SB * SB_40 + FLD + BAT + WPA + nWPA + FA | ID)
d9 <- felm(Sal ~ SB * SB_40 + FLD + BAT + WPA + nWPA + FA | Yr)


d5 <- lm(Sal ~ SB * SB_40 + FLD + BAT + AGE + AGE_sq + Clutch)
d6 <- felm(Sal ~ SB * SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal ~ SB * SB_40 + FLD + BAT + WPA + nWPA | ID)
d8 <- felm(Sal ~ SB * SB_40 + FLD + BAT + WPA + nWPA | Yr)

summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)
summary(d9)

d1 <- lm(Sal ~ BsR * SB_40)
d2 <- lm(Sal ~ BsR * SB_40 + FLD + BAT)
d3 <- lm(Sal ~ BsR * SB_40 + FLD + BAT + AGE + AGE_sq)
d4 <- lm(Sal ~ BsR * SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal ~ BsR * SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal ~ BsR * SB_40 + FLD + BAT + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal ~ BsR * SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal ~ BsR * SB_40 + FLD + BAT + WPA + nWPA + FA | ID)
d9 <- felm(Sal ~ BsR * SB_40 + FLD + BAT + WPA + nWPA + FA | Yr)

d5 <- lm(Sal ~ BsR * SB_40 + FLD + BAT + AGE + AGE_sq + Clutch)
d6 <- felm(Sal ~ BsR * SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal ~ BsR * SB_40 + FLD + BAT + WPA + nWPA | ID)
d8 <- felm(Sal ~ BsR * SB_40 + FLD + BAT + WPA + nWPA | Yr)

summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)
summary(d9)

d1 <- lm(Sal_dev ~ BsR * SB_40)
d2 <- lm(Sal_dev ~ BsR * SB_40 + FLD + BAT)
d3 <- lm(Sal_dev ~ BsR * SB_40 + FLD + BAT + AGE + AGE_sq)
d4 <- lm(Sal_dev ~ BsR * SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal_dev ~ BsR * SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal_dev ~ BsR * SB_40 + FLD + BAT + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal_dev ~ BsR * SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal_dev ~ BsR * SB_40 + FLD + BAT + WPA + nWPA + FA | ID)

d5 <- lm(Sal_dev ~ BsR * SB_40 + FLD + BAT + AGE + AGE_sq + Clutch)
d6 <- felm(Sal_dev ~ BsR * SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal_dev ~ BsR * SB_40 + FLD + BAT + WPA + nWPA | ID)


summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)

#DIDID
e1 <- lm(Sal ~ SB * SB_40 * ERA)
e2 <- lm(Sal ~ SB * SB_40 * ERA + FLD + BAT)
e3 <- lm(Sal ~ SB * SB_40 * ERA + FLD + BAT + AGE + AGE_sq)
e4 <- lm(Sal ~ SB * SB_40 * ERA + FLD + BAT + AGE + AGE_sq + WPA + nWPA)

e5 <- lm(Sal ~ SB * SB_40 * ERA + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- lm(Sal ~ SB * SB_40 * ERA + FLD + BAT + AGE + AGE_sq + Clutch + FA)
e7 <- felm(Sal ~ SB * SB_40 * ERA + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA | POS)
e8 <- felm(Sal ~ SB * SB_40 * ERA + FLD + BAT + WPA + nWPA + FA | ID)

e5 <- lm(Sal ~ SB * SB_40 * ERA + FLD + BAT + AGE + AGE_sq + Clutch)
e6 <- felm(Sal ~ SB * SB_40 * ERA + FLD + BAT + AGE + AGE_sq + WPA + nWPA| POS)
e7 <- felm(Sal ~ SB * SB_40 * ERA + FLD + BAT + WPA + nWPA | ID)

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)

e1 <- lm(Sal_dev ~ SB * SB_40 * ERA)
e2 <- lm(Sal_dev ~ SB * SB_40 * ERA + FLD + BAT)
e3 <- lm(Sal_dev ~ SB * SB_40 * ERA + FLD + BAT + AGE + AGE_sq)
e4 <- lm(Sal_dev ~ SB * SB_40 * ERA + FLD + BAT + AGE + AGE_sq + WPA + nWPA)

e5 <- lm(Sal_dev ~ SB * SB_40 * ERA + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- lm(Sal_dev ~ SB * SB_40 * ERA + FLD + BAT + AGE + AGE_sq + Clutch + FA)
e7 <- felm(Sal_dev ~ SB * SB_40 * ERA + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA | POS)
e8 <- felm(Sal_dev ~ SB * SB_40 * ERA + FLD + BAT + WPA + nWPA + FA | ID)

e5 <- lm(Sal_dev ~ SB * SB_40 * ERA + FLD + BAT + AGE + AGE_sq + Clutch)
e6 <- felm(Sal_dev ~ SB * SB_40 * ERA + FLD + BAT + AGE + AGE_sq + WPA + nWPA| POS)
e7 <- felm(Sal_dev ~ SB * SB_40 * ERA + FLD + BAT + WPA + nWPA | ID)

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)


#OPS
#linear regression
m1 <- lm(Sal ~ OPS + OPS_1000)
m2 <- lm(Sal ~ OPS + OPS_1000 + FLD + BsR)
m3 <- lm(Sal ~ OPS + OPS_1000 + FLD + BsR + AGE + AGE_sq)
m4 <- lm(Sal ~ OPS + OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

m5 <- lm(Sal ~ OPS + OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
m6 <- lm(Sal ~ OPS + OPS_1000 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
m7 <- felm(Sal ~ OPS + OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
m8 <- felm(Sal ~ OPS + OPS_1000 + FLD + BsR + WPA + nWPA + FA | ID)
m9 <- felm(Sal ~ OPS + OPS_1000 + FLD + BsR + WPA + nWPA + FA | Yr)

m5 <- lm(Sal ~ OPS + OPS_1000 + FLD + BsR + AGE + AGE_sq + Clutch)
m6 <- felm(Sal ~ OPS + OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
m7 <- felm(Sal ~ OPS + OPS_1000 + FLD + BsR + WPA + nWPA | ID)
m9 <- felm(Sal ~ OPS + OPS_1000 + FLD + BsR + WPA + nWPA | Yr)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)
summary(m9)

m1 <- lm(Sal_dev ~ OPS + OPS_1000)
m2 <- lm(Sal_dev ~ OPS + OPS_1000 + FLD + BsR)
m3 <- lm(Sal_dev ~ OPS + OPS_1000 + FLD + BsR + AGE + AGE_sq)
m4 <- lm(Sal_dev ~ OPS + OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

m5 <- lm(Sal_dev ~ OPS + OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
m6 <- lm(Sal_dev ~ OPS + OPS_1000 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
m7 <- felm(Sal_dev ~ OPS + OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
m8 <- felm(Sal_dev ~ OPS + OPS_1000 + FLD + BsR + WPA + nWPA + FA | ID)

m5 <- lm(Sal_dev ~ OPS + OPS_1000 + FLD + BsR + AGE + AGE_sq + Clutch)
m6 <- felm(Sal_dev ~ OPS + OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
m7 <- felm(Sal_dev ~ OPS + OPS_1000 + FLD + BsR + WPA + nWPA | ID)


summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)



#Regression discontinuity
r1 <- RDestimate(Sal ~ OPS, cutpoint = 1, se.type = "HC2")
r2 <- RDestimate(Sal ~ OPS | FLD + BsR, cutpoint = 1, se.type = "HC2")
r3 <- RDestimate(Sal ~ OPS | FLD + BsR + AGE + AGE_sq , cutpoint = 1, se.type = "HC2")
r4 <- RDestimate(Sal ~ OPS | FLD + BsR + AGE + AGE_sq + WPA + nWPA ,
                 cutpoint = 1, se.type = "HC2")

r5 <- RDestimate(Sal ~ OPS | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 1, se.type = "HC2")
r6 <- RDestimate(Sal ~ OPS | FLD + BsR + AGE + AGE_sq + Clutch + FA,
                 cutpoint = 1, se.type = "HC2")

r6 <- RDestimate(Sal ~ OPS | FLD + BsR + AGE + AGE_sq + Clutch,
                 cutpoint = 1, se.type = "HC2")
summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)

r1 <- RDestimate(Sal_dev ~ OPS, cutpoint = 1, se.type = "HC2")
r2 <- RDestimate(Sal_dev ~ OPS | FLD + BsR, cutpoint = 1, se.type = "HC2")
r3 <- RDestimate(Sal_dev ~ OPS | FLD + BsR + AGE + AGE_sq , cutpoint = 1, se.type = "HC2")
r4 <- RDestimate(Sal_dev ~ OPS | FLD + BsR + AGE + AGE_sq + WPA + nWPA ,
                 cutpoint = 1, se.type = "HC2")

r5 <- RDestimate(Sal_dev  ~ OPS | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 1, se.type = "HC2")
r6 <- RDestimate(Sal_dev ~ OPS | FLD + BsR + AGE + AGE_sq + Clutch + FA,
                 cutpoint = 1, se.type = "HC2")

r6 <- RDestimate(Sal_dev ~ OPS | FLD + BsR + AGE + AGE_sq + Clutch,
                 cutpoint = 1, se.type = "HC2")

summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)

#DID
d1 <- lm(Sal ~ OPS * OPS_1000)
d2 <- lm(Sal ~ OPS * OPS_1000 + FLD + BsR)
d3 <- lm(Sal ~ OPS * OPS_1000 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal ~ OPS * OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal ~ OPS * OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal ~ OPS * OPS_1000 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal ~ OPS * OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal ~ OPS * OPS_1000 + FLD + BsR + WPA + nWPA + FA | ID)
d9 <- felm(Sal ~ OPS * OPS_1000 + FLD + BsR + WPA + nWPA + FA | Yr)


d5 <- lm(Sal ~ OPS * OPS_1000 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal ~ OPS * OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal ~ OPS * OPS_1000 + FLD + BsR + WPA + nWPA | ID)
d8 <- felm(Sal ~ OPS * OPS_1000 + FLD + BsR + WPA + nWPA | Yr)

summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)
summary(d9)

d1 <- lm(Sal ~ BAT * OPS_1000)
d2 <- lm(Sal ~ BAT * OPS_1000 + FLD + BsR)
d3 <- lm(Sal ~ BAT * OPS_1000 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal ~ BAT * OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal ~ BAT * OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal ~ BAT * OPS_1000 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal ~ BAT * OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal ~ BAT * OPS_1000 + FLD + BsR + WPA + nWPA + FA | ID)
d9 <- felm(Sal ~ BAT * OPS_1000 + FLD + BsR + WPA + nWPA + FA | Yr)

d5 <- lm(Sal ~ BAT * OPS_1000 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal ~ BAT * OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal ~ BAT * OPS_1000 + FLD + BsR + WPA + nWPA | ID)
d8 <- felm(Sal ~ BAT * OPS_1000 + FLD + BsR + WPA + nWPA | Yr)

summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)
summary(d9)

d1 <- lm(Sal_dev ~ BAT * OPS_1000)
d2 <- lm(Sal_dev ~ BAT * OPS_1000 + FLD + BsR)
d3 <- lm(Sal_dev ~ BAT * OPS_1000 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal_dev ~ BAT * OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal_dev ~ BAT * OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal_dev ~ BAT * OPS_1000 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal_dev ~ BAT * OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal_dev ~ BAT * OPS_1000 + FLD + BsR + WPA + nWPA + FA | ID)

d5 <- lm(Sal_dev ~ BAT * OPS_1000 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal_dev ~ BAT * OPS_1000 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal_dev ~ BAT * OPS_1000 + FLD + BsR + WPA + nWPA | ID)


summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)

#DIDID
e1 <- lm(Sal ~ OPS * OPS_1000 * ERA)
e2 <- lm(Sal ~ OPS * OPS_1000 * ERA + FLD + BsR)
e3 <- lm(Sal ~ OPS * OPS_1000 * ERA + FLD + BsR + AGE + AGE_sq)
e4 <- lm(Sal ~ OPS * OPS_1000 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

e5 <- lm(Sal ~ OPS * OPS_1000 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- lm(Sal ~ OPS * OPS_1000 * ERA + FLD + BsR + AGE + AGE_sq + Clutch + FA)
e7 <- felm(Sal ~ OPS * OPS_1000 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
e8 <- felm(Sal ~ OPS * OPS_1000 * ERA + FLD + BsR + WPA + nWPA + FA | ID)

e5 <- lm(Sal ~ OPS * OPS_1000 * ERA + FLD + BsR + AGE + AGE_sq + Clutch)
e6 <- felm(Sal ~ OPS * OPS_1000 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA| POS)
e7 <- felm(Sal ~ OPS * OPS_1000 * ERA + FLD + BsR + WPA + nWPA | ID)

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)

e1 <- lm(Sal_dev ~ OPS * OPS_1000 * ERA)
e2 <- lm(Sal_dev ~ OPS * OPS_1000 * ERA + FLD + BsR)
e3 <- lm(Sal_dev ~ OPS * OPS_1000 * ERA + FLD + BsR + AGE + AGE_sq)
e4 <- lm(Sal_dev ~ OPS * OPS_1000 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

e5 <- lm(Sal_dev ~ OPS * OPS_1000 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- lm(Sal_dev ~ OPS * OPS_1000 * ERA + FLD + BsR + AGE + AGE_sq + Clutch + FA)
e7 <- felm(Sal_dev ~ OPS * OPS_1000 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
e8 <- felm(Sal_dev ~ OPS * OPS_1000 * ERA + FLD + BsR + WPA + nWPA + FA | ID)

e5 <- lm(Sal_dev ~ OPS * OPS_1000 * ERA + FLD + BsR + AGE + AGE_sq + Clutch)
e6 <- felm(Sal_dev ~ OPS * OPS_1000 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA| POS)
e7 <- felm(Sal_dev ~ OPS * OPS_1000 * ERA + FLD + BsR + WPA + nWPA | ID)

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)


#PA
#linear regression
m1 <- lm(Sal ~ PA + PA_500)
m2 <- lm(Sal ~ PA + PA_500 + FLD + BsR)
m3 <- lm(Sal ~ PA + PA_500 + FLD + BsR + AGE + AGE_sq)
m4 <- lm(Sal ~ PA + PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

m5 <- lm(Sal ~ PA + PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
m6 <- lm(Sal ~ PA + PA_500 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
m7 <- felm(Sal ~ PA + PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
m8 <- felm(Sal ~ PA + PA_500 + FLD + BsR + WPA + nWPA + FA | ID)
m9 <- felm(Sal ~ PA + PA_500 + FLD + BsR + WPA + nWPA + FA | Yr)

m5 <- lm(Sal ~ PA + PA_500 + FLD + BsR + AGE + AGE_sq + Clutch)
m6 <- felm(Sal ~ PA + PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
m7 <- felm(Sal ~ PA + PA_500 + FLD + BsR + WPA + nWPA | ID)
m9 <- felm(Sal ~ PA + PA_500 + FLD + BsR + WPA + nWPA | Yr)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)
summary(m9)

m1 <- lm(Sal_dev ~ PA + PA_500)
m2 <- lm(Sal_dev ~ PA + PA_500 + FLD + BsR)
m3 <- lm(Sal_dev ~ PA + PA_500 + FLD + BsR + AGE + AGE_sq)
m4 <- lm(Sal_dev ~ PA + PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

m5 <- lm(Sal_dev ~ PA + PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
m6 <- lm(Sal_dev ~ PA + PA_500 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
m7 <- felm(Sal_dev ~ PA + PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
m8 <- felm(Sal_dev ~ PA + PA_500 + FLD + BsR + WPA + nWPA + FA | ID)

m5 <- lm(Sal_dev ~ PA + PA_500 + FLD + BsR + AGE + AGE_sq + Clutch)
m6 <- felm(Sal_dev ~ PA + PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
m7 <- felm(Sal_dev ~ PA + PA_500 + FLD + BsR + WPA + nWPA | ID)


summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)



#Regression discontinuity
r1 <- RDestimate(Sal ~ PA, cutpoint = 500, se.type = "HC2")
r2 <- RDestimate(Sal ~ PA | FLD + BsR, cutpoint = 500, se.type = "HC2")
r3 <- RDestimate(Sal ~ PA | FLD + BsR + AGE + AGE_sq , cutpoint = 500, se.type = "HC2")
r4 <- RDestimate(Sal ~ PA | FLD + BsR + AGE + AGE_sq + WPA + nWPA ,
                 cutpoint = 500, se.type = "HC2")

r5 <- RDestimate(Sal ~ PA | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 500, se.type = "HC2")
r6 <- RDestimate(Sal ~ PA | FLD + BsR + AGE + AGE_sq + Clutch + FA,
                 cutpoint = 500, se.type = "HC2")

r6 <- RDestimate(Sal ~ PA | FLD + BsR + AGE + AGE_sq + Clutch,
                 cutpoint = 500, se.type = "HC2")
summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)

r1 <- RDestimate(Sal_dev ~ PA, cutpoint = 500, se.type = "HC2")
r2 <- RDestimate(Sal_dev ~ PA | FLD + BsR, cutpoint = 500, se.type = "HC2")
r3 <- RDestimate(Sal_dev ~ PA | FLD + BsR + AGE + AGE_sq , cutpoint = 500, se.type = "HC2")
r4 <- RDestimate(Sal_dev ~ PA | FLD + BsR + AGE + AGE_sq + WPA + nWPA ,
                 cutpoint = 500, se.type = "HC2")

r5 <- RDestimate(Sal_dev  ~ PA | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 500, se.type = "HC2")
r6 <- RDestimate(Sal_dev ~ PA | FLD + BsR + AGE + AGE_sq + Clutch + FA,
                 cutpoint = 500, se.type = "HC2")

r6 <- RDestimate(Sal_dev ~ PA | FLD + BsR + AGE + AGE_sq + Clutch,
                 cutpoint = 500, se.type = "HC2")

summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)

#DID
d1 <- lm(Sal ~ PA * PA_500)
d2 <- lm(Sal ~ PA * PA_500 + FLD + BsR)
d3 <- lm(Sal ~ PA * PA_500 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal ~ PA * PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal ~ PA * PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal ~ PA * PA_500 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal ~ PA * PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal ~ PA * PA_500 + FLD + BsR + WPA + nWPA + FA | ID)
d9 <- felm(Sal ~ PA * PA_500 + FLD + BsR + WPA + nWPA + FA | Yr)


d5 <- lm(Sal ~ PA * PA_500 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal ~ PA * PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal ~ PA * PA_500 + FLD + BsR + WPA + nWPA | ID)
d8 <- felm(Sal ~ PA * PA_500 + FLD + BsR + WPA + nWPA | Yr)

summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)
summary(d9)

d1 <- lm(Sal ~ BAT * PA_500)
d2 <- lm(Sal ~ BAT * PA_500 + FLD + BsR)
d3 <- lm(Sal ~ BAT * PA_500 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal ~ BAT * PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal ~ BAT * PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal ~ BAT * PA_500 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal ~ BAT * PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal ~ BAT * PA_500 + FLD + BsR + WPA + nWPA + FA | ID)
d9 <- felm(Sal ~ BAT * PA_500 + FLD + BsR + WPA + nWPA + FA | Yr)

d5 <- lm(Sal ~ BAT * PA_500 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal ~ BAT * PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal ~ BAT * PA_500 + FLD + BsR + WPA + nWPA | ID)
d8 <- felm(Sal ~ BAT * PA_500 + FLD + BsR + WPA + nWPA | Yr)

summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)
summary(d9)

d1 <- lm(Sal_dev ~ BAT * PA_500)
d2 <- lm(Sal_dev ~ BAT * PA_500 + FLD + BsR)
d3 <- lm(Sal_dev ~ BAT * PA_500 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal_dev ~ BAT * PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal_dev ~ BAT * PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal_dev ~ BAT * PA_500 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal_dev ~ BAT * PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal_dev ~ BAT * PA_500 + FLD + BsR + WPA + nWPA + FA | ID)

d5 <- lm(Sal_dev ~ BAT * PA_500 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal_dev ~ BAT * PA_500 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal_dev ~ BAT * PA_500 + FLD + BsR + WPA + nWPA | ID)


summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)

#DIDID
e1 <- lm(Sal ~ PA * PA_500 * ERA)
e2 <- lm(Sal ~ PA * PA_500 * ERA + FLD + BsR)
e3 <- lm(Sal ~ PA * PA_500 * ERA + FLD + BsR + AGE + AGE_sq)
e4 <- lm(Sal ~ PA * PA_500 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

e5 <- lm(Sal ~ PA * PA_500 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- lm(Sal ~ PA * PA_500 * ERA + FLD + BsR + AGE + AGE_sq + Clutch + FA)
e7 <- felm(Sal ~ PA * PA_500 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
e8 <- felm(Sal ~ PA * PA_500 * ERA + FLD + BsR + WPA + nWPA + FA | ID)

e5 <- lm(Sal ~ PA * PA_500 * ERA + FLD + BsR + AGE + AGE_sq + Clutch)
e6 <- felm(Sal ~ PA * PA_500 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA| POS)
e7 <- felm(Sal ~ PA * PA_500 * ERA + FLD + BsR + WPA + nWPA | ID)

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)

e1 <- lm(Sal_dev ~ PA * PA_500 * ERA)
e2 <- lm(Sal_dev ~ PA * PA_500 * ERA + FLD + BsR)
e3 <- lm(Sal_dev ~ PA * PA_500 * ERA + FLD + BsR + AGE + AGE_sq)
e4 <- lm(Sal_dev ~ PA * PA_500 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

e5 <- lm(Sal_dev ~ PA * PA_500 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- lm(Sal_dev ~ PA * PA_500 * ERA + FLD + BsR + AGE + AGE_sq + Clutch + FA)
e7 <- felm(Sal_dev ~ PA * PA_500 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
e8 <- felm(Sal_dev ~ PA * PA_500 * ERA + FLD + BsR + WPA + nWPA + FA | ID)

e5 <- lm(Sal_dev ~ PA * PA_500 * ERA + FLD + BsR + AGE + AGE_sq + Clutch)
e6 <- felm(Sal_dev ~ PA * PA_500 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA| POS)
e7 <- felm(Sal_dev ~ PA * PA_500 * ERA + FLD + BsR + WPA + nWPA | ID)

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)


#H
#linear regression
m1 <- lm(Sal ~ H + H_200)
m2 <- lm(Sal ~ H + H_200 + FLD + BsR)
m3 <- lm(Sal ~ H + H_200 + FLD + BsR + AGE + AGE_sq)
m4 <- lm(Sal ~ H + H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

m5 <- lm(Sal ~ H + H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
m6 <- lm(Sal ~ H + H_200 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
m7 <- felm(Sal ~ H + H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
m8 <- felm(Sal ~ H + H_200 + FLD + BsR + WPA + nWPA + FA | ID)
m9 <- felm(Sal ~ H + H_200 + FLD + BsR + WPA + nWPA + FA | Yr)

m5 <- lm(Sal ~ H + H_200 + FLD + BsR + AGE + AGE_sq + Clutch)
m6 <- felm(Sal ~ H + H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
m7 <- felm(Sal ~ H + H_200 + FLD + BsR + WPA + nWPA | ID)
m9 <- felm(Sal ~ H + H_200 + FLD + BsR + WPA + nWPA | Yr)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)
summary(m9)

m1 <- lm(Sal_dev ~ H + H_200)
m2 <- lm(Sal_dev ~ H + H_200 + FLD + BsR)
m3 <- lm(Sal_dev ~ H + H_200 + FLD + BsR + AGE + AGE_sq)
m4 <- lm(Sal_dev ~ H + H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

m5 <- lm(Sal_dev ~ H + H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
m6 <- lm(Sal_dev ~ H + H_200 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
m7 <- felm(Sal_dev ~ H + H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
m8 <- felm(Sal_dev ~ H + H_200 + FLD + BsR + WPA + nWPA + FA | ID)

m5 <- lm(Sal_dev ~ H + H_200 + FLD + BsR + AGE + AGE_sq + Clutch)
m6 <- felm(Sal_dev ~ H + H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
m7 <- felm(Sal_dev ~ H + H_200 + FLD + BsR + WPA + nWPA | ID)


summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)



#Regression discontinuity
r1 <- RDestimate(Sal ~ H, cutpoint = 200, se.type = "HC2")
r2 <- RDestimate(Sal ~ H | FLD + BsR, cutpoint = 200, se.type = "HC2")
r3 <- RDestimate(Sal ~ H | FLD + BsR + AGE + AGE_sq , cutpoint = 200, se.type = "HC2")
r4 <- RDestimate(Sal ~ H | FLD + BsR + AGE + AGE_sq + WPA + nWPA ,
                 cutpoint = 200, se.type = "HC2")

r5 <- RDestimate(Sal ~ H | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 200, se.type = "HC2")
r6 <- RDestimate(Sal ~ H | FLD + BsR + AGE + AGE_sq + Clutch + FA,
                 cutpoint = 200, se.type = "HC2")

r6 <- RDestimate(Sal ~ H | FLD + BsR + AGE + AGE_sq + Clutch,
                 cutpoint = 200, se.type = "HC2")
summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)

r1 <- RDestimate(Sal_dev ~ H, cutpoint = 200, se.type = "HC2")
r2 <- RDestimate(Sal_dev ~ H | FLD + BsR, cutpoint = 200, se.type = "HC2")
r3 <- RDestimate(Sal_dev ~ H | FLD + BsR + AGE + AGE_sq , cutpoint = 200, se.type = "HC2")
r4 <- RDestimate(Sal_dev ~ H | FLD + BsR + AGE + AGE_sq + WPA + nWPA ,
                 cutpoint = 200, se.type = "HC2")

r5 <- RDestimate(Sal_dev  ~ H | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 200, se.type = "HC2")
r6 <- RDestimate(Sal_dev ~ H | FLD + BsR + AGE + AGE_sq + Clutch + FA,
                 cutpoint = 200, se.type = "HC2")

r6 <- RDestimate(Sal_dev ~ H | FLD + BsR + AGE + AGE_sq + Clutch,
                 cutpoint = 200, se.type = "HC2")

summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)

#DID
d1 <- lm(Sal ~ H * H_200)
d2 <- lm(Sal ~ H * H_200 + FLD + BsR)
d3 <- lm(Sal ~ H * H_200 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal ~ H * H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal ~ H * H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal ~ H * H_200 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal ~ H * H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal ~ H * H_200 + FLD + BsR + WPA + nWPA + FA | ID)
d9 <- felm(Sal ~ H * H_200 + FLD + BsR + WPA + nWPA + FA | Yr)


d5 <- lm(Sal ~ H * H_200 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal ~ H * H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal ~ H * H_200 + FLD + BsR + WPA + nWPA | ID)
d8 <- felm(Sal ~ H * H_200 + FLD + BsR + WPA + nWPA | Yr)

summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)
summary(d9)

d1 <- lm(Sal ~ BAT * H_200)
d2 <- lm(Sal ~ BAT * H_200 + FLD + BsR)
d3 <- lm(Sal ~ BAT * H_200 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal ~ BAT * H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal ~ BAT * H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal ~ BAT * H_200 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal ~ BAT * H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal ~ BAT * H_200 + FLD + BsR + WPA + nWPA + FA | ID)
d9 <- felm(Sal ~ BAT * H_200 + FLD + BsR + WPA + nWPA + FA | Yr)

d5 <- lm(Sal ~ BAT * H_200 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal ~ BAT * H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal ~ BAT * H_200 + FLD + BsR + WPA + nWPA | ID)
d8 <- felm(Sal ~ BAT * H_200 + FLD + BsR + WPA + nWPA | Yr)

summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)
summary(d9)

d1 <- lm(Sal_dev ~ BAT * H_200)
d2 <- lm(Sal_dev ~ BAT * H_200 + FLD + BsR)
d3 <- lm(Sal_dev ~ BAT * H_200 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal_dev ~ BAT * H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal_dev ~ BAT * H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal_dev ~ BAT * H_200 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal_dev ~ BAT * H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal_dev ~ BAT * H_200 + FLD + BsR + WPA + nWPA + FA | ID)

d5 <- lm(Sal_dev ~ BAT * H_200 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal_dev ~ BAT * H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal_dev ~ BAT * H_200 + FLD + BsR + WPA + nWPA | ID)


summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)

#DIDID
e1 <- lm(Sal ~ H * H_200 * ERA)
e2 <- lm(Sal ~ H * H_200 * ERA + FLD + BsR)
e3 <- lm(Sal ~ H * H_200 * ERA + FLD + BsR + AGE + AGE_sq)
e4 <- lm(Sal ~ H * H_200 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

e5 <- lm(Sal ~ H * H_200 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- lm(Sal ~ H * H_200 * ERA + FLD + BsR + AGE + AGE_sq + Clutch + FA)
e7 <- felm(Sal ~ H * H_200 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
e8 <- felm(Sal ~ H * H_200 * ERA + FLD + BsR + WPA + nWPA + FA | ID)

e5 <- lm(Sal ~ H * H_200 * ERA + FLD + BsR + AGE + AGE_sq + Clutch)
e6 <- felm(Sal ~ H * H_200 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA| POS)
e7 <- felm(Sal ~ H * H_200 * ERA + FLD + BsR + WPA + nWPA | ID)

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)

e1 <- lm(Sal_dev ~ H * H_200 * ERA)
e2 <- lm(Sal_dev ~ H * H_200 * ERA + FLD + BsR)
e3 <- lm(Sal_dev ~ H * H_200 * ERA + FLD + BsR + AGE + AGE_sq)
e4 <- lm(Sal_dev ~ H * H_200 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

e5 <- lm(Sal_dev ~ H * H_200 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- lm(Sal_dev ~ H * H_200 * ERA + FLD + BsR + AGE + AGE_sq + Clutch + FA)
e7 <- felm(Sal_dev ~ H * H_200 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
e8 <- felm(Sal_dev ~ H * H_200 * ERA + FLD + BsR + WPA + nWPA + FA | ID)

e5 <- lm(Sal_dev ~ H * H_200 * ERA + FLD + BsR + AGE + AGE_sq + Clutch)
e6 <- felm(Sal_dev ~ H * H_200 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA| POS)
e7 <- felm(Sal_dev ~ H * H_200 * ERA + FLD + BsR + WPA + nWPA | ID)

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)


#RBI
#linear regression
m1 <- lm(Sal ~ RBI + RBI_100)
m2 <- lm(Sal ~ RBI + RBI_100 + FLD + BsR)
m3 <- lm(Sal ~ RBI + RBI_100 + FLD + BsR + AGE + AGE_sq)
m4 <- lm(Sal ~ RBI + RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

m5 <- lm(Sal ~ RBI + RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
m6 <- lm(Sal ~ RBI + RBI_100 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
m7 <- felm(Sal ~ RBI + RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
m8 <- felm(Sal ~ RBI + RBI_100 + FLD + BsR + WPA + nWPA + FA | ID)
m9 <- felm(Sal ~ RBI + RBI_100 + FLD + BsR + WPA + nWPA + FA | Yr)

m5 <- lm(Sal ~ RBI + RBI_100 + FLD + BsR + AGE + AGE_sq + Clutch)
m6 <- felm(Sal ~ RBI + RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
m7 <- felm(Sal ~ RBI + RBI_100 + FLD + BsR + WPA + nWPA | ID)
m9 <- felm(Sal ~ RBI + RBI_100 + FLD + BsR + WPA + nWPA | Yr)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)
summary(m9)

m1 <- lm(Sal_dev ~ RBI + RBI_100)
m2 <- lm(Sal_dev ~ RBI + RBI_100 + FLD + BsR)
m3 <- lm(Sal_dev ~ RBI + RBI_100 + FLD + BsR + AGE + AGE_sq)
m4 <- lm(Sal_dev ~ RBI + RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

m5 <- lm(Sal_dev ~ RBI + RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
m6 <- lm(Sal_dev ~ RBI + RBI_100 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
m7 <- felm(Sal_dev ~ RBI + RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
m8 <- felm(Sal_dev ~ RBI + RBI_100 + FLD + BsR + WPA + nWPA + FA | ID)

m5 <- lm(Sal_dev ~ RBI + RBI_100 + FLD + BsR + AGE + AGE_sq + Clutch)
m6 <- felm(Sal_dev ~ RBI + RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
m7 <- felm(Sal_dev ~ RBI + RBI_100 + FLD + BsR + WPA + nWPA | ID)


summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)



#Regression discontinuity
r1 <- RDestimate(Sal ~ RBI, cutpoint = 100, se.type = "HC2")
r2 <- RDestimate(Sal ~ RBI | FLD + BsR, cutpoint = 100, se.type = "HC2")
r3 <- RDestimate(Sal ~ RBI | FLD + BsR + AGE + AGE_sq , cutpoint = 100, se.type = "HC2")
r4 <- RDestimate(Sal ~ RBI | FLD + BsR + AGE + AGE_sq + WPA + nWPA ,
                 cutpoint = 100, se.type = "HC2")

r5 <- RDestimate(Sal ~ RBI | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 100, se.type = "HC2")
r6 <- RDestimate(Sal ~ RBI | FLD + BsR + AGE + AGE_sq + Clutch + FA,
                 cutpoint = 100, se.type = "HC2")

r6 <- RDestimate(Sal ~ RBI | FLD + BsR + AGE + AGE_sq + Clutch,
                 cutpoint = 100, se.type = "HC2")
summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)

r1 <- RDestimate(Sal_dev ~ RBI, cutpoint = 100, se.type = "HC2")
r2 <- RDestimate(Sal_dev ~ RBI | FLD + BsR, cutpoint = 100, se.type = "HC2")
r3 <- RDestimate(Sal_dev ~ RBI | FLD + BsR + AGE + AGE_sq , cutpoint = 100, se.type = "HC2")
r4 <- RDestimate(Sal_dev ~ RBI | FLD + BsR + AGE + AGE_sq + WPA + nWPA ,
                 cutpoint = 100, se.type = "HC2")

r5 <- RDestimate(Sal_dev  ~ RBI | FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA,
                 cutpoint = 100, se.type = "HC2")
r6 <- RDestimate(Sal_dev ~ RBI | FLD + BsR + AGE + AGE_sq + Clutch + FA,
                 cutpoint = 100, se.type = "HC2")

r6 <- RDestimate(Sal_dev ~ RBI | FLD + BsR + AGE + AGE_sq + Clutch,
                 cutpoint = 100, se.type = "HC2")

summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)

#DID
d1 <- lm(Sal ~ RBI * RBI_100)
d2 <- lm(Sal ~ RBI * RBI_100 + FLD + BsR)
d3 <- lm(Sal ~ RBI * RBI_100 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal ~ RBI * RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal ~ RBI * RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal ~ RBI * RBI_100 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal ~ RBI * RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal ~ RBI * RBI_100 + FLD + BsR + WPA + nWPA + FA | ID)
d9 <- felm(Sal ~ RBI * RBI_100 + FLD + BsR + WPA + nWPA + FA | Yr)


d5 <- lm(Sal ~ RBI * RBI_100 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal ~ RBI * RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal ~ RBI * RBI_100 + FLD + BsR + WPA + nWPA | ID)
d8 <- felm(Sal ~ RBI * RBI_100 + FLD + BsR + WPA + nWPA | Yr)

summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)
summary(d9)

d1 <- lm(Sal ~ BAT * RBI_100)
d2 <- lm(Sal ~ BAT * RBI_100 + FLD + BsR)
d3 <- lm(Sal ~ BAT * RBI_100 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal ~ BAT * RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal ~ BAT * RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal ~ BAT * RBI_100 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal ~ BAT * RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal ~ BAT * RBI_100 + FLD + BsR + WPA + nWPA + FA | ID)
d9 <- felm(Sal ~ BAT * RBI_100 + FLD + BsR + WPA + nWPA + FA | Yr)

d5 <- lm(Sal ~ BAT * RBI_100 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal ~ BAT * RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal ~ BAT * RBI_100 + FLD + BsR + WPA + nWPA | ID)
d8 <- felm(Sal ~ BAT * RBI_100 + FLD + BsR + WPA + nWPA | Yr)

summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)
summary(d9)

d1 <- lm(Sal_dev ~ BAT * RBI_100)
d2 <- lm(Sal_dev ~ BAT * RBI_100 + FLD + BsR)
d3 <- lm(Sal_dev ~ BAT * RBI_100 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal_dev ~ BAT * RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

d5 <- lm(Sal_dev ~ BAT * RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- lm(Sal_dev ~ BAT * RBI_100 + FLD + BsR + AGE + AGE_sq + Clutch + FA)
d7 <- felm(Sal_dev ~ BAT * RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d8 <- felm(Sal_dev ~ BAT * RBI_100 + FLD + BsR + WPA + nWPA + FA | ID)

d5 <- lm(Sal_dev ~ BAT * RBI_100 + FLD + BsR + AGE + AGE_sq + Clutch)
d6 <- felm(Sal_dev ~ BAT * RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA | POS)
d7 <- felm(Sal_dev ~ BAT * RBI_100 + FLD + BsR + WPA + nWPA | ID)


summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)

#DIDID
e1 <- lm(Sal ~ RBI * RBI_100 * ERA)
e2 <- lm(Sal ~ RBI * RBI_100 * ERA + FLD + BsR)
e3 <- lm(Sal ~ RBI * RBI_100 * ERA + FLD + BsR + AGE + AGE_sq)
e4 <- lm(Sal ~ RBI * RBI_100 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

e5 <- lm(Sal ~ RBI * RBI_100 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- lm(Sal ~ RBI * RBI_100 * ERA + FLD + BsR + AGE + AGE_sq + Clutch + FA)
e7 <- felm(Sal ~ RBI * RBI_100 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
e8 <- felm(Sal ~ RBI * RBI_100 * ERA + FLD + BsR + WPA + nWPA + FA | ID)

e5 <- lm(Sal ~ RBI * RBI_100 * ERA + FLD + BsR + AGE + AGE_sq + Clutch)
e6 <- felm(Sal ~ RBI * RBI_100 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA| POS)
e7 <- felm(Sal ~ RBI * RBI_100 * ERA + FLD + BsR + WPA + nWPA | ID)

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)

e1 <- lm(Sal_dev ~ RBI * RBI_100 * ERA)
e2 <- lm(Sal_dev ~ RBI * RBI_100 * ERA + FLD + BsR)
e3 <- lm(Sal_dev ~ RBI * RBI_100 * ERA + FLD + BsR + AGE + AGE_sq)
e4 <- lm(Sal_dev ~ RBI * RBI_100 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA)

e5 <- lm(Sal_dev ~ RBI * RBI_100 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- lm(Sal_dev ~ RBI * RBI_100 * ERA + FLD + BsR + AGE + AGE_sq + Clutch + FA)
e7 <- felm(Sal_dev ~ RBI * RBI_100 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
e8 <- felm(Sal_dev ~ RBI * RBI_100 * ERA + FLD + BsR + WPA + nWPA + FA | ID)

e5 <- lm(Sal_dev ~ RBI * RBI_100 * ERA + FLD + BsR + AGE + AGE_sq + Clutch)
e6 <- felm(Sal_dev ~ RBI * RBI_100 * ERA + FLD + BsR + AGE + AGE_sq + WPA + nWPA| POS)
e7 <- felm(Sal_dev ~ RBI * RBI_100 * ERA + FLD + BsR + WPA + nWPA | ID)

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)