library(data.table)
library(lfe)
library(rdd)
library(rddensity)

stats <- fread("C:/Users/T-Reio/Master_thesis/master_thesis/output/stats_sal_fa_revised.csv",
               header = T, sep = ",")

#Common Sort
stats <- subset(stats, stats$POS == "P")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, stats$PA >= 90)
stats <- subset(stats, stats$SB >= 5)

#Sort with Eras
stats <- subset(stats, stats$Season <= 1975)
stats <- subset(stats, stats$Season >= 1976 & stats$Season <= 1994)
stats <- subset(stats, stats$Season >= 1995 & stats$Season <= 2003)
stats <- subset(stats, stats$Season >= 2004)

#Sort with charactaristics
stats <- subset(stats, stats$FA == 1)

#For local linear analysis
stats <- subset(stats, AVG < 0.320 & AVG >= 0.280)
stats <- subset(stats, AVG < 0.310 & AVG >= 0.290)

stats <- subset(stats, AVG < 0.3 & AVG >= 0.280)
stats <- subset(stats, AVG < 0.320 & AVG >= 300)

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
WPA <- stats$WPA/G
nWPA <- -stats$`-WPA`/PA
Clutch <- stats$Clutch
ID <- stats$playerid
BAT <- stats$Bat
FLD <- stats$Fld
BsR <- stats$BsR
POS <- stats$POS
TEAM <- stats$TEAM_nextyr


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

MLS <- stats$Yrs
rWARp <- stats$WAR3