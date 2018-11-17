library(data.table)
library(stargazer)
library(lfe)
library(rddensity)
library(Hmisc)
library(plm)
library(xtable)

install.packages("rdd", dependencies = T)
install.packages("rddensity", dependencies = T)

help(RDestimate, package = "rdd")
help(package = "rddensity")
help(ggplot, package = "ggplot2")
help("shapiro.test")
help("hist")
help(package = "rdd")
help(DCdensity)
help(latex)
help("stargazer")

rdd <- rddensity(AVG, c = 0.300)
summary(rdd)
rdd$hat
rdd$test

rdd.250 <- rddensity(AVG, c = 0.250)
rdd.250$test

latex(m1$coefficients, m1$df.residual, file='', booktabs=T, dcolumn=T)
m1$effects
m1$rank
m1$df.residual
m1$assign
m1$terms
r1$bw
latex(r1$est, file = '', booktabs = T, dcolumn = T)

xtable(m1)
latex(list(r1$est, r1$bw), file = '', booktabs = T, dcolumn = T)
r1$ci

stargazer(list(r1$est, r1$se), out = '')
r1

summary(r1)

#packages required
install.packages("data.table", dependencies = T)
install.packages("stargazer", dependencies = T)
install.packages("lfe", dependencies = T)
install.packages("rddensity", dependencies = T)
install.packages("rdd", dependencies = T)
install.packages("plm", dependencies = T)
install.packages("Hmisc", dependencies = T)
install.packages("", dependencies = T)
install.packages("", dependencies = T)
install.packages("", dependencies = T)

library(data.table)
library(lfe)
library(rdd)
library(rddensity)
library(stargazer)

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised_B.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, OBP < 0.395 & OBP >= 0.305)
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
AVG_250 <- stats$AVG_250
OBP_350 <- stats$OBP_350
HR_20 <- stats$HR_20
RBI_100 <- stats$RBI_100
SB_30 <- stats$SB_30
SB_40 <- stats$SB_40
OPS_1000 <- stats$OPS_1000
PA_500 <- stats$PA_500
H_200 <- stats$H_200
FA <- stats$FA

d1 <- lm(Sal_dev ~ OBP * OBP_350)
d2 <- lm(Sal_dev ~ OBP * OBP_350 + FLD + BsR)
d3 <- lm(Sal_dev ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq)
d4 <- lm(Sal_dev ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)
d5 <- lm(Sal_dev ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
d6 <- felm(Sal_dev ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
d7 <- felm(Sal_dev ~ OBP * OBP_350 + FLD + BsR + WPA + nWPA + FA | ID)
d8 <- felm(Sal_dev ~ OBP * OBP_350 + FLD + BsR + WPA + nWPA + FA | POS)


e1 <- lm(Sal_dev ~ BAT * OBP_350)
e2 <- lm(Sal_dev ~ BAT * OBP_350 + FLD + BsR )
e3 <- lm(Sal_dev ~ BAT * OBP_350 + FLD + BsR + AGE + AGE_sq )
e4 <- lm(Sal_dev ~ BAT * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA)
e5 <- lm(Sal_dev ~ BAT * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA)
e6 <- felm(Sal_dev ~ BAT * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA | POS)
e7 <- felm(Sal_dev ~ BAT * OBP_350 + FLD + BsR + WPA + nWPA + FA | ID)
e8 <- felm(Sal_dev ~ BAT * OBP_350 + FLD + BsR + WPA + nWPA + FA | POS )

summary(e1)
summary(e2)
summary(e3)
summary(e4)
summary(e5)
summary(e6)
summary(e7)
summary(e8)
