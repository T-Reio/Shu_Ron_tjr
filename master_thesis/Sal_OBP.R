library(data.table)
library(lfe)
library(rdd)
library(rddensity)
library(stargazer)

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised_B.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, OBP < 0.396 & OBP >= 0.305)
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

d1 <- lm(Sal ~ OBP * OBP_350)
d2 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + Yr)
d3 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr)
d5 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr)
d6 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr | POS)
d7 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + WPA + nWPA + FA + Yr | ID)
d8 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + WPA + nWPA + FA + Yr | POS)

stargazer(d1,d2,d3,d4,d5,d6,d7,d8,
          out = 'C:/Users/easyu/Master_thesis/master_thesis/results/Sal_OBP350_A.tex',
          title = 'Regression on Log-Salary, Including Interaction Term: around .350',
          align = F, initial.zero = F,
          intercept.bottom = FALSE, font.size = "tiny", label = "OBP350_A",
          table.placement = "H", star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c("WPA", "nWPA", "FA", "Yr"),
          add.lines = list(c("WPA", "","","","X","X","X","X","X"),
                           c("FA dummy","","","","","X","X","X","X"),
                           c("Season dummies","","X","X","X","X","X","X","X"),
                           c("Fixed effects","","","","","",
                             "Position", "Individual","Position")
          )
)


stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised_B.csv",
                header = T, sep = ",")
stats <- subset(stats, stats$Season <= 1994)
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, OBP < 0.382 & OBP >= 0.319)
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

d1 <- lm(Sal ~ OBP * OBP_350)
d2 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + Yr)
d3 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr)
d5 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr)
d6 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr | POS)
d7 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + WPA + nWPA + FA + Yr | ID)
d8 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + WPA + nWPA + FA + Yr | POS)

stargazer(d1,d2,d3,d4,d5,d6,d7,d8,
          out = 'C:/Users/easyu/Master_thesis/master_thesis/results/Sal_OBP350_B.tex',
          title = 'Regression on Log-Salary, Including Interaction Term: around .350',
          align = F, initial.zero = F,
          intercept.bottom = FALSE, font.size = "tiny", label = "OBP350_B",
          table.placement = "H", star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c("WPA", "nWPA", "FA", "Yr"),
          add.lines = list(c("WPA", "","","","X","X","X","X","X"),
                           c("FA dummy","","","","","X","X","X","X"),
                           c("Season dummies","","X","X","X","X","X","X","X"),
                           c("Fixed effects","","","","","",
                             "Position", "Individual","Position")
          )
)

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised_B.csv",
                header = T, sep = ",")
stats <- subset(stats, stats$Season >= 1995 & stats$Season <= 2003)
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, OBP < 0.376 & OBP >= 0.325)
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

d1 <- lm(Sal ~ OBP * OBP_350)
d2 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + Yr)
d3 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr)
d5 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr)
d6 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr | POS)
d7 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + WPA + nWPA + FA + Yr | ID)
d8 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + WPA + nWPA + FA + Yr | POS)

stargazer(d1,d2,d3,d4,d5,d6,d7,d8,
          out = 'C:/Users/easyu/Master_thesis/master_thesis/results/Sal_OBP350_C.tex',
          title = 'Regression on Log-Salary, Including Interaction Term: around .350',
          align = F, initial.zero = F,
          intercept.bottom = FALSE, font.size = "tiny", label = "OBP350_C",
          table.placement = "H", star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c("WPA", "nWPA", "FA", "Yr"),
          add.lines = list(c("WPA", "","","","X","X","X","X","X"),
                           c("FA dummy","","","","","X","X","X","X"),
                           c("Season dummies","","X","X","X","X","X","X","X"),
                           c("Fixed effects","","","","","",
                             "Position", "Individual","Position")
          )
)

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised_B.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$Season >= 2004)
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, OBP < 0.378 & OBP >= 0.323)
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

d1 <- lm(Sal ~ OBP * OBP_350)
d2 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + Yr)
d3 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr)
d5 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr)
d6 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr | POS)
d7 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + WPA + nWPA + FA + Yr | ID)
d8 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + WPA + nWPA + FA + Yr | POS)
stargazer(d1,d2,d3,d4,d5,d6,d7,d8,
          out = 'C:/Users/easyu/Master_thesis/master_thesis/results/Sal_OBP350_D.tex',
          title = 'Regression on Log-Salary, Including Interaction Term: around .350',
          align = F, initial.zero = F,
          intercept.bottom = FALSE, font.size = "tiny", label = "OBP350_D",
          table.placement = "H", star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c("WPA", "nWPA", "FA", "Yr"),
          add.lines = list(c("WPA", "","","","X","X","X","X","X"),
                           c("FA dummy","","","","","X","X","X","X"),
                           c("Season dummies","","X","X","X","X","X","X","X"),
                           c("Fixed effects","","","","","",
                             "Position", "Individual","Position")
          )
)