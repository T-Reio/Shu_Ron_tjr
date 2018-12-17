library(data.table)
library(lfe)
library(rdd)
library(rddensity)
library(stargazer)

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised_B.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, AVG < 0.346 & AVG >= 0.255)
Sal <- stats$`Log AVG ANNUAL`
AVG <- stats$AVG
AGE <- stats$Age
AGE_sq <- AGE^2
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
Yr <- as.factor(stats$Season)
AVG_300 <- stats$AVG_300
FA <- stats$FA

d1 <- lm(Sal ~ AVG * AVG_300)
d3 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr + POS)
d5 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr + POS)
d6 <- felm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr | TEAM)
d7 <- felm(Sal ~ AVG * AVG_300 + FLD + BsR + WPA + nWPA + FA + Yr| ID)

#stargazer(d1,d3,d4,d5,d6,d7,
          out = 'C:/Users/easyu/Master_thesis/master_thesis/results/Sal_AVG300_E.tex',
          title = 'Regression on Log-Salary, Including Interaction Term: around .300',
          align = F, initial.zero = F,
          intercept.bottom = FALSE, font.size = "tiny", label = "AVG300_A",
          table.placement = "H", star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c("WPA", "nWPA", "FA", "Yr", "POS", "AGE", "AGE_sq"),
          add.lines = list(c("Season dummies","","X","X","X","X","X"),
                           c("WPA", "","X","X","X","X"),
                           c("AGE (quadratic)", "", "X", "X", "X", "X", ""),
                           c("FA dummy","","", "","X","X","X"),
                           c("Position dummies", "", "", "X", "X", "",""),
                           c("Fixed effects","","","","",
                             "Team", "Individual")
          )
)#

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised_B.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, AVG < 0.299 & AVG >= 0.202)
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

d1 <- lm(Sal ~ AVG * AVG_250)
d3 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr + POS)
d5 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr + POS)
d6 <- felm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr | TEAM)
d7 <- felm(Sal ~ AVG * AVG_250 + FLD + BsR + WPA + nWPA + FA + Yr| ID)

#stargazer(d1,d3,d4,d5,d6,d7,
          out = 'C:/Users/easyu/Master_thesis/master_thesis/results/Sal_AVG250_E.tex',
          title = 'Regression on Log-Salary, Including Interaction Term: around .250',
          align = F, initial.zero = F,
          intercept.bottom = FALSE, font.size = "tiny", label = "AVG250_A",
          table.placement = "H", star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c("WPA", "nWPA", "FA", "Yr", "POS", "AGE", "AGE_sq"),
          add.lines = list(c("Season dummies","","X","X","X","X","X"),
                           c("WPA", "","X","X","X","X"),
                           c("AGE (quadratic)", "", "X", "X", "X", "X", ""),
                           c("FA dummy","","", "","X","X","X"),
                           c("Position dummies", "", "", "X", "X", "",""),
                           c("Fixed effects","","","","",
                             "Team", "Individual")
          )
)

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
d3 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr + POS)
d5 <- lm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr + POS)
d6 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr | TEAM)
d7 <- felm(Sal ~ OBP * OBP_350 + FLD + BsR + WPA + nWPA + FA + Yr| ID)

#stargazer(d1,d3,d4,d5,d6,d7,
          out = 'C:/Users/easyu/Master_thesis/master_thesis/results/Sal_OBP350_E.tex',
          title = 'Regression on Log-Salary, Including Interaction Term: around .350',
          align = F, initial.zero = F,
          intercept.bottom = FALSE, font.size = "tiny", label = "OBP350_A",
          table.placement = "H", star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c("WPA", "nWPA", "FA", "Yr", "POS", "AGE", "AGE_sq"),
          add.lines = list(c("Season dummies","","X","X","X","X","X"),
                           c("WPA", "","X","X","X","X"),
                           c("AGE (quadratic)", "", "X", "X", "X", "X", ""),
                           c("FA dummy","","", "","X","X","X"),
                           c("Position dummies", "", "", "X", "X", "",""),
                           c("Fixed effects","","","","",
                             "Team", "Individual")
          )
)

library(data.table)
library(lfe)
library(rdd)
library(rddensity)
library(stargazer)

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised_B.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, HR < 24 & HR >= 17)
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

d1 <- lm(Sal ~ HR * HR_20)
d3 <- lm(Sal ~ HR * HR_20 + FLD + BsR + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ HR * HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr + POS)
d5 <- lm(Sal ~ HR * HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr + POS)
d6 <- felm(Sal ~ HR * HR_20 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr | TEAM)
d7 <- felm(Sal ~ HR * HR_20 + FLD + BsR + WPA + nWPA + FA + Yr| ID)

#stargazer(d1,d3,d4,d5,d6,d7,
          out = 'C:/Users/easyu/Master_thesis/master_thesis/results/Sal_HR20_E.tex',
          title = 'Regression on Log-Salary, Including Interaction Term: around 20 HR',
          align = F, initial.zero = F,
          intercept.bottom = FALSE, font.size = "tiny", label = "HR20_A",
          table.placement = "H", star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c("WPA", "nWPA", "FA", "Yr", "POS", "AGE", "AGE_sq"),
          add.lines = list(c("Season dummies","","X","X","X","X","X"),
                           c("WPA", "","X","X","X","X"),
                           c("AGE (quadratic)", "", "X", "X", "X", "X", ""),
                           c("FA dummy","","", "","X","X","X"),
                           c("Position dummies", "", "", "X", "X", "",""),
                           c("Fixed effects","","","","",
                             "Team", "Individual")
          )
)

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised_B.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, RBI < 105 & RBI >= 96)
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

d1 <- lm(Sal ~ RBI * RBI_100)
d3 <- lm(Sal ~ RBI * RBI_100 + FLD + BsR + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ RBI * RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr + POS)
d5 <- lm(Sal ~ RBI * RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr + POS)
d6 <- felm(Sal ~ RBI * RBI_100 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr | TEAM)
d7 <- felm(Sal ~ RBI * RBI_100 + FLD + BsR + WPA + nWPA + FA + Yr| ID)

#stargazer(d1,d3,d4,d5,d6,d7,
          out = 'C:/Users/easyu/Master_thesis/master_thesis/results/Sal_RBI100_E.tex',
          title = 'Regression on Log-Salary, Including Interaction Term: around 100 RBI',
          align = F, initial.zero = F,
          intercept.bottom = FALSE, font.size = "tiny", label = "RBI100_A",
          table.placement = "H", star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c("WPA", "nWPA", "FA", "Yr", "POS", "AGE", "AGE_sq"),
          add.lines = list(c("Season dummies","","X","X","X","X","X"),
                           c("WPA", "","X","X","X","X"),
                           c("AGE (quadratic)", "", "X", "X", "X", "X", ""),
                           c("FA dummy","","", "","X","X","X"),
                           c("Position dummies", "", "", "X", "X", "",""),
                           c("Fixed effects","","","","",
                             "Team", "Individual")
          )
)

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised_B.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, SB < 34 & SB >= 27)
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

d1 <- lm(Sal ~ SB * SB_30)
d3 <- lm(Sal ~ SB * SB_30 + FLD + BAT + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ SB * SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + Yr + POS)
d5 <- lm(Sal ~ SB * SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA + Yr + POS)
d6 <- felm(Sal ~ SB * SB_30 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA + Yr | TEAM)
d7 <- felm(Sal ~ SB * SB_30 + FLD + BAT + WPA + nWPA + FA + Yr| ID)

#stargazer(d1,d3,d4,d5,d6,d7,
          out = 'C:/Users/easyu/Master_thesis/master_thesis/results/Sal_SB30_E.tex',
          title = 'Regression on Log-Salary, Including Interaction Term: around 30 SB',
          align = F, initial.zero = F,
          intercept.bottom = FALSE, font.size = "tiny", label = "SB30_A",
          table.placement = "H", star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c("WPA", "nWPA", "FA", "Yr", "POS", "AGE", "AGE_sq"),
          add.lines = list(c("Season dummies","","X","X","X","X","X"),
                           c("WPA", "","X","X","X","X"),
                           c("AGE (quadratic)", "", "X", "X", "X", "X", ""),
                           c("FA dummy","","", "","X","X","X"),
                           c("Position dummies", "", "", "X", "X", "",""),
                           c("Fixed effects","","","","",
                             "Team", "Individual")
          )
)

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised_B.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, SB < 44 & SB >= 37)
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

d1 <- lm(Sal ~ SB * SB_40)
d3 <- lm(Sal ~ SB * SB_40 + FLD + BAT + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ SB * SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + Yr + POS)
d5 <- lm(Sal ~ SB * SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA + Yr + POS)
d6 <- felm(Sal ~ SB * SB_40 + FLD + BAT + AGE + AGE_sq + WPA + nWPA + FA + Yr | TEAM)
d7 <- felm(Sal ~ SB * SB_40 + FLD + BAT + WPA + nWPA + FA + Yr| ID)

#stargazer(d1,d3,d4,d5,d6,d7,
          out = 'C:/Users/easyu/Master_thesis/master_thesis/results/Sal_SB40_E.tex',
          title = 'Regression on Log-Salary, Including Interaction Term: around 40 SB',
          align = F, initial.zero = F,
          intercept.bottom = FALSE, font.size = "tiny", label = "SB40_A",
          table.placement = "H", star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c("WPA", "nWPA", "FA", "Yr", "POS", "AGE", "AGE_sq"),
          add.lines = list(c("Season dummies","","X","X","X","X","X"),
                           c("WPA", "","X","X","X","X"),
                           c("AGE (quadratic)", "", "X", "X", "X", "X", ""),
                           c("FA dummy","","", "","X","X","X"),
                           c("Position dummies", "", "", "X", "X", "",""),
                           c("Fixed effects","","","","",
                             "Team", "Individual")
          )
)

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised_B.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, H < 204 & H >= 197)
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


d1 <- lm(Sal ~ H * H_200)
d3 <- lm(Sal ~ H * H_200 + FLD + BsR + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ H * H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr + POS)
d5 <- lm(Sal ~ H * H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr + POS)
d6 <- felm(Sal ~ H * H_200 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr | TEAM)
d7 <- felm(Sal ~ H * H_200 + FLD + BsR + WPA + nWPA + FA + Yr| ID)

#stargazer(d1,d3,d4,d5,d6,
          out = 'C:/Users/easyu/Master_thesis/master_thesis/results/Sal_H200_E.tex',
          title = 'Regression on Log-Salary, Including Interaction Term: 
          around 200 Base-Hit',
          align = F, initial.zero = F,
          intercept.bottom = FALSE, font.size = "tiny", label = "H200_A",
          table.placement = "H", star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c("WPA", "nWPA", "FA", "Yr", "POS", "AGE", "AGE_sq"),
          add.lines = list(c("Season dummies","","X","X","X","X"),
                           c("WPA", "","X","X","X"),
                           c("AGE (quadratic)", "", "X", "X", "X", "X"),
                           c("FA dummy","","", "","X","X"),
                           c("Position dummies", "", "", "X", "X", ""),
                           c("Fixed effects","","","","",
                             "Team")
          )
)

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised_B.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, stats$Season <= 1994)
stats <- subset(stats, AVG < 0.325 & AVG >= 0.276)
Sal <- stats$`Log AVG ANNUAL`
AVG <- stats$AVG
AGE <- stats$Age
AGE_sq <- AGE^2
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
Yr <- as.factor(stats$Season)
AVG_300 <- stats$AVG_300
FA <- stats$FA

d1 <- lm(Sal ~ AVG * AVG_300)
d3 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr + POS)
d5 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr + POS)
d6 <- felm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr | TEAM)
d7 <- felm(Sal ~ AVG * AVG_300 + FLD + BsR + WPA + nWPA + FA + Yr| ID)

stargazer(d1,d3,d4,d5,d6,d7,
          out = 'C:/Users/easyu/Master_thesis/master_thesis/results/Sal_AVG300_F.tex',
          title = 'Regression on Log-Salary, Including Interaction Term: around .300',
          align = F, initial.zero = F,
          intercept.bottom = FALSE, font.size = "tiny", label = "AVG300_B",
          table.placement = "H", star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c("WPA", "nWPA", "FA", "Yr", "POS", "AGE", "AGE_sq"),
          add.lines = list(c("Season dummies","","X","X","X","X","X"),
                           c("WPA", "","X","X","X","X"),
                           c("AGE (quadratic)", "", "X", "X", "X", "X", ""),
                           c("FA dummy","","", "","X","X","X"),
                           c("Position dummies", "", "", "X", "X", "",""),
                           c("Fixed effects","","","","",
                             "Team", "Individual")
          )
)

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised_B.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, stats$Season >= 1995 & stats$Season <= 2003)
stats <- subset(stats, AVG < 0.341 & AVG >= 0.256)
Sal <- stats$`Log AVG ANNUAL`
AVG <- stats$AVG
AGE <- stats$Age
AGE_sq <- AGE^2
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
Yr <- as.factor(stats$Season)
AVG_300 <- stats$AVG_300
FA <- stats$FA

d1 <- lm(Sal ~ AVG * AVG_300)
d3 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr + POS)
d5 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr + POS)
d6 <- felm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr | TEAM)
d7 <- felm(Sal ~ AVG * AVG_300 + FLD + BsR + WPA + nWPA + FA + Yr| ID)

stargazer(d1,d3,d4,d5,d6,d7,
out = 'C:/Users/easyu/Master_thesis/master_thesis/results/Sal_AVG300_G.tex',
title = 'Regression on Log-Salary, Including Interaction Term: around .300',
align = F, initial.zero = F,
intercept.bottom = FALSE, font.size = "tiny", label = "AVG300_C",
table.placement = "H", star.cutoffs = c(0.05, 0.01, 0.001),
omit = c("WPA", "nWPA", "FA", "Yr", "POS", "AGE", "AGE_sq"),
add.lines = list(c("Season dummies","","X","X","X","X","X"),
                 c("WPA", "","X","X","X","X"),
                 c("AGE (quadratic)", "", "X", "X", "X", "X", ""),
                 c("FA dummy","","", "","X","X","X"),
                 c("Position dummies", "", "", "X", "X", "",""),
                 c("Fixed effects","","","","",
                   "Team", "Individual")
)
)

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised_B.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, stats$Season >= 2004)
stats <- subset(stats, AVG < 0.331 & AVG >= 0.27)
Sal <- stats$`Log AVG ANNUAL`
AVG <- stats$AVG
AGE <- stats$Age
AGE_sq <- AGE^2
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
Yr <- as.factor(stats$Season)
AVG_300 <- stats$AVG_300
FA <- stats$FA

d1 <- lm(Sal ~ AVG * AVG_300)
d3 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr + POS)
d5 <- lm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr + POS)
d6 <- felm(Sal ~ AVG * AVG_300 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr | TEAM)
d7 <- felm(Sal ~ AVG * AVG_300 + FLD + BsR + WPA + nWPA + FA + Yr| ID)

stargazer(d1,d3,d4,d5,d6,d7,
out = 'C:/Users/easyu/Master_thesis/master_thesis/results/Sal_AVG300_H.tex',
title = 'Regression on Log-Salary, Including Interaction Term: around .300',
align = F, initial.zero = F,
intercept.bottom = FALSE, font.size = "tiny", label = "AVG300_D",
table.placement = "H", star.cutoffs = c(0.05, 0.01, 0.001),
omit = c("WPA", "nWPA", "FA", "Yr", "POS", "AGE", "AGE_sq"),
add.lines = list(c("Season dummies","","X","X","X","X","X"),
                 c("WPA", "","X","X","X","X"),
                 c("AGE (quadratic)", "", "X", "X", "X", "X", ""),
                 c("FA dummy","","", "","X","X","X"),
                 c("Position dummies", "", "", "X", "X", "",""),
                 c("Fixed effects","","","","",
                   "Team", "Individual")
)
)