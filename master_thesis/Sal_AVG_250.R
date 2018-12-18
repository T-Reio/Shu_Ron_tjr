library(data.table)
library(lfe)
library(rdd)
library(rddensity)
library(stargazer)

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
d2 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + Yr)
d3 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr)
d5 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr)
d6 <- felm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr | POS)
d7 <- felm(Sal ~ AVG * AVG_250 + FLD + BsR + WPA + nWPA + FA + Yr | ID)
d8 <- felm(Sal ~ AVG * AVG_250 + FLD + BsR + WPA + nWPA + FA + Yr | POS)

stargazer(d1,d2,d3,d4,d5,d6,d7,d8,
          out = 'C:/Users/easyu/Master_thesis/master_thesis/results/Sal_AVG250_A.tex',
          title = 'Regression on Log-Salary, Including Interaction Term: around .250',
          align = F, initial.zero = F,
          intercept.bottom = FALSE, font.size = "tiny", label = "AVG250_A",
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
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, stats$Season <= 1994)
stats <- subset(stats, AVG < 0.287 & AVG >= 0.214)
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
d2 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + Yr)
d3 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr)
d5 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr)
d6 <- felm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr | POS)
d7 <- felm(Sal ~ AVG * AVG_250 + FLD + BsR + WPA + nWPA + FA + Yr | ID)
d8 <- felm(Sal ~ AVG * AVG_250 + FLD + BsR + WPA + nWPA + FA + Yr | POS)

stargazer(d1,d2,d3,d4,d5,d6,d7,d8,
          out = 'C:/Users/easyu/Master_thesis/master_thesis/results/Sal_AVG250_B.tex',
          title = 'Regression on Log-Salary, Including Interaction Term: around .250',
          align = F, initial.zero = F,
          intercept.bottom = FALSE, font.size = "tiny", label = "AVG250_B",
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
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, AVG < 0.294 & AVG >= 0.207)
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
d2 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + Yr)
d3 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr)
d5 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr)
d6 <- felm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr | POS)
d7 <- felm(Sal ~ AVG * AVG_250 + FLD + BsR + WPA + nWPA + FA + Yr | ID)
d8 <- felm(Sal ~ AVG * AVG_250 + FLD + BsR + WPA + nWPA + FA + Yr | POS)

stargazer(d1,d2,d3,d4,d5,d6,d7,d8,
          out = 'C:/Users/easyu/Master_thesis/master_thesis/results/Sal_AVG250_C.tex',
          title = 'Regression on Log-Salary, Including Interaction Term: around .250',
          align = F, initial.zero = F,
          intercept.bottom = FALSE, font.size = "tiny", label = "AVG250_C",
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
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, stats$Season >= 2004)
stats <- subset(stats, AVG < 0.326 & AVG >= 0.175)
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
d2 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + Yr)
d3 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr)
d5 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr)
d6 <- felm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr | POS)
d7 <- felm(Sal ~ AVG * AVG_250 + FLD + BsR + WPA + nWPA + FA + Yr | ID)
d8 <- felm(Sal ~ AVG * AVG_250 + FLD + BsR + WPA + nWPA + FA + Yr | POS)

stargazer(d1,d2,d3,d4,d5,d6,d7,d8,
          out = 'C:/Users/easyu/Master_thesis/master_thesis/results/Sal_AVG250_D.tex',
          title = 'Regression on Log-Salary, Including Interaction Term: around .250',
          align = F, initial.zero = F,
          intercept.bottom = FALSE, font.size = "tiny", label = "AVG250_D",
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
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, stats$Season >= 2004)
stats <- subset(stats, AVG < 0.290 & AVG >= 0.210)
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
d2 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + Yr)
d3 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + Yr)
d4 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + Yr)
d5 <- lm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr)
d6 <- felm(Sal ~ AVG * AVG_250 + FLD + BsR + AGE + AGE_sq + WPA + nWPA + FA + Yr | POS)
d7 <- felm(Sal ~ AVG * AVG_250 + FLD + BsR + WPA + nWPA + FA + Yr | ID)
d8 <- felm(Sal ~ AVG * AVG_250 + FLD + BsR + WPA + nWPA + FA + Yr | POS)

summary(d1)
summary(d6)
summary(d8)


stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised_B.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, AVG < 0.299 & AVG >= 0.202)
stats <- subset(stats, AVG <= 0.245 | AVG > 0.255)
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

stargazer(d1,d3,d4,d5,d6,d7,
          type = 'text',
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