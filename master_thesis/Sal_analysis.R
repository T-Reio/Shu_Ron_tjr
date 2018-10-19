library(data.table)
library(stargazer)
library(lfe)

stats_87 <- fread(
  "C:/Users/T-Reio/Master_thesis/master_thesis/output/stats_salary_87to17.csv", 
  header = T, sep = ",")

stats_87 <- subset(stats_87, stats_87$PA >= 200)



#Full Sample

Sal_AVG <- stats_87$`AVG ANNUAL`
AGE <- stats_87$Age
AGE_sq <- AGE^2
fWAR <- stats_87$WAR
ABOVE_300 <- stats_87$AVG_above
TEAM <- stats_87$TEAM_nextyr
POS <- stats_87$POS
L_Sal <- stats_87$`Log AVG ANNUAL`
individual <- stats_87$playerid
BATTING <- stats_87$Bat
FIELDING <- stats_87$Fld
BaseRun <- stats_87$BsR
AVG_87 <- stats_87$AVG
Season <- stats_87$Season
OBP_87 <- stats_87$OBP
SLG_87 <- stats_87$SLG
OPS_87 <- stats_87$OPS
wRC <- stats_87$`wRC+`
wOBA <- stats_87$wOBA
DEFFENCE <- stats_87$Def
WPA <- stats_87$WPA
nWPA <- -stats_87$`-WPA`
WPA <- WPA/stats_87$G
nWPA <- nWPA/stats_87$G

#指標間の比較

cor(AVG_87, fWAR)

MI1 <- lm(L_Sal ~ AVG_87 * ABOVE_300 + FIELDING + BaseRun)
MI2 <- lm(L_Sal ~ OBP_87 * ABOVE_300 + FIELDING + BaseRun)
MI3 <- lm(L_Sal ~ SLG_87 * ABOVE_300 + FIELDING + BaseRun)
MI4 <- lm(L_Sal ~ OPS_87 * ABOVE_300+ FIELDING + BaseRun)
MI5 <- lm(L_Sal ~ wOBA * ABOVE_300 + FIELDING + BaseRun)
MI6 <- lm(L_Sal ~ wRC * ABOVE_300 + FIELDING + BaseRun)
MI7 <- lm(L_Sal ~ BATTING *ABOVE_300 + FIELDING + BaseRun)
MI8 <- lm(L_Sal ~ fWAR * ABOVE_300)

summary(MI1)
summary(MI2)
summary(MI3)
summary(MI4)
summary(MI5)
summary(MI6)
summary(MI7)
summary(MI8)



M1 <- lm(L_Sal ~ fWAR * ABOVE_300)
M2 <- lm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun)
M3 <- lm(L_Sal ~ fWAR * ABOVE_300 + AGE + AGE_sq)
M4 <- lm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq)
M5 <- felm(L_Sal ~ fWAR * ABOVE_300 + AGE + AGE_sq | TEAM)
M6 <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq | POS)
M7 <- felm(L_Sal ~ fWAR * ABOVE_300 | individual)
M8 <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq | POS + TEAM )
M9 <- felm(L_Sal ~ fWAR * ABOVE_300 + WPA + nWPA | individual + TEAM)
M10 <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + WPA + nWPA |
             individual + TEAM + POS)
M11 <- felm(L_Sal ~ fWAR * ABOVE_300 + AGE + AGE_sq + WPA + nWPA| TEAM)
M12 <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq + WPA + nWPA|
              POS + TEAM)

summary(M0)
summary(M1)
summary(M2)
summary(M3)
summary(M4)
summary(M5)
summary(M6)
summary(M7)
summary(M8)
summary(M9)
summary(M10)
summary(M11)
summary(M12)

#Before Strike

stats_bfst <- subset(stats_87, stats_87$Season <= 1994)

Sal_AVG <- stats_bfst$`AVG ANNUAL`
AGE <- stats_bfst$Age
AGE_sq <- AGE^2
fWAR <- stats_bfst$WAR
ABOVE_300 <- stats_bfst$AVG_above
TEAM <- stats_bfst$TEAM_nextyr
POS <- stats_bfst$POS
L_Sal <- stats_bfst$`Log AVG ANNUAL`
individual <- stats_bfst$playerid
BATTING <- stats_bfst$Bat
FIELDING <- stats_bfst$Fld
BaseRun <- stats_bfst$BsR
AVG <- stats_bfst$AVG
Season <- stats_bfst$Season
DEFFENCE <- stats_bfst$Def
WPA <- stats_bfst$WPA
nWPA <- -stats_bfst$`-WPA`
WPA <- WPA/stats_bfst$G
nWPA <- nWPA/stats_bfst$G

M1_bfst <- lm(L_Sal ~ fWAR * ABOVE_300)
M2_bfst <- lm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun)
M3_bfst <- lm(L_Sal ~ fWAR * ABOVE_300 + AGE + AGE_sq)
M4_bfst <- lm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq)
M5_bfst <- felm(L_Sal ~ fWAR * ABOVE_300 + AGE + AGE_sq | TEAM)
M6_bfst <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq | POS)
M7_bfst <- felm(L_Sal ~ fWAR * ABOVE_300 | individual)
M8_bfst <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq | POS + TEAM )
M9_bfst <- felm(L_Sal ~ fWAR * ABOVE_300 + WPA + nWPA | individual + TEAM)
M10_bfst <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + WPA + nWPA |
              individual + TEAM + POS)
M11_bfst <- felm(L_Sal ~ fWAR * ABOVE_300 + AGE + AGE_sq + WPA + nWPA| TEAM)
M12_bfst <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq + WPA + nWPA|
              POS + TEAM)

summary(M0_bfst)
summary(M1_bfst)
summary(M2_bfst)
summary(M3_bfst)
summary(M4_bfst)
summary(M5_bfst)
summary(M6_bfst)
summary(M7_bfst)
summary(M8_bfst)
summary(M9_bfst)
summary(M10_bfst)
summary(M11_bfst)
summary(M12_bfst)

#After Strike - Before Moneyball

stats_stmb <- subset(stats_87, stats_87$Season >= 1995 & stats_87$Season <= 2001)

Sal_AVG <- stats_stmb$`AVG ANNUAL`
AGE <- stats_stmb$Age
AGE_sq <- AGE^2
fWAR <- stats_stmb$WAR
ABOVE_300 <- stats_stmb$AVG_above
TEAM <- stats_stmb$TEAM_nextyr
POS <- stats_stmb$POS
L_Sal <- stats_stmb$`Log AVG ANNUAL`
individual <- stats_stmb$playerid
BATTING <- stats_stmb$Bat
FIELDING <- stats_stmb$Fld
BaseRun <- stats_stmb$BsR
AVG <- stats_stmb$AVG
Season <- stats_stmb$Season
DEFFENCE <- stats_stmb$Def
WPA <- stats_stmb$WPA
nWPA <- -stats_stmb$`-WPA`
WPA <- WPA/stats_stmb$G
nWPA <- nWPA/stats_stmb$G

M1_stmb <- lm(L_Sal ~ fWAR * ABOVE_300)
M2_stmb <- lm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun)
M3_stmb <- lm(L_Sal ~ fWAR * ABOVE_300 + AGE + AGE_sq)
M4_stmb <- lm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq)
M5_stmb <- felm(L_Sal ~ fWAR * ABOVE_300 + AGE + AGE_sq | TEAM)
M6_stmb <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq | POS)
M7_stmb <- felm(L_Sal ~ fWAR * ABOVE_300 | individual)
M8_stmb <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq | POS + TEAM )
M9_stmb <- felm(L_Sal ~ fWAR * ABOVE_300 + WPA + nWPA | individual + TEAM)
M10_stmb <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + WPA + nWPA |
              individual + TEAM + POS)
M11_stmb <- felm(L_Sal ~ fWAR * ABOVE_300 + AGE + AGE_sq + WPA + nWPA| TEAM)
M12_stmb <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq + WPA + nWPA|
              POS + TEAM)

summary(M0_stmb)
summary(M1_stmb)
summary(M2_stmb)
summary(M3_stmb)
summary(M4_stmb)
summary(M5_stmb)
summary(M6_stmb)
summary(M7_stmb)
summary(M8_stmb)
summary(M9_stmb)
summary(M10_stmb)
summary(M11_stmb)
summary(M12_stmb)

#After Moneyball

stats_afmb <- subset(stats_87, stats_87$Season >= 2002)

Sal_AVG <- stats_afmb$`AVG ANNUAL`
AGE <- stats_afmb$Age
AGE_sq <- AGE^2
fWAR <- stats_afmb$WAR
ABOVE_300 <- stats_afmb$AVG_above
TEAM <- stats_afmb$TEAM_nextyr
POS <- stats_afmb$POS
L_Sal <- stats_afmb$`Log AVG ANNUAL`
individual <- stats_afmb$playerid
BATTING <- stats_afmb$Bat
FIELDING <- stats_afmb$Fld
BaseRun <- stats_afmb$BsR
AVG <- stats_afmb$AVG
Season <- stats_afmb$Season
DEFFENCE <- stats_afmb$Def
WPA <- stats_afmb$WPA
nWPA <- -stats_afmb$`-WPA`
WPA <- WPA/stats_afmb$G
nWPA <- nWPA/stats_afmb$G

M1_afmb <- lm(L_Sal ~ fWAR * ABOVE_300)
M2_afmb <- lm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun)
M3_afmb <- lm(L_Sal ~ fWAR * ABOVE_300 + AGE + AGE_sq)
M4_afmb <- lm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq)
M5_afmb <- felm(L_Sal ~ fWAR * ABOVE_300 + AGE + AGE_sq | TEAM)
M6_afmb <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq | POS)
M7_afmb <- felm(L_Sal ~ fWAR * ABOVE_300 | individual)
M8_afmb <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq | POS + TEAM )
M9_afmb <- felm(L_Sal ~ fWAR * ABOVE_300 + WPA + nWPA | individual + TEAM)
M10_afmb <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + WPA + nWPA |
              individual + TEAM + POS)
M11_afmb <- felm(L_Sal ~ fWAR * ABOVE_300 + AGE + AGE_sq + WPA + nWPA| TEAM)
M12_afmb <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq + WPA + nWPA|
              POS + TEAM)

summary(M0_afmb)
summary(M1_afmb)
summary(M2_afmb)
summary(M3_afmb)
summary(M4_afmb)
summary(M5_afmb)
summary(M6_afmb)
summary(M7_afmb)
summary(M8_afmb)
summary(M9_afmb)
summary(M10_afmb)
summary(M11_afmb)
summary(M12_afmb)

Era <- stats_87$Era

M13 <- felm(L_Sal ~ fWAR * ABOVE_300 * Era + AGE + AGE_sq | individual + TEAM)
M14 <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq + Era |
              individual + TEAM)


MA1 <- felm(L_Sal ~ AVG_87 * ABOVE_300 * Era + FIELDING + BaseRun + AGE + AGE_sq
            | POS + TEAM)
MA2 <- felm(L_Sal ~ OBP_87 * ABOVE_300 * Era + FIELDING + BaseRun + AGE + AGE_sq
            | POS + TEAM)
MA3 <- felm(L_Sal ~ OPS_87 * ABOVE_300 * Era + FIELDING + BaseRun + AGE + AGE_sq
            | POS + TEAM)
MA4 <- felm(L_Sal ~ BATTING * ABOVE_300 * Era + FIELDING + BaseRun + WPA + nWPA
                   | POS + TEAM + individual)
MA5 <- felm(L_Sal ~ fWAR * ABOVE_300 * Era + FIELDING + BaseRun + WPA + nWPA
            | TEAM + individual)

summary(M13)
summary(M14)

summary(MA1)
summary(MA2)
summary(MA3)
summary(MA4)
