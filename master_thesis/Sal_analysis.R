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
nWPA <- stats_87$`-WPA`

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
M7 <- felm(L_Sal ~ fWAR * ABOVE_300 + AGE + AGE_sq | individual)
M8 <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq | POS + TEAM )
M9 <- felm(L_Sal ~ fWAR * ABOVE_300 + AGE + AGE_sq | individual + TEAM)
M10 <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq |
             individual + TEAM)
M11 <- felm(L_Sal ~ fWAR * ABOVE_300 + AGE + AGE_sq  + WPA + nWPA| individual + TEAM)
M12 <- felm(L_Sal ~ BATTING * ABOVE_300 + FIELDING + BaseRun + AGE + AGE_sq + WPA + nWPA|
              individual + TEAM)

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

Sal_AVG_bfst <- stats_bfst$`AVG ANNUAL`
AGE_bfst <- stats_bfst$Age
AGE_sq_bfst <- AGE_bfst^2
fWAR_bfst <- stats_bfst$WAR
ABOVE_300_bfst <- stats_bfst$AVG_above
TEAM_bfst <- stats_bfst$TEAM_nextyr
POS_bfst <- stats_bfst$POS
L_Sal_bfst <- stats_bfst$`Log AVG ANNUAL`
individual_bfst <- stats_bfst$playerid
BATTING_bfst <- stats_bfst$Bat
FIELDING_bfst <- stats_bfst$Fld
BaseRun_bfst <- stats_bfst$BsR
AVG_bfst <- stats_bfst$AVG
Season_bfst <- stats_bfst$Season
DEFFENCE_bfst <- stats_bfst$Def
WPA_bfst <- stats_bfst$WPA
nWPA_bfst <- stats_bfst$`-WPA`

M0_bfst <- lm(L_Sal_bfst ~ AVG_bfst * ABOVE_300_bfst + FIELDING_bfst + BaseRun_bfst)
M1_bfst <- lm(L_Sal_bfst ~ fWAR_bfst * ABOVE_300_bfst)
M2_bfst <- lm(L_Sal_bfst ~ BATTING_bfst * ABOVE_300_bfst + FIELDING_bfst + BaseRun_bfst)
M3_bfst <- lm(L_Sal_bfst ~ fWAR_bfst * ABOVE_300_bfst + AGE_bfst + AGE_sq_bfst)
M4_bfst <- lm(L_Sal_bfst ~ BATTING_bfst * ABOVE_300_bfst + FIELDING_bfst + BaseRun_bfst
              + AGE_bfst + AGE_sq_bfst)
M5_bfst <- felm(L_Sal_bfst ~ fWAR_bfst * ABOVE_300_bfst + AGE_bfst + AGE_sq_bfst | TEAM_bfst)
M6_bfst <- felm(L_Sal_bfst ~ BATTING_bfst * ABOVE_300_bfst + FIELDING_bfst + BaseRun_bfst
                + AGE_bfst + AGE_sq_bfst | POS_bfst)
M7_bfst <- felm(L_Sal_bfst ~ fWAR_bfst * ABOVE_300_bfst + AGE_bfst + AGE_sq_bfst 
                | individual_bfst)
M8_bfst <- felm(L_Sal_bfst ~ BATTING_bfst * ABOVE_300_bfst + FIELDING_bfst + BaseRun_bfst
                + AGE_bfst + AGE_sq_bfst | POS_bfst + TEAM_bfst )
M9_bfst <- felm(L_Sal_bfst ~ fWAR_bfst * ABOVE_300_bfst + AGE_bfst + AGE_sq_bfst 
                | individual_bfst + TEAM_bfst)
M10_bfst <- felm(L_Sal_bfst ~ BATTING_bfst * ABOVE_300_bfst + FIELDING_bfst + BaseRun_bfst
                 + AGE_bfst + AGE_sq_bfst | individual_bfst + TEAM_bfst)
M11_bfst <- felm(L_Sal_bfst ~ fWAR_bfst * ABOVE_300_bfst + AGE_bfst + AGE_sq_bfst  
                 + WPA_bfst + nWPA_bfst| individual_bfst + TEAM_bfst)
M12_bfst <- felm(L_Sal_bfst ~ BATTING_bfst * ABOVE_300_bfst + FIELDING_bfst + BaseRun_bfst
                 + AGE_bfst + AGE_sq_bfst + WPA_bfst + nWPA_bfst
                 | individual_bfst + TEAM_bfst)

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

Sal_AVG_stmb <- stats_stmb$`AVG ANNUAL`
AGE_stmb <- stats_stmb$Age
AGE_sq_stmb <- AGE_stmb^2
fWAR_stmb <- stats_stmb$WAR
ABOVE_300_stmb <- stats_stmb$AVG_above
TEAM_stmb <- stats_stmb$TEAM_nextyr
POS_stmb <- stats_stmb$POS
L_Sal_stmb <- stats_stmb$`Log AVG ANNUAL`
individual_stmb <- stats_stmb$playerid
BATTING_stmb <- stats_stmb$Bat
FIELDING_stmb <- stats_stmb$Fld
BaseRun_stmb <- stats_stmb$BsR
AVG_stmb <- stats_stmb$AVG
Season_stmb <- stats_stmb$Season
DEFFENCE_stmb <- stats_stmb$Def
WPA_stmb <- stats_stmb$WPA
nWPA_stmb <- stats_stmb$`-WPA`

M0_stmb <- lm(L_Sal_stmb ~ AVG_stmb * ABOVE_300_stmb + FIELDING_stmb + BaseRun_stmb)
M1_stmb <- lm(L_Sal_stmb ~ fWAR_stmb * ABOVE_300_stmb)
M2_stmb <- lm(L_Sal_stmb ~ BATTING_stmb * ABOVE_300_stmb + FIELDING_stmb + BaseRun_stmb)
M3_stmb <- lm(L_Sal_stmb ~ fWAR_stmb * ABOVE_300_stmb + AGE_stmb + AGE_sq_stmb)
M4_stmb <- lm(L_Sal_stmb ~ BATTING_stmb * ABOVE_300_stmb + FIELDING_stmb
              + BaseRun_stmb + AGE_stmb + AGE_sq_stmb)
M5_stmb <- felm(L_Sal_stmb ~ fWAR_stmb * ABOVE_300_stmb + AGE_stmb + AGE_sq_stmb 
                | TEAM_stmb)
M6_stmb <- felm(L_Sal_stmb ~ BATTING_stmb * ABOVE_300_stmb + FIELDING_stmb + BaseRun_stmb
                + AGE_stmb + AGE_sq_stmb | POS_stmb)
M7_stmb <- felm(L_Sal_stmb ~ fWAR_stmb * ABOVE_300_stmb + AGE_stmb + AGE_sq_stmb
                | individual_stmb)
M8_stmb <- felm(L_Sal_stmb ~ BATTING_stmb * ABOVE_300_stmb + FIELDING_stmb + BaseRun_stmb
                + AGE_stmb + AGE_sq_stmb | POS_stmb + TEAM_stmb )
M9_stmb <- felm(L_Sal_stmb ~ fWAR_stmb * ABOVE_300_stmb + AGE_stmb + AGE_sq_stmb
                | individual_stmb + TEAM_stmb)
M10_stmb <- felm(L_Sal_stmb ~ BATTING_stmb * ABOVE_300_stmb + FIELDING_stmb + BaseRun_stmb
                 + AGE_stmb + AGE_sq_stmb | individual_stmb + TEAM_stmb)
M11_stmb <- felm(L_Sal_stmb ~ fWAR_stmb * ABOVE_300_stmb + AGE_stmb + AGE_sq_stmb
                 + WPA_stmb + nWPA_stmb| individual_stmb + TEAM_stmb)
M12_stmb <- felm(L_Sal_stmb ~ BATTING_stmb * ABOVE_300_stmb + FIELDING_stmb
                 + BaseRun_stmb + AGE_stmb + AGE_sq_stmb + WPA_stmb + nWPA_stmb|
                   individual_stmb + TEAM_stmb)

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

Sal_AVG_afmb <- stats_afmb$`AVG ANNUAL`
AGE_afmb <- stats_afmb$Age
AGE_sq_afmb <- AGE_afmb^2
fWAR_afmb <- stats_afmb$WAR
ABOVE_300_afmb <- stats_afmb$AVG_above
TEAM_afmb <- stats_afmb$TEAM_nextyr
POS_afmb <- stats_afmb$POS
L_Sal_afmb <- stats_afmb$`Log AVG ANNUAL`
individual_afmb <- stats_afmb$playerid
BATTING_afmb <- stats_afmb$Bat
FIELDING_afmb <- stats_afmb$Fld
BaseRun_afmb <- stats_afmb$BsR
AVG_afmb <- stats_afmb$AVG
Season_afmb <- stats_afmb$Season
DEFFENCE_afmb <- stats_afmb$Def
WPA_afmb <- stats_afmb$WPA
nWPA_afmb <- stats_afmb$`-WPA`

M0_afmb <- lm(L_Sal_afmb ~ AVG_afmb * ABOVE_300_afmb + FIELDING_afmb + BaseRun_afmb)
M1_afmb <- lm(L_Sal_afmb ~ fWAR_afmb * ABOVE_300_afmb)
M2_afmb <- lm(L_Sal_afmb ~ BATTING_afmb * ABOVE_300_afmb + FIELDING_afmb + BaseRun_afmb)
M3_afmb <- lm(L_Sal_afmb ~ fWAR_afmb * ABOVE_300_afmb + AGE_afmb + AGE_sq_afmb)
M4_afmb <- lm(L_Sal_afmb ~ BATTING_afmb * ABOVE_300_afmb + FIELDING_afmb + BaseRun_afmb
              + AGE_afmb + AGE_sq_afmb)
M5_afmb <- felm(L_Sal_afmb ~ fWAR_afmb * ABOVE_300_afmb + AGE_afmb + AGE_sq_afmb | TEAM_afmb)
M6_afmb <- felm(L_Sal_afmb ~ BATTING_afmb * ABOVE_300_afmb + FIELDING_afmb + BaseRun_afmb
                + AGE_afmb + AGE_sq_afmb | POS_afmb)
M7_afmb <- felm(L_Sal_afmb ~ fWAR_afmb * ABOVE_300_afmb + AGE_afmb + AGE_sq_afmb
                | individual_afmb)
M8_afmb <- felm(L_Sal_afmb ~ BATTING_afmb * ABOVE_300_afmb + FIELDING_afmb + BaseRun_afmb
                + AGE_afmb + AGE_sq_afmb | POS_afmb + TEAM_afmb )
M9_afmb <- felm(L_Sal_afmb ~ fWAR_afmb * ABOVE_300_afmb + AGE_afmb + AGE_sq_afmb
                | individual_afmb + TEAM_afmb)
M10_afmb <- felm(L_Sal_afmb ~ BATTING_afmb * ABOVE_300_afmb + FIELDING_afmb + BaseRun_afmb
                 + AGE_afmb + AGE_sq_afmb | individual_afmb + TEAM_afmb)
M11_afmb <- felm(L_Sal_afmb ~ fWAR_afmb * ABOVE_300_afmb + AGE_afmb + AGE_sq_afmb
                 + WPA_afmb + nWPA_afmb| individual_afmb + TEAM_afmb)
M12_afmb <- felm(L_Sal_afmb ~ BATTING_afmb * ABOVE_300_afmb + FIELDING_afmb
                 + BaseRun_afmb + AGE_afmb + AGE_sq_afmb + WPA_afmb + nWPA_afmb|
                   individual_afmb + TEAM_afmb)

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
            | POS + individual + TEAM)
MA2 <- felm(L_Sal ~ OBP_87 * ABOVE_300 * Era + FIELDING + BaseRun + AGE + AGE_sq
            | POS + individual + TEAM)
MA3 <- felm(L_Sal ~ OPS_87 * ABOVE_300 * Era + FIELDING + BaseRun + AGE + AGE_sq
            | POS + individual + TEAM)
MA4 <- felm(L_Sal ~ BATTING * ABOVE_300 * Era + FIELDING + BaseRun + AGE + AGE_sq
                   | POS + individual + TEAM)
MA5 <- felm(L_Sal ~ fWAR * ABOVE_300 * Era + FIELDING + BaseRun + AGE + AGE_sq
            | POS + individual + TEAM)

summary(M13)
summary(M14)

summary(MA1)
summary(MA2)
summary(MA3)
summary(MA4)
