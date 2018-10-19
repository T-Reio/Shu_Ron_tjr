library(data.table)
library(stargazer)
team_stats_mlb <- fread("C:/Users/T-Reio/Master_thesis/master_thesis/output/team_stats_87to17.csv", header = T, sep = ",")
WA <- team_stats_mlb$`W-L%`
R <- team_stats_mlb$R
RA <- team_stats_mlb$RA
AVG <- team_stats_mlb$BA
OBP <- team_stats_mlb$OBP
SLG <- team_stats_mlb$SLG
OPS <- team_stats_mlb$OPS
Luck <- team_stats_mlb$Luck
Pyth <- (R^1.72 /(R^1.72 + RA^1.72))

summary(Luck)
hist(Luck, breaks = seq(-15, 15, 1), main = "Luck")

cor(AVG, OBP)
cor(OBP, SLG)
cor(AVG, SLG)
cor(AVG, OPS)
cor(R, WA)
cor(Pyth, WA)

Model1 <- lm(R ~ AVG)
Model2 <- lm(R ~ OBP)
Model3 <- lm(R ~ SLG)
Model4 <- lm(R ~ AVG + OBP)
Model5 <- lm(R ~ AVG + SLG)
Model6 <- lm(R ~ OBP + SLG)
Model7 <- lm(R ~ OPS)
Model8 <- lm(R ~ AVG + OBP + SLG)

summary(Model1)
summary(Model2)
summary(Model3)
summary(Model4)
summary(Model5)

WA1 <- lm(WA ~ AVG + RA)
WA2 <- lm(WA ~ OBP + RA)
WA3 <- lm(WA ~ SLG + RA)
WA4 <- lm(WA ~ AVG + SLG + RA)
WA5 <- lm(WA ~ OBP + SLG + RA)
WA6 <- lm(WA ~ OPS + RA)

cor(R, RA)

stargazer(Model1, Model2, Model3, Model5, Model6, Model7,
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/team_stats_R_a.tex",
          title = "OLS on Runs")

stargazer(WA1, WA2, WA3, WA4, WA5, WA6,
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/team_stats_R_b.tex",
          title = "OLS on WA")

#200打席以上打率3割越えの打者の数と勝利の相関