library(data.table)
library(stargazer)
team_stats_mlb <- fread("C:/Users/T-Reio/Master_thesis/master_thesis/output/team_stats_00to17.csv", header = T, sep = ",")
WA <- team_stats_mlb$`W-L%`
R <- team_stats_mlb$R
RA <- team_stats_mlb$RA
AVG <- team_stats_mlb$BA
OBP <- team_stats_mlb$OBP
SLG <- team_stats_mlb$SLG
OPS <- team_stats_mlb$OPS

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

stargazer(Model1, Model2, Model3, Model4,
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/team_stats_R_a.tex",
          title = "OLS on Runs")

stargazer(Model5, Model6, Model7, Model8,
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/team_stats_R_b.tex",
          title = "OLS on Runs")

#200打席以上打率3割越えの打者の数と勝利の相関