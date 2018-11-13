png("C:/Users/easyu/Master_thesis/master_thesis/graphs/hist_AVG_all.png", width = 700, height = 500)
hist(AVG, breaks=seq(0.12,0.4,0.001), main = "")
dev.off()

png("C:/Users/easyu/Master_thesis/master_thesis/graphs/hist_OBP_all.png", width = 700, height = 500)
hist(OBP, breaks = seq(0.15, 0.69, 0.001), main = "")
dev.off()

summary(OBP)

png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/hist_HR_all.png", width = 700, height = 500)
hist(HR, breaks = seq(0,75,1), main = "Homeruns")
dev.off()

png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/hist_OPS_all.png", width = 700, height = 500)
hist(OPS, breaks = seq(0.35,1.5,0.001), main = "OPS")
dev.off()

png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/hist_H_all.png", width = 700, height = 500)
hist(H, breaks = seq(0,270,1), main = "Base-Hits")
dev.off()

png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/hist_RBI_all.png", width = 700, height = 500)
hist(RBI, breaks = seq(0,170,1), main = "Runs-Batted-In")
dev.off()

png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/hist_SB_all.png", width = 700, height = 500)
hist(SB, breaks = seq(0,130,1), main = "Stolen-Bases")
dev.off()

png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/hist_PA_all.png", width = 700, height = 500)
hist(PA, breaks = seq(200,780,5), main = "Plate-Appearances")
dev.off()

fWAR <- stats$WAR
png("F:/野球データ/hist_fWAR_all.png", width = 700, height = 500)
hist(fWAR, breaks = seq(-5,15,0.2), main = "fWAR")
dev.off()

png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/hist_AVG_aftermb.png", 
    width = 700, height = 500)
hist(AVG_after, breaks = seq(0.120, 0.400, 0.001), main = "AVG after 'Moneyball'")
dev.off()

png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/hist_AVG_beforemb.png", 
    width = 700, height = 500)
hist(AVG_before, breaks = seq(0.120, 0.400, 0.001), main = "AVG after 'Moneyball'")
dev.off()


png("C:/Users/T-Reio/Master_thesis/master_thesis/hist_AVG.png", width = 700, height = 500)
hist(AVG_since00, breaks=seq(0.12,0.4,0.001))
dev.off()

OBP_since00 <- stats_since00$OBP

png("C:/Users/T-Reio/Master_thesis/master_thesis/hist_OBP_since00.png", width = 700, height = 500)
hist(OBP_since00, breaks = seq(0.15, 0.65, 0.001))
dev.off()


HR_since00 <- stats_since00$HR
hist(HR_since00, breaks = seq(0,75,1))

OPS_since00 <- stats_since00$OPS
hist(OPS_since00, breaks = seq(0.35,1.5,0.005))

H_since00 <- stats_since00$H
hist(H_since00, breaks = seq(0,270,5))

Sal_AVG <- stats_since00$`AVG ANNUAL`
AGE <- stats_since00$Age
AGE_sq <- AGE^2
fWAR <- stats_since00$WAR
ABOVE_300 <- stats_since00$AVG_above
TEAM <- stats_since00$TEAM_played
POS <- stats_since00$POS



Sal_Model1 <- lm(Sal_AVG ~ fWAR + ABOVE_300)
Sal_Model2 <- lm(Sal_AVG ~ fWAR + ABOVE_300 + AGE + AGE_sq)
Sal_Model3 <- lm(Sal_AVG ~ fWAR + ABOVE_300 + AGE + AGE_sq + TEAM)

Sal_Model4 <- lm(Sal_AVG ~ fWAR)
Sal_Model5 <- lm(Sal_AVG ~ fWAR + AGE + AGE_sq)
Sal_Model6 <- lm(Sal_AVG ~ fWAR + AGE + AGE_sq + TEAM)

stargazer(Sal_Model1, Sal_Model2, Sal_Model3,
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/Sal_OLS_include.tex",
          title = "")

stargazer(Sal_Model4, Sal_Model5, Sal_Model6,
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/Sal_OLS_exclude.tex",
          title = "")

png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/AVG_since57.png", width = 700, height = 500)
hist(AVG_200, breaks = seq(0.120, 0.400, 0.001), main = "Batting-Average since '57")
dev.off()


stargazer(Model8, Model9, Model10, title = "",
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/results/Sal_fixed_effect.tex")

