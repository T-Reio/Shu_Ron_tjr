library(stargazer)
library(png)
library(xtable)

#200打席以上:打率
png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/AVG_200PA.png",
    width = 700, height = 500)
hist(AVG_200, breaks = seq(0.130, 0.395, 0.001), main = "AVG for at least 200 PA",
     xlab = "Batting-Average")
dev.off()

#各打撃指標と相対年俸との関係
stargazer(MI1, MI2, MI4, MI5, MI7, MI8,
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/results/sal_index.tex")

#200打席以上　その他指標
summary(OBP)
png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/OBP_200PA.png",
    width = 700, height = 500)
hist(OBP, breaks = seq(0.170, 0.610, 0.001), main = "OBP for at least 200 PA",
     xlab = "On-Base Percentage")
dev.off()

png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/OPS_200PA.png",
    width = 700, height = 500)
hist(OPS, breaks = seq(0.380, 1.425, 0.001), main = "OPS for at least 200PA")
dev.off()

png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/H_90PA.png",
    width = 700, height = 500)
hist(H_57, breaks = seq(0,300,1), main = "Base-Hits for at least 90PA"
     , xlab = "Base-Hit")
dev.off()

png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/HR_90PA.png",
    width = 700, height = 500)
hist(HR_57, breaks = seq(0,75,1), main = "Homeruns for at least 90PA"
     , xlab = "HR")
dev.off()

png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/RBI_90PA.png",
    width = 700, height = 500)
hist(RBI_57, breaks = seq(0, 165, 1), main = "RBI for at least 90PA", xlab = 
       "Runs-Batted-in")
dev.off()

png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/SB_5SB.png",
    width = 700, height = 500)
hist(SB, breaks = seq(3,133,1), main = "SB for at least 90PA")
dev.off()

#三割の効果　full sample

stargazer(M1, M3, M5, M7, M9, M11,
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/results/sal_fwar.tex")

stargazer(M2, M4, M6, M8, M10, M12,
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/results/sal_off.tex")

#時代ごと　打率ヒストグラム

png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/AVG_bffa.png",
    width = 700, height = 500)
hist(AVG_bffa, breaks = seq(0.120, 0.400, 0.001), main = "AVG before FA",
     xlab = "Batting-Average")
dev.off()

png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/AVG_fast.png",
    width = 700, height = 500)
hist(AVG_fast, breaks = seq(0.120, 0.400, 0.001), main = "AVG FA to Strike",
     xlab = "Batting-Average")
dev.off()

png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/AVG_stmb.png",
    width = 700, height = 500)
hist(AVG_stmb, breaks = seq(0.120, 0.400, 0.001), main = "AVG Strike to MB",
     xlab = "Batting-average")
dev.off()

png("C:/Users/T-Reio/Master_thesis/master_thesis/graphs/AVG_afmb.png",
    width = 700, height = 500)
hist(AVG_afmb, breaks = seq(0.120, 0.400, 0.001), main = "AVG after MB",
     xlab = "Batting-Average")
dev.off()

#三割の効果 bfst

stargazer(M1_bfst, M3_bfst, M5_bfst, M7_bfst, M9_bfst, M11_bfst,
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/results/sal_fwar_bfst.tex")

stargazer(M2_bfst, M4_bfst, M6_bfst, M8_bfst, M10_bfst, M12_bfst,
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/results/sal_bat_bfst.tex")

#stmb
stargazer(M1_stmb, M3_stmb, M5_stmb, M7_stmb, M9_stmb, M11_stmb,
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/results/sal_fwar_stmb.tex")

stargazer(M2_stmb, M4_stmb, M6_stmb, M8_stmb, M10_stmb, M12_stmb,
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/results/sal_bat_stmb.tex")

#afmb

stargazer(M1_afmb, M3_afmb, M5_afmb, M7_afmb, M9_afmb, M11_afmb,
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/results/sal_fwar_afmb.tex")

stargazer(M2_afmb, M4_afmb, M6_afmb, M8_afmb, M10_afmb, M12_afmb,
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/results/sal_bat_afmb.tex")

#時代との交差項
stargazer(MA4,
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/results/sal_bat_didid.tex")
stargazer(MA5,
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/results/sal_fwar_didid.tex")
