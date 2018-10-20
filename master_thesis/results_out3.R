library(stargazer)
stargazer(rdd_AVG.300,
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/results/rdd.tex")

stargazer(RDD1,
          out = "C:/Users/T-Reio/Master_thesis/master_thesis/results/rdd_AVG.tex")

png("C:/Users/T-Reio/Master_thesis/master_thesis/results/avg_dcd.png",
    width = 700, height = 500)
DCdensity(AVG, 0.300, bin = 0.001)
dev.off()
title(xlab = 'Batting-Average', ylab = 'Density')