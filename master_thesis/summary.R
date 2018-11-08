library(data.table)
library(lfe)
library(rdd)
library(rddensity)
library(stargazer)

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/input/fstats_5718.csv"
               , header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)

stargazer(stats,
          out = 'C:/Users/easyu/Master_thesis/master_thesis/results/',
          title = 'Summary Statistics', align = T, initial.zero = F,
          font.size = "scriptsize", label = "sum", table.placement = "H")

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/output/stats_sal_fa_revised.csv",
               header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)

stargazer(stats,
          out = 'C:/Users/easyu/Master_thesis/master_thesis/results/',
          title = 'Summary Statistics Sample B', align = T, initial.zero = F,
          font.size = "scriptsize", label = "sum_B", table.placement = "H")
