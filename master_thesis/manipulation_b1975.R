library(data.table)
library(lfe)
library(rdd)
library(rddensity)
stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/input/fstats_5718.csv"
               , header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, stats$Season <= 1975)

AVG <- stats$H / stats$AB
OBP <- (stats$H + stats$BB + stats$IBB + stats$HBP) / (stats$AB ++ stats$BB 
                                                       + stats$IBB + stats$HBP + stats$SF)
HR <- stats$HR

DCdensity(AVG, 0.3, bin = 0.001, ext.out = T)
title('', xlab = 'Batting-Average', ylab = 'Density')

DCdensity(AVG, 0.25, bin = 0.001, ext.out = T)
title('', xlab = 'Batting-Average', ylab = 'Density')

DCdensity(OBP, 0.35, bin = 0.001, ext.out = T)
title('', xlab = 'On-Base Percentage', ylab = 'Density')

DCdensity(HR, 20, bin = 1, ext.out = T)$bw
title('', xlab = 'Homerun', ylab = 'Density')


#FAST
stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/input/fstats_5718.csv"
               , header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, stats$Season >= 1976 & stats$Season <= 1994)
AVG <- stats$H / stats$AB
OBP <- (stats$H + stats$BB + stats$IBB + stats$HBP) / (stats$AB ++ stats$BB 
                                                       + stats$IBB + stats$HBP + stats$SF)
HR <- stats$HR

DCdensity(AVG, 0.3, bin = 0.001, ext.out = T)
title('', xlab = 'Batting-Average', ylab = 'Density')

DCdensity(AVG, 0.25, bin = 0.001, ext.out = T)
title('', xlab = 'Batting-Average', ylab = 'Density')

DCdensity(OBP, 0.35, bin = 0.001, ext.out = T)
title('', xlab = 'On-Base Percentage', ylab = 'Density')

DCdensity(HR, 20, bin = 1, ext.out = T)
title('', xlab = 'Homerun', ylab = 'Density')


#STMB
stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/input/fstats_5718.csv"
               , header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, stats$Season >= 1995 & stats$Season <= 2003)
AVG <- stats$H / stats$AB
OBP <- (stats$H + stats$BB + stats$IBB + stats$HBP) / (stats$AB ++ stats$BB 
                                                       + stats$IBB + stats$HBP + stats$SF)
HR <- stats$HR

DCdensity(AVG, 0.3, bin = 0.001, ext.out = T)
title('', xlab = 'Batting-Average', ylab = 'Density')

DCdensity(AVG, 0.25, bin = 0.001, ext.out = T)
title('', xlab = 'Batting-Average', ylab = 'Density')

DCdensity(OBP, 0.35, bin = 0.001, ext.out = T)
title('', xlab = 'On-Base Percentage', ylab = 'Density')

DCdensity(HR, 20, bin = 1, ext.out = T)
title('', xlab = 'Homerun', ylab = 'Density')


#AFMB
stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/input/fstats_5718.csv"
               , header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, stats$Season >= 2004)
AVG <- stats$H / stats$AB
OBP <- (stats$H + stats$BB + stats$IBB + stats$HBP) / (stats$AB ++ stats$BB 
                                                       + stats$IBB + stats$HBP + stats$SF)
HR <- stats$HR

DCdensity(AVG, 0.3, bin = 0.001, ext.out = T)
title('', xlab = 'Batting-Average', ylab = 'Density')

DCdensity(AVG, 0.25, bin = 0.001, ext.out = T)
title('', xlab = 'Batting-Average', ylab = 'Density')

DCdensity(OBP, 0.35, bin = 0.001, ext.out = T)
title('', xlab = 'On-Base Percentage', ylab = 'Density')

DCdensity(HR, 20, bin = 1, ext.out = T)
title('', xlab = 'Homerun', ylab = 'Density')
