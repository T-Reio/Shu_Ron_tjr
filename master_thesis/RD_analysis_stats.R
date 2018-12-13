library(data.table)
library(lfe)
library(rdd)
library(rddensity)
#count data

#No discontinuity
x<-runif(1000,-1,1)
DCdensity(x,0)

#Discontinuity
x<-runif(1000,-1,1)
x<-x+2*(runif(1000,-1,1)>0&x<0)
DCdensity(x,0, ext.out = T)

stats <- fread("C:/Users/easyu/Master_thesis/master_thesis/input/fstats_5718.csv"
                    , header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats <- subset(stats, stats$PA >= 90)
stats.5SB <- subset(stats, stats$SB >= 5)

stats <- subset(stats, stats$Season <= 1975)
stats <- subset(stats, stats$Season >= 1976 & stats$Season <= 1994)
stats <- subset(stats, stats$Season >= 1995 & stats$Season <= 2003)
stats <- subset(stats, stats$Season >= 2004)

stats <- subset(stats, stats$Season >= 2008)


AVG <- stats$H / stats$AB
OBP <- (stats$H + stats$BB + stats$IBB + stats$HBP) / (stats$AB ++ stats$BB 
                                                       + stats$IBB + stats$HBP + stats$SF)
fWAR <- stats$WAR
HR <- stats$HR
SB <- stats$SB
RBI <- stats$RBI
AVG <- stats$AVG
OPS <- stats$SLG + stats$OBP
PA <- stats$PA
H <- stats$H



DCdensity(AVG, 0.250, bin = 0.001, ext.out = T)
DCdensity(AVG, 0.2995, ext.out = T)
title('', xlab = 'Batting-Average', ylab = 'Density')
rect(0.297, 4.3, 0.303, 14, col = 'red')
DCdensity(AVG, 0.288, bin = 0.001, ext.out = T)

DCdensity(OBP, 0.35, bin = 0.001, ext.out = T)
title('', xlab = 'On-Base Percentage', ylab = 'Density')
DCdensity(OBP, 0.3495, ext.out = T)

DCdensity(OPS, 0.741, bin = 0.001, ext.out = T)
title('', xlab = 'Batting-Average', ylab = 'Density')
DCdensity(fWAR, 4.0, bin = 0.1)
DCdensity(HR, 20, bin = 1, ext.out = T)
title('', xlab = 'Homerun', ylab = 'Density')
DCdensity(SB, 30, bin = 1, ext.out = T)
title('', xlab = 'Stolen-Base', ylab = 'Density')
DCdensity(RBI, 100, bin = 3, ext.out = T)
title('', xlab = 'Runs Batted-In', ylab = 'Density')
DCdensity(PA, 500, bw = 2, bin = 1, ext.out = T)
title('', xlab = 'Plate-Appearance', ylab = 'Density')
DCdensity(H, 200, bin = 1, ext.out = T)
title('', xlab = 'Base-Hit', ylab = 'Density')

summary(OPS)
hist(fWAR, breaks = seq(-4, 13, 0.2))
hist(OPS, breaks = seq(0.38, 1.5, 0.001))


f.HR <- as.factor(HR)
summary(f.HR)

statmode <- function(x) {
  names(which.max(table(x)))
}

statmode(AVG)

rdplotdensity(rdd_AVG.300, AVG, plotRange = NULL, plotN = 10, plotGrid = c("es",
                                                                 "qs"), alpha = 0.05, type = NULL, CItype = NULL, title = "",
              xlabel = "", ylabel = "", lty = NULL, lwd = NULL, lcol = NULL,
              pty = NULL, pwd = NULL, pcol = NULL, CIshade = NULL, CIcol = NULL,
              legendTitle = NULL, legendGroups = NULL)

rdplotdensity(rdd_HR.20, HR, plotRange = NULL, plotN = 10, plotGrid = c("es",
                                                                           "qs"), alpha = 0.05, type = NULL, CItype = NULL, title = "",
              xlabel = "", ylabel = "", lty = NULL, lwd = NULL, lcol = NULL,
              pty = NULL, pwd = NULL, pcol = NULL, CIshade = NULL, CIcol = NULL,
              legendTitle = NULL, legendGroups = NULL)
