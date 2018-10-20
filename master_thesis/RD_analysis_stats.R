library(data.table)
library(lfe)
library(rdd)
library(rddensity)
library(ggplot2)
library(gridExtra)
stats <- fread("C:/Users/T-Reio/Master_thesis/master_thesis/output/battingstats_since57.csv"
               , header = T, sep = ",")
stats <- subset(stats, stats$PA >= 200)
stats.5SB <- subset(stats, stats$SB >= 5)
AVG <- stats$AVG
OBP <- stats$OBP
HR <- stats$HR
SB <- stats$SB
OPS <- stats$OPS
HR <- as.numeric(HR)
SB <- as.numeric(SB)


rdd_AVG.300 <- rddensity(AVG, c = 0.2995)
summary(rdd_AVG.300)
rdd_AVG.300$test

rdd_AVG.250 <- rddensity(AVG, c = 0.250)
rdd_AVG.250$test

rdd_OBP.400 <- rddensity(OBP, c = 0.400)
rdd_OBP.400$test
summary(rdd_OBP.400)

rdd_HR.10 <- rddensity(HR, c = 10)
summary(rdd_HR.10)
rdd_HR.10$test

rdd_HR.20 <- rddensity(HR, c = 20)
summary(rdd_HR.20)
rdd_HR.20$test

rdd_SB.20 <- rddensity(SB, c = 20)
summary(rdd_SB.20)
rdd_SB.20$test

rdd_OPS.800 <- rddensity(OPS, c = 0.8)
summary(rdd_OPS.800)

HR.dev <- HR/100
rdd_HR.dev <- rddensity(HR.dev, c = 0.2)
summary(rdd_HR.dev)

SB <- stats.5SB$SB
SB.dev <- SB/10
rdd_SB.dev <- rddensity(SB.dev, c = 3)
summary(rdd_HR.dev)
rdd_SB.dev$test

ggplot(AVG, mapping = aes())

af <- subset(stats, stats$Season >= 2014)
bf <- subset(stats, stats$Season <= 1964)
AVG_af <- af$AVG
AVG_bf <- bf$AVG
shapiro.test(AVG_afmb)
ks.test(AVG_af, AVG_bf)

#count data

count.AVG <- fread("C:/Users/T-Reio/Master_thesis/master_thesis/output/count_AVG.csv",
                   header = T, sep = ",")
AVG <- count.AVG$index
freq <- count.AVG$`0`

RDfu <- lm(freq ~ AVG)
summary(RDfu)

hist.AVG <- hist(AVG, breaks = seq(0.120, 0.400, 0.001))

counts <- hist.AVG$counts
hist.AVG$mids
hist.AVG$breaks

AVG.DC <- DCdensity(AVG, 0.217, bin = NULL, bw = NULL,
                    verbose = FALSE, plot = TRUE, ext.out = T, htest = T)
summary(AVG.DC)
AVG.DC$p

HR <- HR/100
HR.DC <- DCdensity(HR, 0.26)
HR.DC

CalAVG <- 

#No discontinuity
x<-runif(1000,-1,1)
DCdensity(x,0)

#Discontinuity
x<-runif(1000,-1,1)
x<-x+2*(runif(1000,-1,1)>0&x<0)
DCdensity(x,0, ext.out = T)

stats <- fread("C:/Users/T-Reio/Master_thesis/master_thesis/output/roughstats_5718.csv"
                    , header = T, sep = ",")

stats <- subset(stats, stats$Season <= 1975)
stats <- subset(stats, stats$Season >= 1976 & stats$Season <= 1994)
stats <- subset(stats, stats$Season >= 1995 & stats$Season <= 2003)
stats <- subset(stats, stats$Season <= 2004)

stats <- subset(stats, stats$Season >= 2004)

AVG <- stats$H / stats$AB
OBP <- stats$OBP
fWAR <- stats$WAR

DCdensity(AVG, 0.2995)
DCdensity(AVG, 0.2585)

DCdensity(OBP, 0.39)
DCdensity(fWAR, 3.0)

summary(fWAR)
hist(fWAR, breaks = seq(-4, 13, 0.1))

count <- fread("C:/Users/T-Reio/Master_thesis/master_thesis/output/count_AVG.csv"
               , header = T, sep = ",")

freq <- count$'0'
AVG <- count$AVG

summary(AVG)

count <- subset(count, count$AVG >= 0.251)

count <- subset(count, count$AVG <= 0.299)
count <- subset(count, count$AVG >= 0.300)

freq <- count$'0'
AVG <- count$AVG
AVG <- stats$AVG

est2 <- lm(freq ~ AVG)

summary(est1)
summary(est2)

predict(est1, df.test)

1520.45 + 0.3 * (-4730.59)
495.166 + 0.3 * (-1371.246)

statmode <- function(x) {
  names(which.max(table(x)))
}

statmode(AVG)
