library(data.table)
library(stargazer)
library(tikzDevice)
library(gplots)

stats <- fread("C:/Users/T-Reio/Master_thesis/master_thesis/output/battingstats_74to17.csv", header = T, sep = ",")

AVG <- stats$AVG
hist(AVG, breaks=seq(0.12,0.4,0.001))
AVG_fac <- as.factor(AVG)
summary(AVG_fac)

OBP <- stats$OBP
HR <- stats$HR
OPS <- stats$OPS
H <- stats$H
RBI <- stats$RBI
SB <- stats$SB
PA <- stats$PA
G <- stats$G
hist(G, breaks = seq(40,165,1))

after_MB <- subset(stats, stats$Season >= 2004)

AVG_after <- after_MB$AVG
hist(AVG_after, breaks = seq(0.120, 0.400, 0.001))

before_MB <- subset(stats, stats$Season >= 1987 & stats$Season <= 2000)

AVG_before <- before_MB$AVG
hist(AVG_before, breaks = seq(0.120, 0.400, 0.001))


AVG_285to315 <- subset(stats, stats$AVG >= 0.285 & stats$AVG <= 0.315)
AVG_in <- AVG_285to315$AVG
hist(AVG_in, breaks = seq(0.285, 0.315, 0.001))

OBP_after <- after_MB$OBP
hist(OBP_after, breaks = seq(0.15, 0.68, 0.001))

RBI_before <- before_MB$RBI
RBI_after <- after_MB$RBI
hist(RBI_after, breaks = seq(0, 170, 1))
hist(RBI_before, breaks = seq(0, 170, 1))

AVG_factor <- as.factor(AVG_285to315$AVG)

summary(AVG_factor)

