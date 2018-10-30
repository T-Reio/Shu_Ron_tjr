library(data.table)
library(stargazer)
library(lfe)
library(rddensity)
library(Hmisc)
library(plm)
library(xtable)

install.packages("rdd", dependencies = T)
install.packages("rddensity", dependencies = T)

help(RDestimate, package = "rdd")
help(package = "rddensity")
help(ggplot, package = "ggplot2")
help("shapiro.test")
help("hist")
help(package = "rdd")
help(DCdensity)
help(latex)
help("stargazer")

rdd <- rddensity(AVG, c = 0.300)
summary(rdd)
rdd$hat
rdd$test

rdd.250 <- rddensity(AVG, c = 0.250)
rdd.250$test

latex(m1$coefficients, m1$df.residual, file='', booktabs=T, dcolumn=T)
m1$effects
m1$rank
m1$df.residual
m1$assign
m1$terms
r1$bw
latex(r1$est, file = '', booktabs = T, dcolumn = T)

xtable(m1)
latex(list(r1$est, r1$bw), file = '', booktabs = T, dcolumn = T)
r1$ci

stargazer(list(r1$est, r1$se), out = '')
r1

summary(r1)

#packages required
install.packages("data.table", dependencies = T)
install.packages("stargazer", dependencies = T)
install.packages("lfe", dependencies = T)
install.packages("rddensity", dependencies = T)
install.packages("rdd", dependencies = T)
install.packages("plm", dependencies = T)
install.packages("Hmisc", dependencies = T)
install.packages("", dependencies = T)
install.packages("", dependencies = T)
install.packages("", dependencies = T)