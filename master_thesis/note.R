library(data.table)
library(stargazer)
library(lfe)
library(rddensity)
install.packages("rdd", dependencies = T)
install.packages("rddensity", dependencies = T)

help(RDestimate, package = "rdd")
help(package = "rddensity")
help(ggplot, package = "ggplot2")
help("shapiro.test")
help("hist")
help(package = "rdd")

rdd <- rddensity(AVG, c = 0.300)
summary(rdd)
rdd$hat
rdd$test

rdd.250 <- rddensity(AVG, c = 0.250)
rdd.250$test




#RDestimate(formula, data, subset = NULL, cutpoint = NULL, bw = NULL,kernel = 
# "triangular", se.type = "HC1", cluster = NULL,verbose = FALSE, model = FALSE, frame = FALSE)