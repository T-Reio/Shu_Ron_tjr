library(data.table)
tstats_npb<-fread("C:/Users/T-Reio/Master_thesis/team_stats_npb.csv",header=T,sep=",")
summary(tstats_npb)
Avg<-tstats_npb$AVG
Obp<-tstats_npb$OBP
WA<-tstats_npb$WA
Slg<-tstats_npb$SLG
Runs<-tstats_npb$Runs
ERuns<-tstats_npb$Eruns

plot(Avg,Runs)
plot(Obp,Runs)
plot(Slg,Runs)
RegObp<-lm(Runs~Obp,data=tstats_npb)
RegAvg<-lm(Runs~Avg,data=tstats_npb)
RegSlg<-lm(Runs~Slg,data=tstats_npb)
OPS=Obp+Slg
RegOPS<-lm(Runs~OPS,data=tstats_npb)

WARegObp<-lm(WA~Obp,data=tstats_npb)
WARegAvg<-lm(WA~Avg,data=tstats_npb)
WARegSlg<-lm(WA~Slg,data=tstats_npb)
WARegOPS<-lm(WA~OPS,data=tstats_npb)

WARegObp1<-lm(WA~Obp+ERuns,data=tstats_npb)
WARegAvg1<-lm(WA~Avg+ERuns,data=tstats_npb)
WARegSlg1<-lm(WA~Slg+ERuns,data=tstats_npb)
WARegOPS1<-lm(WA~OPS+ERuns,data=tstats_npb)

SRRegOPS<-lm(Runs~Obp+Slg,data=tstats_npb)
SWARegOPS<-lm(WA~Obp+Slg+ERuns,data=tstats_npb)

SRRegAPS<-lm(Runs~Avg+Slg,data=tstats_npb)
SWARegAPS<-lm(WA~Avg+Slg+ERuns,data=tstats_npb)


cor(Avg,Runs)
cor(Obp,Runs)
cor(Slg,Runs)
cor(OPS,Runs)
cor(Avg,WA)
cor(Obp,WA)
cor(Slg,WA)
cor(OPS,WA)

summary(RegAvg)
summary(RegSlg)
summary(RegObp)
summary(RegOPS)

summary(WARegAvg)
summary(WARegSlg)
summary(WARegObp)
summary(WARegOPS)

summary(WARegAvg1)
summary(WARegSlg1)
summary(WARegObp1)
summary(WARegOPS1)

summary(SRRegOPS)
summary(SWARegOPS)

summary(SRRegAPS)
summary(SWARegAPS)

mean(Slg)
median(Slg)
mean(Obp)
median(Obp)
