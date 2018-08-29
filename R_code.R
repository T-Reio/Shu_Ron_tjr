library(data.table)
tstats_npb<-fread("C:/users/T-Reio/Master_thesis/team_stats_npb.csv",header=T,sep=",")
summary(tstats_npb)
AVG<-tstats_npb$AVG
OBP<-tstats_npb$OBP
WA<-tstats_npb$WA
SLG<-tstats_npb$SLG
R<-tstats_npb$Runs
RA<-tstats_npb$Eruns

RegOBP<-lm(R~OBP,data=tstats_npb)
RegAVG<-lm(R~AVG,data=tstats_npb)
RegSLG<-lm(R~SLG,data=tstats_npb)
OPS=OBP+SLG
RegOPS<-lm(R~OPS,data=tstats_npb)

WARegOBP<-lm(WA~OBP,data=tstats_npb)
WARegAVG<-lm(WA~AVG,data=tstats_npb)
WARegSLG<-lm(WA~SLG,data=tstats_npb)
WARegOPS<-lm(WA~OPS,data=tstats_npb)

WARegOBP1<-lm(WA~OBP+RA,data=tstats_npb)
WARegAVG1<-lm(WA~AVG+RA,data=tstats_npb)
WARegSLG1<-lm(WA~SLG+RA,data=tstats_npb)
WARegOPS1<-lm(WA~OPS+RA,data=tstats_npb)

SRRegOPS<-lm(R~OBP+SLG,data=tstats_npb)
SWARegOPS<-lm(WA~OBP+SLG+RA,data=tstats_npb)

SRRegAPS<-lm(R~AVG+SLG,data=tstats_npb)
SWARegAPS<-lm(WA~AVG+SLG+RA,data=tstats_npb)


cor(AVG,R)
cor(OBP,R)
cor(SLG,R)
cor(OPS,R)
cor(AVG,WA)
cor(OBP,WA)
cor(SLG,WA)
cor(OPS,WA)

summary(RegAVG)
summary(RegSLG)
summary(RegOBP)
summary(RegOPS)

summary(WARegAVG)
summary(WARegSLG)
summary(WARegOBP)
summary(WARegOPS)

summary(WARegAVG1)
summary(WARegSLG1)
summary(WARegOBP1)
summary(WARegOPS1)

summary(SRRegOPS)
summary(SWARegOPS)

summary(SRRegAPS)
summary(SWARegAPS)

mean(SLG)
median(SLG)
mean(OBP)
median(OBP)


library(stargazer)
stargazer(SRRegOPS,SRRegAPS, title="Contribution to winning averages",out="C:/Users/T-Reio/Master_thesis/R_R_0628.tex",align=T,no.space=T)
stargazer(SWARegOPS,SWARegAPS, title="Contribution to runs",out="C:/Users/T-Reio/Master_thesis/R_WA_0628.tex",align=T,no.space=T)
