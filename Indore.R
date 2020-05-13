options(stringsAsFactors=F)

Data.1=read.csv("raw_data1.csv")
Data.2=read.csv("raw_data2.csv")
Data.3=read.csv("raw_data3.csv")

Indore.1=Data.1[Data.1$Detected.District=="Indore",3]
Indore.2=Data.2[Data.2$Detected.District=="Indore",3]

Indore.3=Data.3[Data.3$Detected.District=="Indore",c(3,10)]

IndoreInc.1=table(Indore.1)
IndoreInc.2=table(Indore.2)

IndoreInc.3=Indore.3$Num.Cases
names(IndoreInc.3)=Indore.3$Date.Announced
IndoreInc=c(IndoreInc.1,IndoreInc.2,IndoreInc.3)

IndoreInc.Table=data.frame(names(IndoreInc),IndoreInc)
IndoreInc.Table=aggregate(IndoreInc.Table$IndoreInc,by=list(Date=IndoreInc.Table$names.IndoreInc.),FUN=sum)
IndoreInc.Table=IndoreInc.Table[order(as.Date(IndoreInc.Table$Date,format="%d/%m/%Y")),]
rownames(IndoreInc.Table)=1:nrow(IndoreInc.Table)
colnames(IndoreInc.Table)=c("Date","Incidence")

Dates=seq(from=as.Date(as.character(IndoreInc.Table$Date[1]),"%d/%m/%Y"),to=as.Date(IndoreInc.Table$Date[nrow(IndoreInc.Table)],"%d/%m/%Y"),by="day")
IndoreInc.Table$Date=as.Date(IndoreInc.Table$Date,"%d/%m/%Y")

Zeros=data.frame(Date=as.Date(Dates),Incidence=rep(0,length(Dates)))
IndoreInc.Table$Date=as.character(IndoreInc.Table$Date)
Zeros$Date=as.character(Zeros$Date)

IndoreInc.Table=rbind(IndoreInc.Table,Zeros)
IndoreInc.Table=aggregate(IndoreInc.Table$Incidence,by=list(Date=IndoreInc.Table$Date),FUN=sum)
IndoreInc.Table=IndoreInc.Table[order(as.Date(IndoreInc.Table$Date)),]
IndoreInc.Table$Date=as.Date(IndoreInc.Table$Date)

library(EpiEstim)

R_Summary=estimate_R(IndoreInc.Table$x,method="parametric_si",config=list(t_start=2:42,t_end=5:45,n1=500,mean_si=3.96,std_si=4.75,n2=100,seed=1,mcmc_control=list(init.pars=NULL,burnin=10000,thin=1000,seed=1)))

pdf("R_Indore.pdf")
plot(R_Summary)
dev.off()

save.image("R_Indore.rda")