options(stringsAsFactors=F)

Data.1=read.csv("raw_data1.csv")
Data.2=read.csv("raw_data2.csv")
Data.3=read.csv("raw_data3.csv")

Districts=readxl::read_xlsx("Districts.xlsx",col_names=FALSE)
Districts=Districts$...1

for(i in 1:length(Districts))
{
  District=Districts[i]
  
  District.1=Data.1[Data.1$Detected.District==District,3]
  District.2=Data.2[Data.2$Detected.District==District,3]
  
  District.3=Data.3[Data.3$Detected.District==District & Data.3$Current.Status=="Hospitalized",c(3,10)]
  
  if((length(District.1)+length(District.2)+nrow(District.3))>0)
  {
    DistrictInc.1=table(District.1)
    DistrictInc.2=table(District.2)
    
    DistrictInc.3=District.3$Num.Cases
    names(DistrictInc.3)=District.3$Date.Announced
    DistrictInc=c(DistrictInc.1,DistrictInc.2,DistrictInc.3)
    
    DistrictInc.Table=data.frame(names(DistrictInc),DistrictInc)
    DistrictInc.Table=aggregate(DistrictInc.Table$DistrictInc,by=list(Date=DistrictInc.Table$names.DistrictInc.),FUN=sum)
    DistrictInc.Table=DistrictInc.Table[order(as.Date(DistrictInc.Table$Date,format="%d/%m/%Y")),]
    rownames(DistrictInc.Table)=1:nrow(DistrictInc.Table)
    colnames(DistrictInc.Table)=c("Date","Incidence")
    
    Dates=seq(from=as.Date(as.character(DistrictInc.Table$Date[1]),"%d/%m/%Y"),to=as.Date(DistrictInc.Table$Date[nrow(DistrictInc.Table)],"%d/%m/%Y"),by="day")
    DistrictInc.Table$Date=as.Date(DistrictInc.Table$Date,"%d/%m/%Y")
    
    Zeros=data.frame(Date=as.Date(Dates),Incidence=rep(0,length(Dates)))
    DistrictInc.Table$Date=as.character(DistrictInc.Table$Date)
    Zeros$Date=as.character(Zeros$Date)
    
    DistrictInc.Table=rbind(DistrictInc.Table,Zeros)
    DistrictInc.Table=aggregate(DistrictInc.Table$Incidence,by=list(Date=DistrictInc.Table$Date),FUN=sum)
    DistrictInc.Table=DistrictInc.Table[order(as.Date(DistrictInc.Table$Date)),]
    DistrictInc.Table$Date=as.Date(DistrictInc.Table$Date)
    
    library(EpiEstim)
    
    Length=nrow(DistrictInc.Table)
    
    R_Summary=estimate_R(DistrictInc.Table$x,method="parametric_si",config=list(t_start=2:(Length-4),t_end=5:(Length-1),n1=500,mean_si=3.96,std_si=4.75,n2=100,seed=1,mcmc_control=list(init.pars=NULL,burnin=10000,thin=1000,seed=1)))
    
    pdf(paste0("R_",District,".pdf"))
    plot(R_Summary)
    dev.off()
    
    save.image(paste0("R_",District,".rda"))
  }
}