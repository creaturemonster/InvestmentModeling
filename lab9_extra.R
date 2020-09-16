library(quantmod)
library(ggplot2)
s=stockSymbols()
dfr=data.frame(s$Symbol,s$Industry)
names(dfr)=c("ticker","industry")
television=subset(dfr,ticker=="DIS",)
software=subset(dfr,ticker=="ORCL",)
food=subset(dfr,ticker=="PLKI",)
tv_industry=as.vector(television$industry)
soft_industry=as.vector(software$industry)
food_industry=as.vector(food$industry)
dfrtick=subset(dfr,industry==tv_industry,)
softtick=subset(dfr,industry==soft_industry)
foodtick=subset(dfr,industry==food_industry)
tvtickers=as.vector(dfrtick$ticker)
softtickers=as.vector(softtick$ticker)
foodtickers=as.vector(foodtick$ticker)
rundata=function(t){
  tryCatch({
    fin=getFinancials(t,auto.assign=F)
    labdata=rep(0,12)
    labdata[1]=fin$IS$A["Operating Income",1]
    labdata[2]=fin$BS$A["Property/Plant/Equipment, Total - Gross",1]
    labdata[3]=fin$BS$A["Accumulated Depreciation, Total",1]
    labdata[4]=fin$BS$A["Total Current Assets",1]
    labdata[5]=fin$BS$A["Total Current Liabilities",1]
    labdata[6]=fin$BS$A["Total Common Shares Outstanding",1]
    labdata[7]=getQuote(t)$Last
    labdata[8]=fin$BS$A["Total Debt",1]
    labdata[9]=fin$BS$A["Cash & Equivalents",1]
    labdata[10]=fin$BS$A["Preferred Stock - Non Redeemable, Net",1]
    labdata[11]=fin$BS$A["Redeemable Preferred Stock, Total",1]
    labdata[12]=fin$BS$A["Minority Interest",1]
    labdata[is.na(labdata)]=0
    run=rep(0:12)
    run[1]=fin$IS$A["Operating Income",4]
    run[2]=fin$BS$A["Property/Plant/Equipment, Total-Gross",4]
    run[3]=fin$BS$A["Accumulated Depreciation, Total",4]
    run[4]=fin$BS$A["Total Current Assets",4]
    run[5]=fin$BS$A["Total Current Liabilities",4]
    run[6]=fin$BS$A["Total Common Shares Outstanding",4]
    run[7]=getQuote(t)$Last
    run[8]=fin$BS$A["Total Debt",4]
    run[9]=fin$BS$A["Cash & Equivalents",4]
    run[10]=fin$BS$A["Preferred Stock - Non Redeemable, Net",4]
    run[11]=fin$BS$A["Redeemable Preferred Stock, Total",4]
    run[12]=fin$BS$A["Minority Interest",4]
    run[is.na(run)]=0
    roc_recent = labdata[1]/(labdata[2]+labdata[3]+labdata[4]-labdata[5])
    roc_last=run[1]/(run[2]+run[3]+run[4]-run[5])
    return(c(roc_recent, roc_last))
  }, 
  error=function(q)
  {
    return(rep(NA,4))
  })
}
operating=function(q){
  tryCatch(
    {
      fin=getFinancials(q,auto.assign = F)
      operating=fin$IS$A["Operating Income",]
      operating[is.na(operating)]=0
      return(operating)
    },
    error=function(q)
    {
      return(rep(NA,4))
    }
  )
  
}
foodop=mapply(operating,foodtickers)
softwareop=mapply(operating,softtickers)
tvop=mapply(operating,tvtickers)
food=mapply(rundata,foodtickers)
foodop[is.na(foodop)]=0
softwareop[is.na(softwareop)]=0
tvop[is.na(tvop)]=0
software=mapply(rundata,softtickers)
tv=mapply(rundata,tvtickers)
food[is.na(food)]=0
software[is.na(software)]=0
tv[is.na(tv)]=0
year1=sum(food[2,])
year4=sum(food[1,])
yr=rep(0,NROW(food))
yt=rep(0,NROW(software))
yy=rep(0,NROW(television))
for(i in 1:NROW(foodtickers)){
  yr[i]=sum(food[i,])
}
for(i in 1:NROW(tvtickers)){
  yt[i]=sum(tv[i,])
}
for(i in 1:NROW(softtickers)){
  yy[i]=sum(software[i,])
}
v1=c(year1)
v2=c(year4)
yoy=((v2-v1))/v1
gdf=data.frame(yoy,time=1:3)
  o=rep(0:NROW(food))
  c=rep(0:NROW(food))
  
  extra_food=function(q){
    tryCatch(
      {
      for(i in 1:NROW(food))
      {
        o[i]=getSymbols(q,src = "google")
        recent=q$q.Close["2016-11-16"]
        past=q$q.Close["2012-11-17"]
      }
      change=(recent-past)/past
      return(change)
    },
    error=function(q)
    {
      return(rep(NA,4))
    }
    )
  }
  mapply(extra_food,foodtickers)
  mapply(extra_food,tvtickers)
  mapply(extra_food,softtickers)
  
