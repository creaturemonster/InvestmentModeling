tick=c("aapl","gpro","fb","ibm","tsla","ba","dri","nflx","twtr","sjm")
rundata=function(t,q){
  
  fin=getFinancials(t,auto.assign=F)
  labdata=rep(0,10)
  labdata[1]=fin$IS$Q["Operating Income",q]
  labdata[2]=fin$BS$Q["Total Current Assets",q]
  labdata[3]=fin$BS$Q["Total Current Liabilities",q]
  labdata[4]=fin$BS$Q["Total Common Shares Outstanding",q]
  labdata[5]=getQuote(t)$Last
  labdata[6]=fin$BS$Q["Total Debt",q]
  labdata[7]=fin$BS$Q["Cash & Equivalents",q]
  labdata[8]=fin$BS$Q["Preferred Stock - Non Redeemable, Net",q]
  labdata[9]=fin$BS$Q["Redeemable Preferred Stock, Total",q]
  labdata[10]=fin$BS$Q["Minority Interest",q]
  
  labdata[is.na(labdata)]=0
TEV=labdata[4]*labdata[5]+labdata[6]-labdata[7]-(labdata[2]-labdata[3])+(labdata[8]+labdata[9])+labdata[10]
  return(c(labdata[1],TEV))

}
h=rundata("AAPL",1)
v=rundata("AAPL",5)
(h-v)/v
h=mapply(rundata,tick,1)
u=mapply(rundata,tick,5)
(h-u)/u


sss = stockSymbols()
names(sss)
df = data.frame(sss$Symbol,sss$Sector,sss$Industry)
driSector = as.vector(subset(df,sss.Symbol=="DRI",sss.Sector)$sss.Sector)
driIndustry = as.vector(subset(df,sss.Symbol=="DRI",sss.Industry)$sss.Industry)

df2 = subset(df, sss.Industry == driIndustry, )

alltick=as.vector(df2$sss.Symbol)
#alltick = as.vector(df2[,1])
alltick
df2
mapply(rundata,alltick,1)
