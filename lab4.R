library(ggplot2)
library(quantmod)
xz=function(x,i)
{
  fin = getFinancials(x,auto.assign = F)
  data = rep(0,12)
  
  data[1]=fin$IS$Q["Operating Income",i]
  data[2]=fin$BS$Q["Property/Plant/Equipment, Total - Gross",i]
  data[3]=fin$BS$Q["Accumulated Depreciation, Total",i]
  data[4]=fin$BS$Q["Total Current Assets",i]
  data[5]=fin$BS$Q["Total Current Liabilities",i]
  data[6]=fin$BS$Q["Total Common Shares Outstanding",i]
  data[7]=getQuote(x)$Last
  data[8]=fin$BS$Q["Total Debt",i]
  data[9]=fin$BS$Q["Cash & Equivalents",i]
  data[10]=fin$BS$Q["Preferred Stock - Non Redeemable, Net",i]
  data[11]=fin$BS$Q["Redeemable Preferred Stock, Total",i]
  data[12]=fin$BS$Q["Minority Interest",i]
  data[is.na(data)]=0
  
  roc = data[1]/(data[2]+data[3]+data[4]-data[5])
  ca = data[4]/data[5]
  ey = data[1]/((data[6]*data[7])+data[8]-data[9]-(data[4]-data[5])+data[10]+data[11]+data[12])
  mc=data[6]*data[7]
  return(c(roc,ca,ey,mc))
}


tickers=c("aapl","gpro","fb","ibm","tsla","ba","dri","nflx","twtr","sjm")
mm=mapply(xz,tickers,5)
rocf=rep(0,4)
caf=rep(0,4)
eyf=rep(0,4)
for(i in 1:4){
  mm=mapply(xz,tickers,i)
  TMC=sum(mm[4,])
  rocf[i]=sum(mm[1,]*(mm[4,]/TMC))
  caf[i]=sum(mm[2,]*(mm[4,]/TMC))
  eyf[i]=sum(mm[3,]*(mm[4,]/TMC))
}
ROC_df=data.frame(rocf)
CA_df=data.frame(caf)
EY_df=data.frame(eyf)
ggplot(ROC_df, aes(x=1:4, y=rocf))+geom_bar(stat="identity", fill="blue", color="red")
ggplot(CA_df,aes(x=1:4, y=caf))+geom_bar(stat="identity", fill="blue", color="red")
ggplot(EY_df,aes(x=1:4, y=eyf))+geom_bar(stat="identity", fill="blue", color="red")

