library(quantmod)
library(ggplot2)
tick=c("dri","aapl","ibm","tsla","ba" ,"FB","GPRO", "NFLX","TWTR","SJM")
xz=function(x){
  Fin=getFinancials(x,auto.assign = F)
  OI=Fin$IS$Q["Operating Income",1]
  PPE=Fin$BS$Q["Property/Plant/Equipment, Total - Gross",1]+Fin$BS$Q["Accumulated Depreciation, Total",1]
  NWC=Fin$BS$Q["Total Current Assets",1]-Fin$BS$Q["Total Current Liabilities",1]
  CA=Fin$BS$Q["Total Current Assets",1]
  CL=Fin$BS$Q["Total Current Liabilities",1]
  CR=CA/CL
  price=getQuote(x)$Last
  MC=Fin$BS$Q["Total Common Shares Outstanding",1]*price
  TD=Fin$BS$Q["Total Debt",1]
  WC = CA - CL
  EC = Fin$BS$Q["Cash & Equivalents",1]+WC
  EC[is.na(EC)]=0
  RPS = Fin$BS$Q["Redeemable Preferred Stock, Total",1]
  NRPS = Fin$BS$Q["Preferred Stock - Non Redeemable, Net",1]
  PS = RPS + NRPS
  PS[is.na(PS)]=0
  MI = Fin$BS$Q["Minority Interest",1]
  MI[is.na(MI)]=0  
  ROC = OI/(PPE +CA - CL)
  EV = OI/(MC+TD-EC+PS+MI-WC)
  CR = CA/CL
  return(c(ROC,EV,CR))
  
}
mapply(xz,tick)
