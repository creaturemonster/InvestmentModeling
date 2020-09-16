library(quantmod)
library(ggplot2)
sss=stockSymbols()
dataf=data.frame(sss$Symbol)
df = data.frame(sss$Symbol,sss$Sector,sss$Industry)
driSector = as.vector(subset(df,sss.Symbol=="DRI",sss.Sector)$sss.Sector)
driIndustry = as.vector(subset(df,sss.Symbol=="DRI",sss.Industry)$sss.Industry)
df2 = subset(df, sss.Industry == driIndustry,)
alltick=as.vector(df2$sss.Symbol)
alltick = as.vector(df2[,1])
tickers=data.frame(alltick)
part1=function(t,q){
  tryCatch(
    {
      fin=getFinancials(t,auto.assign=F)
      labdata=rep(0,15)
      labdata[1]=fin$IS$Q["Operating Income",q]
      labdata[2]=fin$BS$Q["Property/Plant/Equipment, Total - Gross",q]
      labdata[3]=fin$BS$Q["Accumulated Depreciation, Total",q]
      labdata[4]=fin$BS$Q["Total Current Assets",q]
      labdata[5]=fin$BS$Q["Total Current Liabilities",q]
      labdata[6]=fin$BS$Q["Total Common Shares Outstanding",q]
      labdata[7]=getQuote(t)$Last
      labdata[8]=fin$BS$Q["Total Debt",q]
      labdata[9]=fin$BS$Q["Cash & Equivalents",q]
      labdata[10]=fin$BS$Q["Preferred Stock - Non Redeemable, Net",q]
      labdata[11]=fin$BS$Q["Redeemable Preferred Stock, Total",q]
      labdata[12]=fin$BS$Q["Minority Interest",q]
      labdata[13]=fin$BS$Q["Total Assets",q]
      labdata[14]=fin$BS$Q["Total Liabilities",q]
      labdata[15]=labdata[6]*labdata[7]
      labdata[is.na(labdata)]=0
      bv=(labdata[13]-labdata[14])
      part1=((labdata[4]-labdata[5])/labdata[15])
      mv=labdata[6]*labdata[7]
      part2=bv/(labdata[10]+labdata[11]+labdata[15]+labdata[12])

      return(c(part1))
    },
    error=function(x){
      return(c(NA))
    }
  )
}
mn=mapply(part1,alltick,1:4)
mn
part2=function(t,q){
         tryCatch(
           {
             fin=getFinancials(t,auto.assign=F)
             labdata=rep(0,15)
             labdata[1]=fin$IS$Q["Operating Income",q]
             labdata[2]=fin$BS$Q["Property/Plant/Equipment, Total - Gross",q]
             labdata[3]=fin$BS$Q["Accumulated Depreciation, Total",q]
             labdata[4]=fin$BS$Q["Total Current Assets",q]
             labdata[5]=fin$BS$Q["Total Current Liabilities",q]
             labdata[6]=fin$BS$Q["Total Common Shares Outstanding",q]
             labdata[7]=getQuote(t)$Last
             labdata[8]=fin$BS$Q["Total Debt",1]
             labdata[9]=fin$BS$Q["Cash & Equivalents",q]
             labdata[10]=fin$BS$Q["Preferred Stock - Non Redeemable, Net",q]
             labdata[11]=fin$BS$Q["Redeemable Preferred Stock, Total",q]
             labdata[12]=fin$BS$Q["Minority Interest",q]
             labdata[13]=fin$BS$Q["Total Assets",q]
             labdata[14]=fin$BS$Q["Total Liabilities",q]
             labdata[15]=labdata[6]*labdata[7]
             labdata[is.na(labdata)]=0
             bv=(labdata[13]-labdata[14])
             part1=((labdata[4]-labdata[5])/labdata[15])
             mv=labdata[6]*labdata[7]
             part2=bv/(labdata[10]+labdata[11]+labdata[15]+labdata[12])
             
             return(c(part2))
           },
           error=function(x){
             return(c(NA))
           }
         )
}
mm=mapply(part2,alltick,1:4)
mm
fin=getFinancials(alltick,auto.assign = F)
labdata=rep(0,5)
labdata[1]=fin$IS$A["Operating Income",1]
labdata[2]=fin$IS$A["Operating Income",2]
labdata[3]=fin$IS$A["Operating Income",3]
labdata[4]=fin$IS$A["Operating Income",4]
labdata[5]=fin$IS$A["Operating Income",5]
change = rep(0,5)
change[1] = ((labdata[1] - labdata[2])/labdata[2]) * 100
change[2] = ((labdata[2] - labdata[3])/labdata[3]) * 100
change[3] = ((labdata[3] - labdata[4])/labdata[4]) * 100
change[4] = ((labdata[4] - labdata[5])/labdata[5]) * 100
df5=data.frame(change)
ggplot(df5, aes(x=1:5,y=change)) + geom_bar(stat="identity",fill="orange",color="pink")


