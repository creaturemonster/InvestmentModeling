library(quantmod)
mm=data.frame(matrix(c(7,1,4,10,2,3,6,8,1,6,8,4,4,10,1,4,5,3,1,1,1,6,7,7,9,8,3,1,2,2),ncol=10,nrow=3))
df=data.frame(matrix(c(mm[1,],mm[2,]),ncol=2,nrow=3))
sum(mm[,3])
sum(mm)
even= seq_len(ncol(mm))%%2
sum(mm[,even])
sss=stockSymbols()
df = data.frame(sss$Symbol,sss$Industry)
dfsub = subset(df,df$sss.Symbol=="INTC",sss.Industry)
industry = as.vector(dfsub$sss.Industry)
dftick = subset(df,sss.Industry==industry,sss.Symbol)
dfcapLower=subset(df,dfsub$sss.MarketCap>=10000000,sss.Symbol)
dfcapUpper=subset(df,dfsub$sss.MarketCap<=50000000,sss.Symbol)
tick = as.vector(dftick$sss.Symbol)
df_tick=data.frame(dftick)
tick1 = c("DRI", "IBM")
runData=function(t)
{
  tryCatch(
    {
      xx=getFinancials(t,auto.assign = FALSE)
      oi = xx$IS$Q["Operating Income",1]
      is.na(oi)=0
      return(c(nodata))
    },
    error=function(){
      return(NA)
    }
  )
 
}
m1 = mapply(runData,tick)
m1

xx=getFinancials(tick1,auto.assign = F)
oi = xx$IS$Q["Operating Income",1]
labData=function(t)
{
  tryCatch(
    {
      xx=getFinancials(t,auto.assign = FALSE)
      oi = xx$IS$A["Operating Income",4]
      is.na(oi)=0
      return(c(oi))
    },
    error=function(){
      return(c(NA))
    }
  )
  
}
mapply(labdata,tick1)
Data=function(t,q)
{
  tryCatch(
    {
      xx=getFinancials(t,auto.assign = FALSE)
      oi = xx$IS$Q["Operating Income",q]
      ca=xx$BS$Q["Total Current Assets",q]
      is.na(oi)=0
      return(c(oi,ca))
    },
    error=function(){
      return(c(NA,NA))
    }
  )
  
}
mapply(Data,tick1,1:4)
mat=matrix(c(3,9,12,3,2,4,5,63,4,3,6,1),ncol=4,nrow=3)
nodata=function(t){
  tryCatch(
    {
      fin=getFinancials(t,auto.assign = FALSE)
      oi=fin$IS$Q["Operating Income",]
      ca=fin$BS$Q["Total Current Assets",]
      cl=fin$BS$Q["Total Current Liabilities",]
      cr=ca/cl
      is.na(oi)=0
      if(cr==0){
        return(c(t))
      }
      else{
        return(c(oi,cr))
      }
     
    }
  )
}
mapply(nodata,tick)
