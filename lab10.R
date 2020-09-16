library(quantmod)
sss=stockSymbols()
dfr=data.frame(sss$Symbol,sss$Industry)
names(dfr)=c("ticker","industry")
television=subset(dfr,ticker=="DIS",)
tv_industry=as.vector(television$industry)
dfrtick=subset(dfr,industry==tv_industry,)
tvtickers=as.vector(dfrtick$ticker)
rundata=function(t){
  tryCatch(
    {
      fin=getFinancials(t,auto.assign = FALSE)
      sumOfCapital=0.10
      data=rep(0,6)
      data[1]=fin$IS$A["Operating Income",]
      data[2]=fin$BS$A["Total Common Shares Outstanding",]
      data[3]=fin$BS$A["Total Assets",]
      data[4]=fin$BS$A["Goodwill, Net",]
      data[5]=fin$BS$A["Total Liabilities",]
      data[6]=getQuote(t)$Last
      rap=data[3]+data[4]+data[5]
      rapPerShare=rap/data[2]
      EPV=data[1]/sumOfCapital
      EPVPerShare=EPV/data[2]
      price=data[6]
      data[is.na(data)]=0
      return(c(price,rapPerShare,EPVPerShare))
      
    },
    error=function(x){
      return(c(NA,NA,NA))
    }
  )
}
mm=mapply(rundata,tvtickers)
mm[is.na(mm)]=0
a=as.data.frame(table(tvtickers))
for(i in 1:sum(table(tvtickers))){
  if(mm[2,]>mm[3,]){
    return(mm[i]+"Above")
  }
  else{
    return(mm[i]+"below")
  }
}
resdf = data.frame(tvtickers,mm[1,],mm[2,],mm[3,])
names(resdf) = c("tick","price","rap","EPV")
head(resdf[order(-resdf$price),],7)
head(resdf[order(-resdf$rap),],7)
head(resdf[order(-resdf$EPV),],7)
resdf$rank = NA

