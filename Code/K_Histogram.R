#K_Histogram 

K_Histogram<-function(K = 20 ,DDate = 20160101) {

  #converting 

  #if (StockData == 0) { print('Please instert Data')}
  load("DataWork/StockPrices.Rdata")
  load("DataWork/Date.Rdata")
  
  StockData<-StockPrices
  #K<-20
  #DDate<-strptime("01/07/2014","%d/%m/%Y")
  #if (DDate>"20160725"){print ("error in DDate!! last Date is on 20160725")}
  #DDate<-20160101
   Timeperiod<-paste("20060101/",DDate,sep = "")
 
    StockData<-StockData[Timeperiod,]
    Date<-sort(Date,decreasing = F)
    Date<-Date[1:dim(StockData)[1]]
    
  
  #Temp<-1:(length(Date)-K)/K
  LenDate<-1:(length(Date)-K+1)
  Kvec<-0
  for( i in LenDate){
    DateFlage<-Date[i:(i+K-1)]
    Flag<-sum(DateFlage[1:(K-1)]+24*60*60*3>=DateFlage[2:K],na.rm = T)
    if (Flag ==K-1) {Kvec<-c(Kvec,i)}
  }    
  Kvec<-Kvec[-1]
  
}


#SA<-(which(Date==strptime("02/07/2016","%d/%m/%Y")))
#SA<-(which(Date==strptime("02/07/2016","%d/%m/%Y")+24*60*60))
#SA<-(which(Date==strptime("02/07/2016","%d/%m/%Y")+24*60*60*2))
#SA<-min(SA,na.rm = T)
#Date<-Date[-(1:SA)]
#Cut<-i+which(Flag!=1)-1
#CutDate<-min(i+19,Cut)
#Continue<-min(i+19,max(Cut))
#ProblomaticDates<-append(ProblomaticDates,CutDate:Continue)
#if( max(Cut)==i+19)
#i<-i+1
#TotalDateManipulation<-unique(c(1:CutDate,Continue:lenght(Date)))  
#if (sum(Flag<(K-1))) {indecies<-append(indecies,unique(CutDate:Continue))
#                      Data<-Date[TotalDateManipulation] }
#}
#row.names(StockData)
