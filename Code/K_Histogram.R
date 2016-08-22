#K_Histogram 

K_Histogram<-function(K = 20 ,DDate = 20160101, Holiday = 0) {

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
    
    LenDate<-1:(length(Date)-K)
    
  if (Holiday == 0){  #Del Holidays from obs
  Kvec<-0
  for( i in LenDate){
    DateFlage<-Date[i:(i+K-1)]
    Flag<-sum(DateFlage[1:(K-1)]+3>=DateFlage[2:K],na.rm = T)
    if (Flag ==K-1) {Kvec<-c(Kvec,i)}
    }    
  Kvec<-Kvec[-1] } #K-histogram without holidays
  
  if (Holiday == 1){ Kvec<-LenDate }  
    return(Kvec)
    
}


