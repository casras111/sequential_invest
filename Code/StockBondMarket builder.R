##1st function
#purpose : generating a whole datafarme set for trading.
SDB<-function(Regular = 0){  
  
  #Market data:
  Market<-read.csv("DataRaw/MarketPortfolio.csv",header = T,as.is = T,skip = 3)
  Marketreturn<-Market[,4]/Market[,2]
  Marketreturn<-c((Marketreturn-1)*100)
  #Marketdata<-cbind(Marketreturn,Market[,1])
  #building dataframe:  
  COlNames<-c("Date","PNominal","Pclose","return","Open","Basis")
  CompanyNames<-c("Teva")#,"Africa","poalim",'Bezek',"cil","Delek","discountInvesment","leumi","migdal","partner","Mizrahi")
  indecies<-c(5,3,6,4)
  
  Stop<-c(1:length(CompanyNames))
  for (i in Stop){
    FileName<-paste("DataRaw/",CompanyNames[i],".csv",sep = "")
    ReadindCSV<-read.csv(FileName,header = T,skip = 3,as.is = T,quote = "")[,1:6]
    COlumnNames<-paste("S",i,COlNames,sep = "")
    colnames(ReadindCSV)<-COlumnNames
    if (i==1){ DataRaw<-ReadindCSV[,c(1,indecies)]}
    else {DataRaw<-cbind.data.frame(DataRaw,ReadindCSV[,indecies])}
    
    if (Regular == 1 ){
      IndicesO<-grep("Open",names(ReadindCSV))     #index for open 
      IndicesC<-grep("close",names(ReadindCSV))    #index for close
      O2CR<-(ReadindCSV[,IndicesC]/ReadindCSV[,IndicesO]-1)*100
      if(i ==1){O2C<-cbind.data.frame(O2CR)}
      if (i!=1){O2C<-cbind.data.frame(O2C,O2CR)} }
  }
  
  #naming the col in O2C
  if(Regular ==1){colnames(O2C)<-paste("O2Creturn",1:i,sep = "")}
  
  
  #generating a Bond
  bond<-rep(0,dim(DataRaw)[1])
  DataRaw<-cbind.data.frame(DataRaw,cashreturn = bond,Marketreturn)
  
  #xts Format
  DataRaw$S1Date<-strptime(DataRaw$S1Date,"%d/%m/%Y")
  
  StockPrices<-xts(DataRaw[,-1],DataRaw$S1Date) 
  
  #if we seek for open 2 close 
  if (Regular == 1){DataRaw<-cbind.data.frame(S1Date = DataRaw$S1Date,O2C,cashreturn = bond,Marketreturn)
  StockPrices<-xts(DataRaw[,-1],DataRaw$S1Date) }
  
  #not the most necessary for code:
  
  #special Vec for K-histogram
  Date<-as.Date(DataRaw$S1Date)
  
  rm(DataRaw,ReadindCSV,COlNames,FileName,CompanyNames,Stop,i,indecies,COlumnNames) #del unneccery data
  
  #Saving Data
  save(Date,file = "DataWork/Date.Rdata")
  save(StockPrices,file = "DataWork/StockPrices.Rdata")
  
  StockPrices<-StockPrices }


#StockPrices<-StockPrices[,grep("ret",names(StockPrices))]

# ###################################
# #Validatae the return calculation #
# ###################################
# Validate<-DataRaw
# NAMES1<-grep("Ba",names(DataRaw))
# NAMES2<-grep("close",names(DataRaw))
# R0<-DataRaw[,grep("ret",names(DataRaw))]
# R1<-(Validate[,NAMES2]/Validate[,NAMES1]-1)*100
# ###################################
