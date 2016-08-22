##1st function
#purpose : generating a whole datafarme set for trading.
SDB<-function(Regulgar = 0){  
COlNames<-c("Date","PNominal","Pclose","return","Open","Basis")
CompanyNames<-c("Teva","Africa","poalim",'Bezek',"cil","Delek","discountInvesment","leumi","migdal","partner","Mizrahi")
#we got problems in 
indecies<-c(5,3,6,4)

Stop<-c(1:length(CompanyNames))
for (i in Stop){
   FileName<-paste("DataRaw/",CompanyNames[i],".csv",sep = "")
   ReadindCSV<-read.csv(FileName,header = T,skip = 3,as.is = T,quote = "")[,1:6]
   COlumnNames<-paste("S",i,COlNames,sep = "")
   colnames(ReadindCSV)<-COlumnNames
   if (i==1){ DataRaw<-ReadindCSV[,c(1,indecies)]}
   else {DataRaw<-cbind.data.frame(DataRaw,ReadindCSV[,indecies])}
   
   if (Regulgar == 1 ){
   IndicesO<-grep("Open",names(ReadindCSV))     #index for open 
   IndicesC<-grep("close",names(ReadindCSV))    #index for close
   O2CR<-(ReadindCSV[,IndicesO]/ReadindCSV[,IndicesC]-1)*100
   if(i ==1){O2C<-cbind.data.frame(O2CR)}
   if (i!=1){O2C<-cbind.data.frame(O2C,O2CR)} }
}

#naming the col in O2C
if(Regulgar ==1){colnames(O2C)<-paste("O2CReurn",1:i,sep = "")}

#xts Format
DataRaw$S1Date<-strptime(DataRaw$S1Date,"%d/%m/%Y")
StockPrices<-xts(DataRaw[,-1],DataRaw$S1Date) 

#if we seek for open 2 close 
if (Regulgar == 1){DataRaw<-cbind.data.frame(DataRaw,O2C)
                   StockPrices<-xts(DataRaw[,-1],DataRaw$S1Date) }

#not the most necessary for code:

#special Vec for K-histogram
Date<-DataRaw$S1Date

rm(DataRaw,ReadindCSV,COlNames,FileName,CompanyNames,Stop,i,indecies,COlumnNames) #del unneccery data

#Saving Data
save(Date,file = "DataWork/Date.Rdata")
save(StockPrices,file = "DataWork/StockPrices.Rdata")

StockPrices<-StockPrices }

# ###################################
# #Validatae the return calculation #
# ###################################
# Validate<-DataRaw
# NAMES1<-grep("Ba",names(DataRaw))
# NAMES2<-grep("close",names(DataRaw))
# R0<-DataRaw[,grep("ret",names(DataRaw))]
# R1<-(Validate[,NAMES2]/Validate[,NAMES1]-1)*100
# ###############



