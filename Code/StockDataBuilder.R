require(xts)
#setwd("C:/Users/Oz/Desktop/study/courses/physicsProject/sequential_invest/Code")
#setwd("../")
#setwd("~/DataRaw/")
COlNames<-c("Date","Pclose","PNominal","return","Open","Basis")
CompanyNames<-c("Teva","Africa",'Bezek',"cil","Delek","discountInvesment","leumi","migdal","poalim","partner","Mizrahi")
indecies<-c(5,2,6,4)

Stop<-c(1:11)
for (i in Stop){
   FileName<-paste("DataRaw/",CompanyNames[i],".csv",sep = "")
   ReadindCSV<-read.csv(FileName,header = T,skip = 3,as.is = T,quote = "")[,1:6]
   COlumnNames<-paste("S",i,COlNames,sep = "")
   colnames(ReadindCSV)<-COlumnNames
   if (i==1){ DataRaw<-ReadindCSV[,c(1,indecies)]}
   else {DataRaw<-cbind.data.frame(DataRaw,ReadindCSV[,indecies])}
}

DataRaw$S1Date<-strptime(DataRaw$S1Date,"%d/%m/%Y")

StockPrices<-xts(DataRaw[,-1],DataRaw$S1Date) #xts Format

rm(DataRaw,ReadindCSV,COlNames,FileName,CompanyNames,Stop,i,indecies,COlumnNames) #del unneccery data

save(StockPrices,file = "DataWork/StockPrices.Rdata")

#At statrt I cheached if we have mismatch in dates.
# sum(A1$תאריך == A2$תאריך)
# sum(A1$תאריך == A3$תאריך)
# sum(A1$תאריך == A4$תאריך)
# sum(A1$תאריך == A5$תאריך)
# sum(A1$תאריך == A6$תאריך)
# sum(A1$תאריך == A7$תאריך)
# sum(A1$תאריך == A8$תאריך)
# sum(A1$תאריך == A8$תאריך)
# sum(A1$תאריך == A9$תאריך) #A9 contains dates from 2013 - perigo company
# sum(A1$תאריך == A10$תאריך) #we short in time so i drop perigo.
# sum(A1$תאריך == A11$תאריך)
# sum(A1$תאריך == A12$תאריך)



