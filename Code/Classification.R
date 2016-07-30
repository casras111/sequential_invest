#Classification
require("randomForest")
Classification<-function (KVec ,StockPrices,K = 2 , L , KKR = "Monkey"){
#  K-means, kernel, randomforest & Monkey
  CLASS<-c("K-means","Kernel","Randomforest","Monkey")
  load("DataWork/StockPrices.Rdata")
  #Part A Building the matrix
  K=K-1 # adjustment to time period
  Columns2Save<-c(1:11)*4 #saving only th3e returnes.
  StockPrices<-StockPrices[,Columns2Save]
  IndexOut<-1:(dim(StockPrices)[1] - K+1) #number of moving windows.
  
  for (i in IndexOut ){
    DataSet1<-StockPrices[i:(i+K-1),] 
  IndexIn<-1:dim(DataSet1)[2]  #SR
 
   for (j in IndexIn){
      if (j == 1){DataSet2<-DataSet1[,j]}
      else {DataSet2<-c(DataSet2,DataSet1[,j])} #Oz please pay attention
    }
  if (i==1) {DataSet<-DataSet2}
  else {DataSet<-cbind(DataSet,DataSet2)}
  }
  
  #Part B, cheaking the Classification
  
  Flag<-which (CLASS == KKR)
  
  if (Flag == 1){ #K-means ,classifier ,randomforest
    #for (o in 1:10){
    Trading_Day_Cluster <- kmeans(DataSet[,1:i],L , nstart = 20)
    #Trading_Day_Cluster$cluster
    #Trading_Day_Cluster$totss
    #Trading_Day_Cluster$size
    #Trading_Day_Cluster$withinss
        #if (o==1){ ABC<-Trading_Day_Cluster$totss}
    #else {ABC<-cbind(ABC,Trading_Day_Cluster$totss)}
    #}
    #plot(log(ABC),1:10)
    }#end if Flag == 1;
  
  
  
  if (Flag == 2){ #Kernel
    Num_Of_Windowdim<-dim(DataSet)[2]
    Trading_Day_Cluster <-round(runif(Num_Of_Windowdim,1,L))
  }  
  
  
  if (Flag == 3){ #RandomForest
    randomForest(,DataSet,cutoff = 1/L)
    Num_Of_Windowdim<-dim(DataSet)[2]
    Trading_Day_Cluster <-round(runif(Num_Of_Windowdim,1,L))
  }  
  
  
  
  if (Flag == 4){ #Monkey
  Num_Of_Windowdim<-dim(DataSet)[2]
  Trading_Day_Cluster <-runif(Num_Of_Windowdim,1,L)
  }
  
  
  
  
  
  
  Classifier<-cbind(KVec,Trading_Day_Cluster)
  save(Classifier,"Classifier.Rdata")