#Classification

Classification<-function (KVec ,StockPrices,K , L , KKR = "K-means"){
#  K-means, kernel, randomforest
  CLASS<-c("K-means","Kernel","Randomforest")

  #Part A Building the matrix
  IndexOut<-1:(dim(StockPrices)[1] - K+1)
  
  for (i in IndexOut ){
    DataSet1<-StockPrices[i:(i+K-1),] 
  IndexIn<-1:dim(DataSet1)[2] 
 
   for (j in IndexIn){
      if (j == 1){DataSet2<-DataSet1[,j]}
      else {DataSet2<-rbind(DataSet2,DataSet1[,j])}
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
  
  Classifier<-cbind(KVec,Trading_Day_Cluster)
  
  
  
}