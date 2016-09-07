##Classification
#group k-windows into classes using KKR method
#library("raster")
library("cluster")
#library("randomForest")
Classification<-function (KVec ,K = 20 , L = 8 , KKR = "Monkey"){

  
  #All available featuress :  K-means & Monkey
  CLASS<-c("K-means","Clara","Kernel","Randomforest","Monkey")
  load("DataWork/StockPrices.Rdata")
 
  #Part A Building the matrix:
  Columns2Save<-grep("return",colnames(StockPrices))
  StockPrices<-StockPrices[,Columns2Save] #saving only the returnes.
  IndexOut<-1:length(KVec) #number of moving windows.
  
  listOfDataFrames<-list()
  for (i in IndexOut ){
    DataSet1<-c(t(StockPrices[KVec[i]:(KVec[i]+K-1),]))
    #DataSet1<-c(t(DataSet1))
    listOfDataFrames[[i]] <- data.frame(DataSet1)
  }
  
  DataSet <- do.call("cbind", listOfDataFrames)
  
 
  #Part B, cheaking the Classification:
  Flag <- which (CLASS == KKR)  #K-means, classifier, randomforest, Monkey ; c(1,2,3,4)
  
  if (Flag == 1){
    #for (o in 1:10){
    #options(warn=-1)  #stop getting warnings in R about the "converge in 10 iterations"
    set.seed(123)
    #Class <- kmeans(t(DataSet[,IndexOut]),L , nstart = L,iter.max = 20 )#
    Class <- kmeans(t(DataSet[,IndexOut]),L , nstart = L,iter.max = 20 )#
    #Chanig to matrix!!!!!!!!!!!!!!!!!!ss
    #options(warn=0)   #keep getting warnings about problems
    Classifier<-cbind.data.frame(KVec,Class=Class$cluster)
    ClassCenters<-Class$centers
    rownames(ClassCenters) <- paste("center",1:L,sep = "")
    save(ClassCenters,file = "DataWork/ClassCenters.Rdata")
  }#end if Flag == 1;
  
  
  
  if (Flag == 2) {
    set.seed(176)
    #Class <- svm (DataSet~DataSet[,1],type = "C-classification",data = DataSet,gamma = 0.1)
    Class <- clara(t(DataSet),L,samples=500,sampsize = nrow(t(DataSet)),metric="manhattan",pamLike=T)
    Classifier<-cbind.data.frame(KVec,Class=Class$clustering)
    #hist(Classifier$Class)
    ClassCenters<-Class$medoids
    rownames(ClassCenters)<-paste("center",1:L,sep = "")
    colnames(ClassCenters)<-paste("Day",1:ncol(ClassCenters),sep = "")
    save(ClassCenters,file = "DataWork/ClassCenters.Rdata")
  }
  
  if (Flag == 4){ #Monkey
    Num_Of_Windowdim<-length(KVec)
    Class <-round(runif(Num_Of_Windowdim,1,L))
    Classifier<-cbind.data.frame(KVec,Class)
   }
  
  print(paste0("Classifier working, k is: ",K))
  filename <- paste0("DataWork/Classifier",K,".Rdata")
  save(Classifier,file=filename)
  return(Classifier)

}