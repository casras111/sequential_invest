#Classification

Classification<-function (KVec ,K = 2 , L = 8 , KKR = "Monkey"){
  #  K-means, kernel, randomforest & Monkey
  CLASS<-c("K-means","Kernel","Randomforest","Monkey")
  load("DataWork/StockPrices.Rdata")
 
  #Part A Building the matrix
  #K=K-1 # adjustment to time period
  Columns2Save<-grep("return",colnames(StockPrices))
  #Columns2Save<-c(1:11)*4 
  StockPrices<-StockPrices[,Columns2Save] #saving only th3e returnes.
  IndexOut<-1:length(Kvec) #number of moving windows.
  
  listOfDataFrames<-list()
  for (i in IndexOut ){
    DataSet1<-c(t(StockPrices[Kvec[i]:(Kvec[i]+K-1),]))
    #DataSet1<-c(t(DataSet1))
    listOfDataFrames[[i]] <- data.frame(DataSet1)}
  
  DataSet <- do.call("cbind", listOfDataFrames)
  
 
  #Part B, cheaking the Classification
  
  Flag<-which (CLASS == KKR)
  
  if (Flag == 1){ #K-means ,classifier ,randomforest
    #for (o in 1:10){
    options(warn=-1)  #stop getting warnings in R about the "converge in 10 iterations"
    Class <- kmeans(t(DataSet[,1:i]),L , nstart = length(Kvec)*2)
    options(warn=0)   #keep getting warnings about problems
    #Class$cluster
    #Class$totss
    #Class$size
    #Class$withinss
    #if (o==1){ ABC<-Class$totss}
    #else {ABC<-cbind(ABC,Class$totss)}
    #}
    #plot(log(ABC),1:10)
    Classifier<-cbind.data.frame(Kvec,Class=Class$cluster)
    ClassCenters<-Class$centers
    rownames(ClassCenters)<-paste("center",1:L,sep = "")
    save(ClassCenters,file = "DataWork/ClassCenters.Rdata")
  }#end if Flag == 1;
  
  
  
  if (Flag == 2){ #Kernel
    Num_Of_Windowdim<-dim(DataSet)[2]
    Class <-round(runif(Num_Of_Windowdim,1,L))
  }  
  
  
  # if (Flag == 3){ #RandomForest
  #   randomForest(,DataSet,cutoff = 1/L)
  #   Num_Of_Windowdim<-dim(DataSet)[2]
  #   Class <-round(runif(Num_Of_Windowdim,1,L))
  # }  
  
  
  
  if (Flag == 4){ #Monkey
    Num_Of_Windowdim<-length(Kvec)
    Class <-round(runif(Num_Of_Windowdim,1,L))
    Classifier<-cbind.data.frame(Kvec,Class)
  }
  

  save(Classifier,file="DataWork/Classifier.Rdata")

}