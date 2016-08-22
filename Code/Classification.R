##Classification
#group k-windows into classes using KKR method

Classification<-function (KVec ,K = 2 , L = 8 , KKR = "Monkey"){
#All available featuress :  K-means & Monkey
  CLASS<-c("K-means","Kernel","Randomforest","Monkey")
  load("DataWork/StockPrices.Rdata")
 
  #Part A Building the matrix:
  Columns2Save<-grep("return",colnames(StockPrices))
  StockPrices<-StockPrices[,Columns2Save] #saving only the returnes.
  Columns2Del<-
  IndexOut<-1:length(Kvec) #number of moving windows.
  
  listOfDataFrames<-list()
  for (i in IndexOut ){
    DataSet1<-c(t(StockPrices[Kvec[i]:(Kvec[i]+K-1),]))
    #DataSet1<-c(t(DataSet1))
    listOfDataFrames[[i]] <- data.frame(DataSet1)}
  
  DataSet <- do.call("cbind", listOfDataFrames)
  
 
  #Part B, cheaking the Classification:
  Flag<-which (CLASS == KKR)  #K-means, classifier, randomforest, Monkey ; c(1,2,3,4)
  
  if (Flag == 1){
    #for (o in 1:10){
    options(warn=-1)  #stop getting warnings in R about the "converge in 10 iterations"
    Class <- kmeans(t(DataSet[,1:i]),L , nstart = length(Kvec)*2)
    options(warn=0)   #keep getting warnings about problems
    Classifier<-cbind.data.frame(Kvec,Class=Class$cluster)
    ClassCenters<-Class$centers
    rownames(ClassCenters)<-paste("center",1:L,sep = "")
    save(ClassCenters,file = "DataWork/ClassCenters.Rdata")
    }#end if Flag == 1;
  
  if (Flag == 4){ #Monkey
    Num_Of_Windowdim<-length(Kvec)
    Class <-round(runif(Num_Of_Windowdim,1,L))
    Classifier<-cbind.data.frame(Kvec,Class)
   }
  

  save(Classifier,file="DataWork/Classifier.Rdata")

}