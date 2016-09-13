##Classification
#group k-windows into classes using KKR method

library("cluster")
#library("randomForest")

Classification<-function (KVec ,K = 20 , L = 8 , KKR = "Monkey",optimize = 0){

  
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
  
  #############################
  ## optimize clara / elbow  ##
  #############################
  
  ClaraOptimzation<-function(Critical_Value = 0.1){
    paste0(timestamp(),"Start Opimization")
    #Critical_Value is the decision point of improvements to 
    #set the number of K in el for opimize the classification.
    set.seed(176)
    n_centers = 1:20  #numcer of center we shell check. 
    Cost_elbow<-sapply(n_centers,function(x) clara(t(DataSet),x,samples=500
                        ,sampsize = nrow(t(DataSet)),metric="manhattan",pamLike=T)$objective)
    #plot(n_centers,log(Cost_elbow))
    A<-sqrt((Cost_elbow[-20]-Cost_elbow[-1])^2)  #calculating absolute distance between Costs
    B<-abs((A[-1]-A[-19])/A[-1])   #calculating the precentage of accuracy 
    Opitmum_Of_Clara<-min(which(B<Critical_Value))+2  # Find the first num which obey the restriction
    #plot(abs(B))                                    # have to add 2 for matcing the indices.
    paste0(timestamp(),"end optinization")
    return(Opitmum_Of_Clara)
  }
  
  if(optimize==1) L<-ClaraOptimzation()
  
  ####################
  # End Of optimize  #
  ####################
  
  #Flag <- which (CLASS == KKR)  #K-means, classifier, randomforest, Monkey ; c(1,2,3,4)
  Flag <- switch(KKR, "K-means"=1, "Clara"=2, "Kernel"=3, "Randomforest"=4, "Monkey"=0)
  
  if (Flag == 1){
    set.seed(123)
    Class <- kmeans(t(DataSet[,IndexOut]),L , nstart = L,iter.max = 20 )#
    Classifier<-cbind.data.frame(KVec,Class=Class$cluster)
    ClassCenters<-Class$centers
    rownames(ClassCenters) <- paste("center",1:L,sep = "")
    save(ClassCenters,file = "DataWork/ClassCenters.Rdata")
  }#end if Flag == 1;
  
  
  # Correlations_clastering<-function(){
  #   Windows<-1:(length(KVec)-K) #number of moving windows.
  #   
  #   CorrData<-sapply(Windows, function(x){
  #                   Correlation<-cor(StockPrices[x:(x+K-1),1:13])
  #                   CorrelationIndices<-lower.tri(Correlation, diag = FALSE)
  #            לבדוק את הנושא לעומק לפני שימוש במטריצה זו       UsefulCorrelations<-c(Correlation[CorrelationIndices])
  #                   return(UsefulCorrelations)
  #                   })
  #     
  #   ###########need to rm the cash from stick data.
  # }
  # 
  
  
  
  
  
  
  
  
  if (Flag == 2) {
    set.seed(176)
    Class <- clara(t(DataSet),L,samples=500,sampsize = nrow(t(DataSet)),metric="manhattan",pamLike=T)
    Classifier<-cbind.data.frame(KVec,Class=Class$clustering)
    #hist(Classifier$Class)
    ClassCenters<-Class$medoids
    rownames(ClassCenters)<-paste("center",1:L,sep = "")
    
    colnames(ClassCenters)<-paste("Day",1:ncol(ClassCenters),sep = "")
    centers_file_name<-paste0("DataWork/ClassCenters",K,".Rdata")
    save(ClassCenters,file = centers_file_name)
  }
  
  if (Flag == 0){ #Monkey
    Num_Of_Windowdim<-length(KVec)
    Class <-round(runif(Num_Of_Windowdim,1,L))
    Classifier<-cbind.data.frame(KVec,Class)
   }
  
  print(paste0("Classifier working, k is: ",K))
  filename <- paste0("DataWork/Classifier",K,".Rdata")
  save(Classifier,file=filename)
  
  return(Classifier)

}