#Find which class classification best matches last k days of trading
#Builds on previously calculated Classifier dataframe for eack k-days segment
#and matches best class for k days ago starting from DDate day parameter

require(xts)

load("DataWork/Classifier.Rdata")
load("DataWork/StockPrices.Rdata")
retcolnames <- grep("return",colnames(StockPrices))

matchfunc <- function(DDate,k,l) {
  #fix if date is not on trade day, bring to first trading day
  DDate <- index(first(StockPrices[paste0(DDate,"/")],"1 day"))
  DDateindx <- which(index(StockPrices)==DDate)
  #k-days segment under test
  kRetMat <- StockPrices[(DDateindx-k):(DDateindx-1),retcolnames] #k window back
  
  #filter Classifier data to only look at history, no future peeking
  HistClassifier <- Classifier[Classifier$Kvec<DDateindx,] #relies on Kvec column name
  #Loop through each class and measure distance from k-days segment under test
  #MinClass will have the index of the best match class
  MinClass <- -1
  for (i in 1:l) {
    #class l segments,relies on Class column name
    ClassSegments <- HistClassifier[HistClassifier$Class==i,1]
    #illegal initial value to distinguish from incremental add later
    TotalDist <- -1
    for (j in ClassSegments) {
      RetMat <- StockPrices[j:(j+k-1),retcolnames]
      DistEuclid <- sum((coredata(RetMat) - coredata(kRetMat))^2)
      if (TotalDist < 0) {TotalDist <- DistEuclid} else {
        TotalDist <- TotalDist+DistEuclid
      }
    }
    if (i==1) {
      LowestDist <- TotalDist
      MinClass <- i } else {
        if ((TotalDist < LowestDist) && (TotalDist > 0)) {
          LowestDist <- TotalDist
          MinClass <- i
        }
      }
  }
  if (MinClass < 0) stop("Error matchfunc: no available class in data")
  return(MinClass)
}

#for testing separately
# DDate <- as.Date("2016/5/1")
# k <- 20 #how many days to use for k parameter
# l <- 10 #number of different classes used in classification
# matchfunc(DDate,k,l)
