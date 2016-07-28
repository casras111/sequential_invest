#Find which class classification best matches last k days of trading
#Builds on previously calculated Classifier dataframe for eack k-days segment
#and matches best class for k days ago starting from DDate day parameter

require(xts)

load("DataWork/Classifier.Rdata")
load("DataWork/StockPrices.Rdata")
retcolnames <- grep("return",colnames(StockPrices))

matchfunc <- function(DDate,k,l) {
  #k-days segment under test
  #period <- paste0(DDate,"/",DDate+k)
  DDateindx <- which(as.Date(index(StockPrices))==DDate)
  kRetMat <- StockPrices[DDateindx:(DDateindx+k),retcolnames]
  
  #Loop through each class and measure distance from k-days segment under test
  #MinClass will have the index of the best match class
  for (i in 1:l) {
    ClassSegments <- Classifier[Classifier$Class==i,1]
    #illegal initial value to distinguish from incremental add later
    TotalDist <- -1
    for (j in ClassSegments) {
      RetMat <- StockPrices[j:(j+k),retcolnames]
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
  return(MinClass)
}

DDate <- as.Date("2016/5/1")
k <- 20 #how many days to use for k parameter
l <- 10 #number of different classes used in classification

matchfunc(DDate,k,l)