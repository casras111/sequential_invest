# Predict best allocation of capital among stocks based on chosen Lclass history
# Maximum likelihood to find allocation vector that brings to maximum compounded return
# Does this for each of the methods: Kmeans/Kernel/RandomForest/Monkey
# Returns xvec with best allocation of capital proportion among n stocks

require(xts)

load("DataWork/Classifier.Rdata")
load("DataWork/StockPrices.Rdata")

beststrat <- function(DDate,k,l) {
  BestClass <- matchfunc(DDate,k,l)
  ClassSegments <- Classifier[Classifier$Class==BestClass,1]
  retcolnames <- grep("return",colnames(StockPrices))
  DDateReturns <- StockPrices[ClassSegments+k,retcolnames] #TBD to verify k or k-1
  nstocks <- length(retcolnames)
  n <- dim(StockPrices)[1]
  #Monkey randomize stock allocation
  xvec <- runif(nstocks)
  #make sure it adds up to 1
  xvec <- xvec/sum(xvec)
  return(xvec)
}

#for testing separately
# DDate <- as.Date("2016/5/1")
# k <- 20 #how many days to use for k parameter
# l <- 10 #number of different classes used in classification
# beststrat(DDate,k,l)
