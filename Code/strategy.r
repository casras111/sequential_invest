# Predict best allocation of capital among stocks based on chosen Lclass history
# Optimize compound return product to find allocation vector that brings to maximum
# Does this for each of the methods: Kmeans/Kernel/RandomForest/Monkey
# Returns xvec with best allocation of capital proportion among n stocks

require(xts)
require(Rsolnp)

#load("DataWork/Classifier.Rdata")
load("DataWork/StockPrices.Rdata")

fopt <- function(x,r) {-sum(log(x%*%t(r)))}  #function for solnp optimization
eqfun <- function(x,r) {sum(x)}              #for equality optimization constraint
                                             #need to have same # of parameters
beststrat <- function(DDate,k,l,window_th=60) {
  DDate <- as.Date(DDate)
  #fix if date is not on trade day, bring to first trading day
  DDate <- index(first(StockPrices[paste0(DDate,"/")],"1 day"))
  DDateindx <- which(index(StockPrices)==DDate)
  options(warn=-1) #block warning for non-existent file
  try(load("DataWork/Classifier.Rdata"),silent=T)
  options(warn=0)
  if (!exists("Classifier")) {
    Kvec <- K_Histogram(K=k,DDate=DDate)
    Classifier <- Classification(KVec=Kvec,K=k,L=l,KKR=KKR) #group k-windows into classes using KKR method
  }
  if (last(Classifier$KVec) < (DDateindx-window_th)) {
    Kvec <- K_Histogram(K=k,DDate=DDate)
    Classifier <- Classification(KVec=Kvec,K=k,L=l,KKR=KKR) #group k-windows into classes using KKR method
  }
  
  #filter Classifier data to only look at history, no future peeking
  #HistClassifier <- Classifier[Classifier$KVec<DDateindx,] #relies on Kvec column name
  BestClass <- matchfunc(DDate,k,l,Classifier)
  ClassSegments <- Classifier[Classifier$Class==BestClass,1] #assume Class column name
  retcolnames <- grep("return",colnames(StockPrices))
  DDateReturns <- StockPrices[ClassSegments+k,retcolnames] #Returns on day k+1
  DDateReturns <- DDateReturns/100+1                       #price relatives in pct
  nstocks <- length(retcolnames)
  #use Rsolnp Lagrange general optimization package
  s <- solnp(rep(1/nstocks,nstocks),fopt, #start with equal weights
             eqfun=eqfun,eqB=1,           #sum weights equal to 1 constraint
             LB=rep(0,nstocks),           #all weights>0, long only lower bound constraint
             r=DDateReturns, control=list(trace=0))
  #xvec <- round(s$pars,digits=2)
  xvec <- s$pars
  return(xvec)
}

#for testing separately
# DDate <- as.Date("2011/2/1")
# k <- 20 #how many days to use for k parameter
# l <- 10 #number of different classes used in classification
# beststrat(DDate,k,l)
