# Predict best allocation of capital among stocks based on chosen Lclass history
# Optimize compound return product to find allocation vector that brings to maximum
# Does this for each of the methods: Kmeans/Kernel/RandomForest/Monkey
# Returns xvec with best allocation of capital proportion among n stocks

require(xts)
require(Rsolnp)

load("DataWork/Classifier.Rdata")
load("DataWork/StockPrices.Rdata")

fopt <- function(x,r) {-sum(log(x%*%t(r)))}  #function for solnp optimization
eqfun <- function(x,r) {sum(x)}              #for equality optimization constraint
                                             #need to have same # of parameters
beststrat <- function(DDate,k,l) {
  DDate <- as.Date(DDate)
  BestClass <- matchfunc(DDate,k,l)
  ClassSegments <- Classifier[Classifier$Class==BestClass,1] #assume Class column name
  retcolnames <- grep("return",colnames(StockPrices))
  DDateReturns <- StockPrices[ClassSegments+k,retcolnames] #Returns on day k+1
  DDateReturns <- DDateReturns/100+1                       #price relatives in pct
  nstocks <- length(retcolnames)
  #use Rsolnp Lagrange general optimization package
  s <- solnp(rep(1/nstocks,nstocks),fopt, #start with equal weights
             eqfun=eqfun,eqB=1,           #sum weights equal to 0 constraint
             LB=rep(0,nstocks),           #all weights>0, long only lower bound constraint
             r=DDateReturns, control=list(trace=0))
  xvec <- round(s$pars,digits=2)
  return(xvec)
}

#for testing separately
# DDate <- as.Date("2011/2/1")
# k <- 20 #how many days to use for k parameter
# l <- 10 #number of different classes used in classification
# beststrat(DDate,k,l)
