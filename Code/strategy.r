# Predict best allocation of capital among stocks based on chosen Lclass history
# Optimize compound return product to find allocation vector that brings to maximum
# Calls K_Histogram to create kvec partitioning and then Classifier to classify
# Returns xvec with best allocation of capital proportion among n stocks

library(xts)
library(Rsolnp)

#load("DataWork/StockPrices.Rdata")

fopt <- function(x,r) {-sum(log(x%*%t(r)))}  #function for solnp optimization
eqfun <- function(x,r) {sum(x)}              #for equality optimization constraint
                                             #need to have same # of parameters
beststrat <- function(DDate,ksearch,l,window_th=10,recent_weight=FALSE) {
  retcolnames <- grep("return",colnames(StockPrices))
  retcolnames <- setdiff(retcolnames,which(colnames(StockPrices)=="Marketreturn"))
  nstocks <- length(retcolnames)
  DDate <- as.Date(DDate)
  print(DDate)
  #fix if date is not on trade day, bring to first trading day
  DDate <- index(first(StockPrices[paste0(DDate,"/")],"1 day"))
  DDateindx <- which(index(StockPrices)==DDate)
  maxret_vec <- rep(-1,length(ksearch)) #illegal returns for different k
  #create xvec matrix with illegal weight allocations,row for each k for averaging later
  xvec_k <- matrix(rep(0,nstocks*length(ksearch)),nrow=length(ksearch)) 
  for (i in seq_along(ksearch)) {
    if (exists("Classifier")) {rm(Classifier)}
    filename <- paste0("DataWork/Classifier",ksearch[i],".Rdata")
    if (file.exists(filename)) {
      load(filename)
    }
    #Classifier creates new clusters each time it is run
    #Run classifer using current date if file doesn't exist
    if (!exists("Classifier")) {
      #Kvec - split dates from DDate backward into k-windows
      #Parameter Holiday = default 0 removes more than 3 days of no trading, to set to 1 for all days
      Kvec <- K_Histogram(K=ksearch[i],DDate=DDate)
      #group k-windows into classes using KKR method
      Classifier <- Classification(KVec=Kvec,K=ksearch[i],L=l,KKR=KKR)
    }
    #recreate classifier if it's older than window_th days or
    #has data based on future dates
    if ((last(Classifier$KVec) < (DDateindx-window_th)) ||
        (last(Classifier$KVec) > (DDateindx-ksearch[i]))) {
      Kvec <- K_Histogram(K=ksearch[i],DDate=DDate)
      #group k-windows into classes using KKR method
      Classifier <- Classification(KVec=Kvec,K=ksearch[i],L=l,KKR=KKR)
    }
    BestClass <- matchfunc(DDate,ksearch[i],l,Classifier,recent_weight)
    ClassSegments <- Classifier[Classifier$Class==BestClass,1] #assume Class column name
    #Returns on day k+1 without last column that has market returns
    DDateReturns <- StockPrices[ClassSegments+ksearch[i],retcolnames]
    DDateReturns <- DDateReturns/100+1                       #price relatives in pct
    #use Rsolnp Lagrange general optimization package
    s <- solnp(rep(1/nstocks,nstocks),      #start with equal weights
               fopt,                        #function on which to optimize first parameter
               eqfun=eqfun,eqB=1,           #sum weights equal to 1 constraint
               LB=rep(0,nstocks),           #all weights>0, long only lower bound constraint
               r=DDateReturns, control=list(trace=0))
    maxret_vec[i] <- -last(s$values)   #maximum value found in min optimizer
    #normalize return for number of segments in class chosen
    maxret_vec[i] <- maxret_vec[i]/length(ClassSegments)
    #0.01 used to prevent calculation precision errors when comparing to 0
    if (maxret_vec[i]<(-0.01)) stop("solnp optimization gives negative return instead of cash")
    xvec_k[i,] <- s$pars               #optimal allocation found
  }
  print("maxret is: ");print(round(maxret_vec,3))
  ##### START for winner takes all strategy  ############
  # maxmax <- which.max(maxret_vec)
  # maxret_vec[maxmax] <- 1
  # maxret_vec[-maxmax] <-0
  # print("maxret is: ");print(round(maxret_vec,3))
  ##### END for winner takes all strategy    ############
  maxret_mat <- matrix(rep(maxret_vec,nstocks),nrow=length(ksearch),byrow=F)
  maxret_mat <- maxret_mat/(sum(maxret_vec))  #normalize weights to 1
  print(cbind(ksearch,round(maxret_mat[,1],3)))
  xvec <- colSums(xvec_k*maxret_mat)
  names(xvec) <- colnames(StockPrices)[retcolnames]
  #0.01 used to prevent calculation precision errors when comparing to 0
  if (abs(sum(xvec)-1)>0.01) stop("xvec calculation in beststrat does not sum to 1")
  return(xvec)
}

#for testing separately
# DDate <- as.Date("2011/2/1")
# k <- 20 #how many days to use for k parameter
# l <- 10 #number of different classes used in classification
# beststrat(DDate,k,l)
