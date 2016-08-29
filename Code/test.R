#main - calls all other sequential invest modules
file.remove("DataWork/Classifier.Rdata")
#packages
require(xts)
require(randomForest)

set.seed(123) #for Monkey strategy reproducibility
if (basename(getwd())=="Code") setwd(normalizePath("..")) #for knitr workaround
#knitr::opts_knit$set(root.dir="..")

#source all modules and execute before trading
#builds StockPrices dataframe
source("Code/StockDataBuilder.R",echo=TRUE,max.deparse.length = 10000)
#splits StockPrices to K-period days and returns KVec index vector
source("Code/K_Histogram.R",echo=TRUE,max.deparse.length = 10000)           
#classifer of trading days groups
source("Code/Classification.R",echo=TRUE,max.deparse.length = 10000)        

#SDB StockDataBuilder function, imports data
#default basis to close returns, call with (Regular=1) for open 2 close
SDB()
################### TRAIN ####################
#DDate <- as.Date("2016/7/1")           #train on first 5 years
k <- 20                                #how many days to use for k parameter

###############################
## Testing the Saw
###############################

load("DataWork/StockPrices.Rdata")
StockPrices<-StockPrices[,45:46]  #("cashreturn", "Marketreturn")
n<-c(10000,300)  #number of randimize returns the 1st for up, the second down.#must have multiple of 10 for 10 Stocks 
RandoMUp<-runif(n[1],0.1,0.4)
RandoMUp<-matrix(RandoMUp,ncol = 10)
RandoMDown<--runif(n[2],0.1,0.4)
RandoMDown<-matrix(RandoMDown,ncol=10)
RandoM<-rbind(RandoMUp,RandoMDown)
hist(RandoM)
plot(cumprod(RandoM/100+1))
StockPrices<-cbind(StockPrices[1:(sum(n)/10),1],RandoM)
colnames(StockPrices)<-c("cashreturn","returns1","returns2","returns3","returns4","returns5","returns6","returns7","returns8","returns9","returns10")
save(StockPrices,file  = "DataWork/StockPrices.Rdata")
###################################
## End building databse for Saw  ##
###################################

#Kvec - split dates from DDate backward into k-windows
#Holiday include parameter default 0, removes more than 3 days of no trading
#Kvec <- K_Histogram(K=k,DDate=DDate)
l <- 2                              #number of different classes used in classification
KKR <- "K-means"
#Classification(KVec=Kvec,K=k,L=l,KKR=KKR) #group k-windows into classes using KKR method

################# Predict ####################
#matchfunc function load
source("Code/match.r",echo=TRUE,max.deparse.length = 10000)       
#beststrat function load, uses matchfunc
source("Code/strategy.r",echo=TRUE,max.deparse.length = 10000)    
startDate <-as.Date("2010/01/21")
#DDate <- as.Date("2010/02/08")           #date for prediction, last date for backtest

#It will Stop at the first canging in weights or after 30 days.
DDate<-startDate
for ( i in 1:30){
  DDate <- DDate + 1
  xvec <- beststrat(DDate,k,l)           #generate single prediction for new day
  if(xvec[1]>0.5)break}
print(xvec)


file.remove("DataWork/Classifier.Rdata")
#backtest function load, calls beststrat
source("Code/trading.r",echo=TRUE,max.deparse.length = 10000)

#backtest starting from 1 year after initial date in import up to DDate
#startDate <- index(last(first(StockPrices,"1 year"),"1 day")) #1 year from start of data

backtest(startDate,DDate,k,l)          #for period backtesting and graphs/statistics
#file.remove("DataWork/Classifier.Rdata")
