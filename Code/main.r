#main - calls all other sequential invest modules

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
DDate <- as.Date("2008/1/1")           #train on first 5 years
k <- 20                                #how many days to use for k parameter
#Kvec - split dates from DDate backward into k-windows
#Holiday include parameter default 0, removes more than 3 days of no trading
#Kvec <- K_Histogram(K=k,DDate=DDate)
l <- 10                                #number of different classes used in classification
KKR <- "K-means"
#Classification(KVec=Kvec,K=k,L=l,KKR=KKR) #group k-windows into classes using KKR method

################# Predict ####################
#matchfunc function load
source("Code/match.r",echo=TRUE,max.deparse.length = 10000)       
#beststrat function load, uses matchfunc
source("Code/strategy.r",echo=TRUE,max.deparse.length = 10000)    
DDate <- as.Date("2008/1/2")           #date for prediction, last date for backtest
xvec <- beststrat(DDate,k,l)           #generate single prediction for new day
file.remove("DataWork/Classifier.Rdata")
#backtest function load, calls beststrat
source("Code/trading.r",echo=TRUE,max.deparse.length = 10000)

#backtest starting from 1 year after initial date in import up to DDate
startDate <- index(last(first(StockPrices,"1 year"),"1 day")) #1 year from start of data
backtest(startDate,DDate,k,l)          #for period backtesting and graphs/statistics
file.remove("DataWork/Classifier.Rdata")
