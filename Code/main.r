#main - calls all other sequential invest modules

#packages
require(xts)
require(randomForest)

set.seed(123) #for Monkey strategy reproducibility

#source all modules and execute before trading
source("Code/StockDataBuilder.R")      #builds StockPrices dataframe, to change to function
source("Code/K_Histogram.R")           #splits StockPrices to K-period days, returns KVec index vector
source("Code/Classification.R")        #classifer of trading days groups

SDB()                                  #StockDataBuilder function, imports all data
DDate <- as.Date("2011/1/1")           #train on first 5 years
k <- 20                                #how many days to use for k parameter
Kvec <- K_Histogram(K=k,DDate=DDate)   #split dates from DDate backward into k-windows
l <- 10                                #number of different classes used in classification
KKR <- "Monkey"
Classification(KVec=Kvec,K=k,L=l,KKR=KKR) #group k-windows into classes using KKR method
source("Code/match.r")                 #matchfunc function load
source("Code/strategy.r")              #beststrat function load, uses matchfunc
DDate <- as.Date("2011/2/1")
xvec <- beststrat(DDate,k,l)           #generate single prediction for new day
source("Code/trading.r")               #backtest function load, calls beststrat

#backtest starting from 1 year after initial date in import up to DDate
startDate <- index(last(first(StockPrices,"1 year"),"1 day")) #1 year from start of data
backtest(startDate,DDate,k,l)          #for period backtesting and graphs/statistics
