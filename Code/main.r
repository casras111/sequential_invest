#main - calls all other sequential invest modules

#packages :
require(xts)
require(randomForest)

#source all modules and execute before trading
source("Code/StockDataBuilder.R")      #builds StockPrices dataframe, to change to function
source("Code/K_Histogram.R")           #splits StockPrices to K-period days, returns KVec index vector
source("Code/Classification.R")

StockDataBuilder<-SDB()                #StockDataBuilder
DDate <- as.Date("2011/1/1")           #train on first 5 years
k <- 20 #how many days to use for k parameter
Kvec <- K_Histogram(K=k,DDate=DDate)
l <- 10 #number of different classes used in classification
KKR <- "K-means"
Classification(KVec=Kvec,K=k,L=l,KKR=KKR) #classifer #have no output,
source("Code/match.r")
source("Code/strategy.r")
DDate <- as.Date("2011/2/1") #TBD to fix match to take also non-trading days
xvec <- beststrat(DDate,k,l) #for new single prediction
source("Code/trading.r")
startDate <- index(last(first(StockPrices,"1 year"),"1 day")) #1 year from start of data
backtest(startDate,DDate,k,l)    #for period backtesting and graphs/statistics
