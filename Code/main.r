#main - calls all other sequential invest modules

#packages
library(xts)

set.seed(1234) #for Monkey strategy reproducibility
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

################# Parameters Monkey #####################
k <- 10                     #how many days to use for k parameter
l <- 10                     #number of different classes used in classification
KKR <- "Monkey"            #clustering method, Monkey - random, K-Means - K-means

################# Train and Backtest ####################
#matchfunc function load
source("Code/match.r",echo=TRUE,max.deparse.length = 10000)       
#beststrat function load, uses matchfunc
source("Code/strategy.r",echo=TRUE,max.deparse.length = 10000)    
#backtest function load, calls beststrat
source("Code/trading.r",echo=TRUE,max.deparse.length = 10000)
#backtest starting from 1 year after initial date in import up to DDate
startDate <- index(last(first(StockPrices,"1 year"),"1 day")) #1 year from start of data
DDate <- as.Date("2008/1/2")           #date for prediction, last date for backtest
backtest(startDate,DDate,k,l)          #for period backtesting and graphs/statistics
file.remove("DataWork/Classifier.Rdata")

################# Parameters K-Means ####################
KKR <- "K-means"            #clustering method, Monkey - random, K-Means - K-means
backtest(startDate,DDate,k,l)          #for period backtesting and graphs/statistics
file.remove("DataWork/Classifier.Rdata")


################# Predict next day ######################
DDate <- as.Date("2009/1/2")           #date for prediction
xvec <- beststrat(DDate,k,l)           #generate single prediction for new day
file.remove("DataWork/Classifier.Rdata")
