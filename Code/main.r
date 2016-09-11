#main - calls all other sequential invest modules

#packages
library(xts)
library(ggplot2)

set.seed(1234) #for Monkey strategy reproducibility
if (basename(getwd())=="Code") setwd(normalizePath("..")) #for knitr workaround
#knitr::opts_knit$set(root.dir="..")

#source all modules and execute before trading
#SDB function, builds StockPrices dataframe
source("Code/StockDataBuilder.R",echo=TRUE,max.deparse.length = 10000)
#splits StockPrices to K-period days and returns KVec index vector
source("Code/K_Histogram.R",echo=TRUE,max.deparse.length = 10000)           
#classifer of trading days groups
source("Code/Classification.R",echo=TRUE,max.deparse.length = 10000)        

#SDB StockDataBuilder function, imports data
#default basis to close returns, call with (Regular=1) for open 2 close
StockPrices <- SDB()

#plot total period returns
retcolnames <- grep("return",colnames(StockPrices))
print(autoplot(cumprod(StockPrices[,retcolnames]/100+1),facets=NULL,
               main="Relative Stock Returns"))

# debug section reducing stocks universe to 1 stock and cash
# limitstocks <- c(2,12,13)
# autoplot(cumprod(StockPrices[,retcolnames[limitstocks]]/100+1),facets=NULL)
# StockPrices <- StockPrices[,retcolnames[limitstocks]]
# save(StockPrices,file = "DataWork/StockPrices.Rdata")

################# Parameters Monkey #####################
ksearch <- c(10) # (2,5,10,15,20) #values of k for which to optimize allocation vector
#k <- 10                     #how many days to use for k parameter
l <- 10                     #number of different classes used in classification
#KKR <- "Monkey"            #clustering method, Monkey - random, K-Means - K-means
KKR <- "Clara"
################# Train and Backtest ####################
#matchfunc function load
source("Code/match.r",echo=TRUE,max.deparse.length = 10000)       
#beststrat function load, uses matchfunc,K-Histogram and Classifier
source("Code/strategy.r",echo=TRUE,max.deparse.length = 10000)    
#backtest function load, calls beststrat
source("Code/trading.r",echo=TRUE,max.deparse.length = 10000)
#backtest starting from 1 year after initial date in import up to DDate
#startDate <- index(last(first(StockPrices,"3 years"),"1 day")) #1 year from start of data
StartDate <- as.Date("2008/7/20")    #first date for backtest prediction
DDate <- as.Date("2009/1/20")        #date for prediction, last date for backtest
#backtest - for period backtesting and graphs/statistics
backtest(StartDate,DDate,ksearch,l,window_th=0,recent_weight=F)

################# Parameters K-Means ####################
# KKR <- "K-means"            #clustering method, Monkey - random, K-Means - K-means
# #backtest - for period backtesting and graphs/statistics
# backtest(StartDate,DDate,ksearch,l,window_th=0,recent_weight=F)
# file.remove("DataWork/Classifier.Rdata")


################# Predict next day ######################
# DDate <- as.Date("2009/1/2")           #date for prediction
# #generate single prediction for new day
# (xvec <- beststrat(DDate,ksearch,l,window_th=0,recent_weight=F))

