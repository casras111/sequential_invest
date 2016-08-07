#main - calls all other sequential invest modules

#source all modules and execute before trading
source("Code/StockDataBuilder.R")
source("Code/K_Histogram.R")
DDate <- as.Date("2016/1/1")
Kvec <- K_Histogram(K=20,DDate=DDate)
source("Code/Classification.R")
k <- 20 #how many days to use for k parameter
l <- 10 #number of different classes used in classification
Classification(KVec=Kvec,K=k,L=l)
source("Code/match.r")
DDate <- as.Date("2016/5/1")
source("Code/strategy.r")
xvec <- beststrat(DDate,k,l)
source("Code/trading.r")
