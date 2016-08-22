# Trading function for sequential invest

library(xts)
library(ggplot2)

load("DataWork/StockPrices.Rdata")
default1ystart <- index(last(first(StockPrices,"1 year"),"1 day"))
#backtest strategy from start of stock data till DDate
backtest <- function(StartDate=default1ystart, DDate,k,l) {
  retcolnames <- grep("return",colnames(StockPrices))
  nstocks <- length(retcolnames)
  n <- dim(StockPrices)[1]
  print(autoplot(cumprod(StockPrices[,retcolnames]/100+1),facets=NULL))
  period <- paste0(StartDate,"/",DDate)
  test_prices <- StockPrices[period]
  #run best strategy function for each date in test period and create xvec allocation matrix
  xvec <- t(apply(as.matrix(as.Date(index(test_prices))),1,beststrat,k,l))
  colnames(xvec) <- colnames(StockPrices)[retcolnames]
  colnames(xvec) <- gsub("return","weight",colnames(xvec))
  BacktestAllocation <- xts(xvec,index(test_prices))
  print(autoplot(BacktestAllocation)) #for debug of allocation vector
  TradeReturn <- rowSums(xvec*(test_prices[,retcolnames]/100+1))
  CumTradeReturn <- cumprod(TradeReturn)
  SDTradeReturn <- sd(TradeReturn)
  SharpeTradeReturn <- (last(CumTradeReturn)-1)/SDTradeReturn
  g1 <- ggplot(data.frame(date=index(test_prices),ret=CumTradeReturn))+geom_line(aes(x=date,y=ret))+
    ggtitle("Cumulative return from strategy")
  print(g1)
  print(sprintf("Total Profit percentage %.1f%% with volatility of %.1f and Sharpe %.1f",
                100*last(CumTradeReturn)-100,100*SDTradeReturn,SharpeTradeReturn))

  save(BacktestAllocation,file="DataWork/BacktestAllocation.Rdata")
  save(TradeReturn,file="DataWork/TradeReturn.Rdata")
}
