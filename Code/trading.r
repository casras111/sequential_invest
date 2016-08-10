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
  TradeReturn <- rowSums(xvec*test_prices[,retcolnames])
  CumTradeReturn <- cumsum(TradeReturn) #cumprod fix
  SDTradeReturn <- sd(TradeReturn)
  SharpeTradeReturn <- last(CumTradeReturn)/SDTradeReturn
  g1 <- ggplot(data.frame(date=index(test_prices),ret=CumTradeReturn))+geom_line(aes(x=date,y=ret))+
    ggtitle("Cumulative return from strategy")
  print(g1)
  print(sprintf("Total Profit %.1f NIS with volatility of %.1f and Sharpe %.1f",
                last(CumTradeReturn),SDTradeReturn,SharpeTradeReturn))
}
