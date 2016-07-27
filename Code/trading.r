# Trading function for sequential invest

library(xts)
library(ggplot2)


load("DataWork/StockPrices.Rdata")
retcolnames <- grep("return",colnames(StockPrices))
nstocks <- length(retcolnames)
n <- dim(StockPrices)[1]
#temp random data as standby for Oz data
#dates <- seq(as.Date("2006/1/1"),as.Date("2016/5/1"),by="day")
#n <- length(dates)
#randomize return simulation
#StockPrices <- xts(cbind(a=rnorm(n),b=rnorm(n),c=rnorm(n)),dates)
autoplot(cumprod(StockPrices[,retcolnames]/100+1),facets=NULL)

#nstocks <- dim(StockPrices)[2]
#randomize stock allocation
xvec <- matrix(runif(n*nstocks),ncol=nstocks)
#make sure it adds up to 1
xvec <- xvec/rowSums(xvec)
#end of temp

TradeReturn <- rowSums(xvec*StockPrices[,retcolnames])
CumTradeReturn <- cumsum(TradeReturn)
SDTradeReturn <- sd(TradeReturn)
SharpeTradeReturn <- CumTradeReturn[n]/SDTradeReturn
ggplot(data.frame(date=index(StockPrices),ret=CumTradeReturn))+geom_line(aes(x=date,y=ret))+
  ggtitle("Cumulative return from strategy")
print(sprintf("Total Profit %.1f NIS with volatility of %.1f and Sharpe %.1f",
              CumTradeReturn[n],SDTradeReturn,SharpeTradeReturn))
