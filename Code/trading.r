# Trading function for sequential invest

library(xts)
library(ggplot2)
library(reshape2)

load("DataWork/StockPrices.Rdata")
default1ystart <- index(last(first(StockPrices,"1 year"),"1 day"))
#backtest strategy from start of stock data till DDate
backtest <- function(StartDate=default1ystart, DDate,ksearch,l,
                     window_th,recent_weight) {
  retcolnames <- grep("return",colnames(StockPrices))
  nstocks <- length(retcolnames)
  period <- paste0(StartDate,"/",DDate)
  test_prices <- StockPrices[period]
  print(autoplot(cumprod(test_prices[,retcolnames]/100+1),facets=NULL,
           main="Relative Stock Returns in backtest"))
  #run best strategy function for each date in test period and create xvec allocation matrix
  xvec <- t(apply(as.matrix(as.Date(index(test_prices))),
                  1,beststrat,ksearch,l,window_th,recent_weight))
  colnames(xvec) <- colnames(StockPrices)[retcolnames[-nstocks]]
  colnames(xvec) <- gsub("return","",colnames(xvec))
  BacktestAllocation <- xts(xvec,index(test_prices))
  print(autoplot(BacktestAllocation,              #for debug of allocation vector
                 main="Stock Weights for backtest period"))
  TradeReturn <- rowSums(xvec*(test_prices[,retcolnames[-nstocks]]/100+1))
  CumTradeReturn <- cumprod(TradeReturn)
  #to calculate sharpe derive yearly volatility and normalize return to yearly value
  SDTradeReturn <- sd(TradeReturn)*16 #multiply by sqrt(256) to get yearly sd
  yearsperiod <- as.numeric(index(last(test_prices))-index(first(test_prices)))/360
  SharpeTradeReturn <- (last(CumTradeReturn)^(1/yearsperiod)-1)/SDTradeReturn
  MktReturn <- test_prices$Marketreturn/100+1
  CumMktReturn <- cumprod(MktReturn)
  # bardat <- xts(TradeReturn-MktReturn,index(test_prices))
  # colnames(bardat) <- "Alpha"
  # barplot(bardat,main="Alpha achieved by strategy")
  plotmkt <- as.numeric(coredata(MktReturn)-1)
  plottrade <- TradeReturn-1
  print("Comparison of positive/negative returns vs market")
  print(table(Market=sign(plotmkt),Strategy=sign(plottrade)))
  boxplot(plotmkt,plottrade,names=c("Market Returns","Strategy Returns"))
  
  plotdata <- data.frame(CumMktReturn,CumTradeReturn,Date=index(test_prices))
  colnames(plotdata) <- c("CumMarketReturn","CumTradeReturn","Date")
  plotdata <- melt(plotdata,id="Date")
  klabs <- paste(ksearch,collapse=",") #create string of k values
  g1 <- ggplot(plotdata,aes(x=Date,y=value,colour=variable))+geom_line()+
    ggtitle(paste0(KKR," k: ",klabs," - Cumulative return"))
  print(g1)

  print(sprintf("Total Profit percentage %.1f%% with volatility of %.1f and Sharpe %.1f",
                100*last(CumTradeReturn)-100,100*SDTradeReturn,SharpeTradeReturn))

  save(BacktestAllocation,file="DataWork/BacktestAllocation.Rdata")
  save(TradeReturn,file="DataWork/TradeReturn.Rdata")
}
