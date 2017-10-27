require(quantmod)
require(dplyr)
require(scales)

GetTicker <- function () 
{
  today = Sys.Date()
  SymbolList = c("XLY", "XLP","XLE","XLF","XLV",
  "XLI","XLB","XLK","XLU")
  # S&P 500 Index
  GetTickerPerf <- function(Sym, Duration)
  {
    Ticker <- getSymbols(Sym, from = today - Duration, to = today, auto.assign = FALSE)
   
    #g <- autoplot(Ticker[4])
    xclose <- Ticker[,4]
    currQ = getQuote(Sym, src="yahoo")
    
    perf = round(100 * as.numeric((currQ[[2]] - xclose[[1]])/xclose[[1]]), digits=2)
    perf
  }
  
  DurationList <- c(7, 30, 60, 90) # 1 month, 2 month, 3 month
  sData = c()
  
  
  for (i in 1:length(SymbolList))
  {
    thisRow = c()
    for (d in 1 : length(DurationList))
    {
      perf <- GetTickerPerf(SymbolList[i], DurationList[d])
      thisRow <- c(thisRow,perf)
    }
   
    if (i == 1)
    {
      sData <- thisRow
    }
    else 
    {
      sData <- rbind(sData, thisRow)
    }
  }
  sData <- cbind(sData, SymbolList)
  colnames(sData) <- c("Price", "M1", "M2", "M3", "Symbol")
  rownames(sData) <- c(1:9)
  ds <- data.frame(sData, stringsAsFactors = TRUE)
  
  ggplot( aes(Symbol,Price, colour="Week 1", label="Price performance"), data=ds) + 
    geom_point(size=4) + 
    geom_point(aes(Symbol,M1, colour="Month 1"), data=ds,size=2) + 
    geom_point(aes(Symbol,M2, colour="Month 2"), data=ds,size=2) + 
    geom_point(aes(Symbol,M3, colour="Month 3"), data=ds,size=2)
   
}