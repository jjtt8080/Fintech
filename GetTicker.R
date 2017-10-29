require(quantmod)
require(dplyr)
require(scales)
require(ggplot2)
GetTicker <- function () 
{
  today = Sys.Date()
  SymbolList = c("SPY", "XLY", "XLP","XLE","XLF","XLV",
  "XLI","XLB","XLK","XLU")
  # S&P 500 Index
  GetTickerPerf <- function(Sym, DurationList)
  {
    ld <- DurationList[length(DurationList)]
    Ticker <- getSymbols(Sym, from = today - ld, to = today, auto.assign = FALSE)
    currQ = getQuote(Sym, src="yahoo")
    #g <- autoplot(Ticker[4])
    ds = c()  
     
    for (i in 1:length(DurationList))
    {
      currDuration = DurationList[i]
      destDate = today - currDuration
      ## Adjust saturday to Friday
      if (weekdays(destDate, abbreviate = TRUE) == "Sat")
        destDate = destDate - 1
      if (weekdays(destDate, abbreviate = TRUE) == "Sun")
        destDate = destDate + 1
   
      #print(currDuration)
      #print(destDate)
      #print(dim(Ticker))
      xclose <- Ticker[destDate]
      currPerf = round(100 * as.numeric((currQ[[2]] - xclose[[1]])/xclose[[1]]), digits=2)
      ds <- rbind(ds, c(Sym, as.numeric(DurationList[i]), as.numeric(currPerf)))
    }
    colnames(ds) <- c("Symbol", "Time", "Price")
    ds <- data.frame(ds)
    ds
  }
  
  DurationList <- c(1, 7, 30, 60, 90) # 1 month, 2 month, 3 month
  sData = c()
  
   
  for (i in 1:length(SymbolList))
  {
      thisRow = c()
      
      ## Find the current stock's perf in this duration range
      
      currPerf <- GetTickerPerf(SymbolList[i], DurationList)
      meanPerf <- mean(as.numeric(as.character(currPerf$Price)))
      currPerf <- cbind(currPerf, meanPerf)
      sData <- rbind(sData, currPerf)
  
  }
  colnames(sData) <- c("Symbol", "Time", "PricePerf", "MeanPerf") 
  # Find the top 4 performer and mark it as Pick/NotPick
  u <- unique(sData$MeanPerf)
  ordered_perf <- u[order(u,decreasing=TRUE)]
  maxNumTick = ifelse(length(SymbolList)/2>=5, 5, length(SymbolList)/2>=5)
  cutoff_perf <- ordered_perf[maxNumTick]
  sData$Pick <- ifelse(sData$MeanPerf >= cutoff_perf, "Y", "N")
  #print(sData)
  rownames(sData) <- seq(1:dim(sData)[1])
  ds <- data.frame(sData)
  
  ds$Time <- as.numeric(levels(ds$Time)[ds$Time])
  options(digits=5)
  ds$PricePerf <- as.numeric(as.character(ds$PricePerf))
 
  g <- ggplot(ds, aes(x=Time,
                      y=PricePerf,
                      group=Symbol,
                      colour=Symbol)) + 
    facet_grid(.~Pick,labeller = label_both)+
    geom_point(size=3,show.legend=TRUE, aes(alpha=Symbol)) + 
    geom_line() + 
    labs(y ="Price Performance %", x = "Time (days)") +
    geom_hline(yintercept = 0.1, colour='coral', size=2) + 
    scale_x_continuous(breaks = DurationList) + 
    scale_y_continuous(breaks = c(-5:20, 5)) +
    ggtitle("ETF Sector picker")
  ggsave(filename="Ticker_Output.png", plot=g)
  print(g)
  
  print("Pick the top 5 performer according to mean performance of week,30, 60, 90 days:")
  ds %>%
    filter(Pick=="Y") %>%
    select(Symbol, MeanPerf) %>%
    group_by(Symbol) %>%
    arrange(desc(MeanPerf)) %>%
    unique()
   
}