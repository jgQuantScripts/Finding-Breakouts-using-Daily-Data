require("quantmod");require("RSQLite");require("lubridate");require("DEoptim");require("PerformanceAnalytics")
require("data.table");require("pbapply")
# *************************************************************************************************
#                                    Wrappers
# *************************************************************************************************
# function to get stock data from SQLite DB
getOHLC = function(ticker)
{
  # connect to SQLite DB
  driver = dbDriver("SQLite")
  daytoday = "20210509"
  con = dbConnect(driver, dbname = paste0("/Volumes/3TB/SQLite/",daytoday,"_getSymbols.db"))
  # get stock data
  df = dbGetQuery(con, paste0("SELECT * FROM getSymbols WHERE Symbol ='",ticker,"'"))
  # disconnect
  dbDisconnect(con)
  # convert to xts object
  df <- xts(df[,c("Open","High","Low","Close","Volume")], order.by = as.Date(df$Date, origin="1970-01-01"))
  # format column names
  colnames(df) <- paste0(ticker,".",names(df))
  # only want OHLC data-> drop Volume Column
  OHLC(df)
}
# function to find breakouts n Days
# df          : OHLC object
# lookBackDays: will lookback n-days to see if stock broke out of the range
# closeAtHigh : if TRUE will ONLY consider it a break out if stock closed at the High of the day
#             : if FALSE will consider it a break out if it is higher than n-days,
#               but did not close at the High of the Day
breakOut = function(df, lookBackDays, closeAtHigh=FALSE)
{
  # add MAX price column
  df$MAX = na.omit(apply(coredata(df),1, max))
  # merge Closes with the MaX prices
  tmp = merge(Cl(df), df$MAX)
  # rollapply function every step ('by') & window is width
  tmp <- rollapply(data=tmp, width=lookBackDays,by=1,align = 'right',by.column = FALSE, 
                   FUN=function(x){
                     LAST <- as.numeric(last(Cl(x))) # last is the last/latest price in nth place 
                     bo <- nrow(x[LAST >= x$MAX,])   # compare how many times the lastest price >= MAX column
                   })
  # add column to data
  df$nDaysHigher <- as.numeric(coredata(tmp))
  # complete cases
  df <- na.omit(df)
  # subset breakout days
  if(closeAtHigh == TRUE)
  {
    breakOutDates =  df[which(df$nDaysHigher == lookBackDays),]
  }
  
  if(closeAtHigh == FALSE)
  {
    # stocks not closing at the Highs of the day will return lookBackDays - 1 
    breakOutDates = rbind(df[which(df$nDaysHigher == lookBackDays),],
                          df[which(df$nDaysHigher == (lookBackDays-1)),]) %>% 
      make.index.unique(drop=TRUE, fromLast = TRUE)
  }
  # return df
  breakOutDates
}
# ATH_on_Close: if TRUE it will calculate the All-Time-High (ATH) based on the Close 
#             : if FALSE it will calculate the ATH using the High column
breakOutATH = function(df, ATH_on_Close=TRUE)
{
  
  # add all time High
  if(ATH_on_Close == TRUE) {df$ATH <- cummax(Cl(df))}
  if(ATH_on_Close == FALSE){df$ATH <- cummax(Hi(df))}
  # subset all time Highs
  breakOutDates = df[which(Cl(df) == df$ATH),]
  # to return
  breakOutDates
}
# function to return performance for: 1-week, 2-weeks, 4-weeks, & 6-weeks after breakout
breakOutPerformance = function(df,breakOuts)
{
  # extract dates
  BO = index(breakOuts)
  # calculate performance After Break Out: 1 week, 2 weeks, 4 weeks, 6 weeks
  pct = lapply(as.list(1:length(BO)), function(ii){
    # subset starting date only
    START = BO[ii]
    # calculate date ranges
    oneWk  = paste0(START,"/",START+weeks(1))
    twoWk  = paste0(START,"/",START+weeks(2))
    fourWk = paste0(START,"/",START+weeks(4))
    siixWk = paste0(START,"/",START+weeks(6))
    # subset data    
    oneWk  = df[oneWk]
    twoWk  = df[twoWk]
    fourWk = df[fourWk]
    siixWk = df[siixWk]
    # calculate performance
    boPRC= as.numeric(Cl(oneWk))[1]
    oneWk  = round((as.numeric(Cl(oneWk[nrow(oneWk)]))/boPRC)-1,4)
    twoWk  = round((as.numeric(Cl(twoWk[nrow(twoWk)]))/boPRC)-1,4)
    fourWk = round((as.numeric(Cl(fourWk[nrow(fourWk)]))/boPRC)-1,4)
    siixWk = round((as.numeric(Cl(siixWk[nrow(siixWk)]))/boPRC)-1,4)
    # return as a data frame
    toRET = as.data.frame(cbind(paste(START), boPRC, oneWk,twoWk,fourWk,siixWk))
    colnames(toRET)[1] = c("boDATE")
    toRET
  })
  # row bind
  pct = do.call(rbind,pct)
  # convert to numeric
  pct$boPRC <- as.numeric(pct$boPRC)
  pct$oneWk <- as.numeric(pct$oneWk)
  pct$twoWk <- as.numeric(pct$twoWk)
  pct$fourWk <- as.numeric(pct$fourWk)
  pct$siixWk <- as.numeric(pct$siixWk)
  # return df
  pct
}
# get Geometric Return, Gain 2 Loss Ratio, & Sharpe Ratio
getStats = function(boPerformance){
############## 1st Week Stats
geo1 = table.Stats(boPerformance$oneWk)["Geometric Mean",] %>% as.numeric()
g2l_1WK = round(length(boPerformance$oneWk[boPerformance$oneWk>=0])/length(boPerformance$oneWk),4)
sharpe1 = round(mean(boPerformance$oneWk)/sd(boPerformance$oneWk),4)
############## 2nd Week Stats
geo2 = table.Stats(boPerformance$twoWk)["Geometric Mean",] %>% as.numeric()
g2l_2WK = round(length(boPerformance$twoWk[boPerformance$twoWk>=0])/length(boPerformance$twoWk),4)
sharpe2 = round(mean(boPerformance$twoWk)/sd(boPerformance$twoWk),4)
############## 3rd Week Stats
geo3 = table.Stats(boPerformance$fourWk)["Geometric Mean",] %>% as.numeric()
g2l_4WK = round(length(boPerformance$fourWk[boPerformance$fourWk>=0])/length(boPerformance$fourWk),4)
sharpe3 = round(mean(boPerformance$fourWk)/sd(boPerformance$fourWk),4)
############## 4th Week Stats
geo4 = table.Stats(boPerformance$siixWk)["Geometric Mean",] %>% as.numeric()
g2l_6WK = round(length(boPerformance$siixWk[boPerformance$siixWk>=0])/length(boPerformance$siixWk),4)
sharpe4 = round(mean(boPerformance$siixWk)/sd(boPerformance$siixWk),4)
############## merge 
tmp <- as.data.frame(cbind(ii, geo1,geo2,geo3,geo4,
                           g2l_1WK,g2l_2WK,g2l_4WK,g2l_6WK,
                           sharpe1,sharpe2,sharpe3,sharpe4))
tmp
}
# *************************************************************************************************
#                                   Test Function
# *************************************************************************************************
ticker = "SPY"
# get stock data
df = getOHLC(ticker)
#chartSeries(df[,c(1:4)]["20200311/20210331"])
breakOuts = breakOut(df=df, lookBackDays = 10,closeAtHigh=FALSE)
boPerformance = breakOutPerformance(df=df,breakOuts=breakOuts)

# Break-Outs on ALL TIME HIGHS only
breakOutsATH = breakOutATH(df=df,ATH_on_Close=TRUE)
boPerformanceATH = breakOutPerformance(df=df,breakOuts=breakOutsATH)
# *************************************************************************************************
#                                   Find Returns for different lookBackDays
# *************************************************************************************************
# calculate geometric means of a range of 'lookBackDays' 
rets <- pblapply(as.list(9:252), function(ii){
  breakOuts = breakOut(df=df,lookBackDays=ii,closeAtHigh=FALSE)
  boPerformance = breakOutPerformance(df=df,breakOuts=breakOuts)
  tmp <- getStats(boPerformance)
  tmp
})
rets <- do.call(rbind,rets)
colnames(rets) <- c("N","oneWK","twoWK","fourWK","siixWK",
                    "g2l_1WK","g2l_2WK","g2l_4WK","g2l_6WK",
                    "sharpe1","sharpe2","sharpe3","sharpe4")
# rets <- readRDS("retsBO_SPY.rds")
# View results
breakOuts = breakOut(df=df,lookBackDays=9,closeAtHigh=FALSE)
boPerformance = breakOutPerformance(df=df,breakOuts=breakOuts)
# ****************************************************************************************************
# ****************************************************************************************************
# calculate geometric means of a range of 'lookBackDays' 
retsCH <- pblapply(as.list(9:252), function(ii){
  breakOuts = breakOut(df=df,lookBackDays=ii,closeAtHigh=TRUE)
  boPerformance = breakOutPerformance(df=df,breakOuts=breakOuts)
  tmp <- getStats(boPerformance)
  tmp
})
retsCH <- as.data.frame(do.call(rbind,retsCH))
colnames(retsCH) <- c("N","oneWK","twoWK","fourWK","siixWK",
                      "g2l_1WK","g2l_2WK","g2l_4WK","g2l_6WK",
                      "sharpe1","sharpe2","sharpe3","sharpe4")
#retsCH <- readRDS(retsCH,"retsCH_SPY.rds")
# View results
breakOuts = breakOut(df=df,lookBackDays=9,closeAtHigh=TRUE)
boPerformance = breakOutPerformance(df=df,breakOuts=breakOuts)
# ****************************************************************************************************
# ****************************************************************************************************
# connect to database
driver = dbDriver("SQLite")
daytoday = "20210509"
con = dbConnect(driver, dbname = paste0("/Volumes/3TB/SQLite/",daytoday,"_getSymbols.db"))
# calculating dollar volume on tickers in my database
dbDollarVol = dbGetQuery(con, paste0("SELECT Symbol, round(getSymbols.Close*getSymbols.Volume,1)", 
                                     "as DollarVol FROM getSymbols GROUP BY Symbol ",
                                     "ORDER BY AVG(DollarVol) DESC "))
# should return as numeric, but assigning as numeric column
dbDollarVol$DollarVol <- as.numeric(dbDollarVol$DollarVol)
# re-order -> Decreasing order
dbDollarVol = dbDollarVol[order(dbDollarVol$DollarVol, decreasing = TRUE),]
# delete INDEX instances 
dbDollarVol <- dbDollarVol[dbDollarVol$Symbol != c("DJI","DJT","SP600"),]

# top 50 stocks
tickers <- dbDollarVol$Symbol[1:50]
# Calculate breakouts and extract stats to compare
ALL = lapply(as.list(tickers), function(ticker){
  df = getOHLC(ticker)
  rets <- pblapply(as.list(9:252), function(ii){
    # try-error in case it is a relatively new ticker
    breakOuts = try(breakOut(df=df,lookBackDays=ii,closeAtHigh=FALSE), silent = TRUE)
    # calculate stats
    if(!inherits(breakOuts, 'try-error')){
      boPerformance = breakOutPerformance(df=df,breakOuts=breakOuts)
      geo1 = table.Stats(boPerformance$oneWk)["Geometric Mean",] %>% as.numeric()
      g2l_1WK = round(length(boPerformance$oneWk[boPerformance$oneWk>=0])/length(boPerformance$oneWk),4)
      sharpe1 = round(mean(boPerformance$oneWk)/sd(boPerformance$oneWk),4)
      geo2 = table.Stats(boPerformance$twoWk)["Geometric Mean",] %>% as.numeric()
      g2l_2WK = round(length(boPerformance$twoWk[boPerformance$twoWk>=0])/length(boPerformance$twoWk),4)
      sharpe2 = round(mean(boPerformance$twoWk)/sd(boPerformance$twoWk),4)
      geo3 = table.Stats(boPerformance$fourWk)["Geometric Mean",] %>% as.numeric()
      g2l_4WK = round(length(boPerformance$fourWk[boPerformance$fourWk>=0])/length(boPerformance$fourWk),4)
      sharpe3 = round(mean(boPerformance$fourWk)/sd(boPerformance$fourWk),4)
      geo4 = table.Stats(boPerformance$siixWk)["Geometric Mean",] %>% as.numeric()
      g2l_6WK = round(length(boPerformance$siixWk[boPerformance$siixWk>=0])/length(boPerformance$siixWk),4)
      sharpe4 = round(mean(boPerformance$siixWk)/sd(boPerformance$siixWk),4)
      tmp <- as.data.frame(cbind(ii, geo1,geo2,geo3,geo4,
                                 g2l_1WK,g2l_2WK,g2l_4WK,g2l_6WK,
                                 sharpe1,sharpe2,sharpe3,sharpe4))
      tmp$Symbol <- ticker
    }else{
      tmp <- NULL
    }
    tmp
  })
  rets <- rbindlist(rets,use.names = TRUE)
  if(nrow(rets)>0){
    colnames(rets) <- c("N","oneWK","twoWK","fourWK","siixWK",
                        "g2l_1WK","g2l_2WK","g2l_4WK","g2l_6WK",
                        "sharpe1","sharpe2","sharpe3","sharpe4",
                        "Symbol")
  }
  rets
})
# row bind results
ALL <- rbindlist(ALL,use.names = TRUE)
ALL <- readRDS("BO_50.rds")

# drill-down best performers
ticker = "PLTR"
N = 19
# get stock data
df = getOHLC(ticker)
breakOuts = breakOut(df=df, lookBackDays = N,closeAtHigh=FALSE)
boPerformance = breakOutPerformance(df=df,breakOuts=breakOuts)


# we can subset by Sharpe and Holding Period
twoWK_Sharpe <- subset(ALL, ALL$sharpe2 > 0.80 & ALL$g2l_2WK > 0.70)

