library(plyr)
#library(RBloomberg)
library(timeDate)

#If to use git
source("CleanData.R")
source("VolatilitySurfaceGrid.R")
source("Algorithm.R")

source("BlackScholesFormulas.R")
source("Interpolation.R")
source("Strategy_1.R")

#Block 1: Set trading strategy diapazon

#Period 55
start_str.date <- as.Date("2013/09/04", format="%Y/%m/%d")
end_str.date <- as.Date("2013/10/24", format="%Y/%m/%d")

#Period 54
start_str.date <- as.Date("2013/03/28", format="%Y/%m/%d")
end_str.date <- as.Date("2013/05/02", format="%Y/%m/%d")

#Period 53
start_str.date <- as.Date("2013/03/10", format="%Y/%m/%d")
end_str.date <- as.Date("2013/03/27", format="%Y/%m/%d")

#Period 52
start_str.date <- as.Date("2013/02/25", format="%Y/%m/%d")
end_str.date <- as.Date("2013/03/06", format="%Y/%m/%d")

#Period 51
start_str.date <- as.Date("2012/12/10", format="%Y/%m/%d")
end_str.date <- as.Date("2012/12/21", format="%Y/%m/%d")

trading_period = seq(start_str.date, end_str.date, by="days") #the general period where we test model
trading_dates<- trading_period[isWeekday(trading_period)]; #actual trading dates within the period; remove weekends
path_BLData = paste("C://Users//Konstantin//Desktop//On hand//Europe Finance//Data From BL//Trading period ", trading_period[1], " to ", trading_period[length(trading_period)], "//", sep = "")
path_BLData
#temp
trade(ticker, trading_dates, trading_period, path_BLData, path_rates, strategy = "strategy_1")

#modify path BL data to store data associated with this trading period there
path_rates = "C://Users//Konstantin//Desktop//On hand//Europe Finance//Data From BL//"

#Block 2: Get Data from BL, interest rates and spot
conn <- blpConnect()

Interest_Rates_Get_Data(conn, start_hist.date, end_hist.date, path_BLData)
FXSpot_Get_Data(conn, start_hist.date, end_hist.date, path_BLData)

blpDisconnect(conn)
proc.time()

#Block 3 Clean interest rates, construct VS strike from VS deltas

#interest rates are taken from BL, weekends are omitted
#fx is taken hourly from Metastock, weekends are not omitted; generally can be taken from BL;
ticker = "EURUSD Curncy_"
CleanInterestRates(path_rates)
CleanFXSpot(path_BLData, ticker, trading_period) #trading_period, because input format for prices is in form of periods; but we skip via it using trading dates
CleanVSDelta(trading_dates, path_BLData, ticker) #trading dates, because we don't need weekends delta, even if BL provides them
VolatilitySurfaceGridStrikes(trading_dates, ticker, path_BLData, path_rates)#trading dates, as we need spots to do this; spot input format is only for weekdays


#trade(ticker, trading_dates, trading_period, path_BLData, path_rates, strategy = "strategy_1")

# c = BlackScholes76CallPrice(1.3641, 1.3316, 0.073, 0.0065, 0.0045, 1.0/252.0)*10000000
# c
# date = as.Date("2013/10/03", format="%Y/%m/%d")
# date
