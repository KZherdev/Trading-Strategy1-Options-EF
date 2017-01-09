library(plyr)
library(RBloomberg)
library(timeDate)

path_Scripts = "C://Users//Konstantin//Desktop//On hand//Europe Finance//Version 7//"
path_BLData = "C://Users//Konstantin//Desktop//On hand//Europe Finance//Data From BL//"

source(paste(path_Scripts, "CleanData.R", sep = ""))
source(paste(path_Scripts, "VolatilitySurfaceGrid.R", sep = ""))
#source(paste(path_Scripts, "DataBloomberg.R", sep = ""))
source(paste(path_Scripts, "Interpolation.R", sep = ""))

source(paste(path_Scripts, "BlackScholesFormulas.R", sep = ""))
source(paste(path_Scripts, "Algorithm.R", sep = ""))
source(paste(path_Scripts, "Strategy_1.R", sep = ""))

#Block 1: Set trading strategy diapazon

start_str.date <- as.Date("2013/09/04", format="%Y/%m/%d")
end_str.date <- as.Date("2013/10/24", format="%Y/%m/%d")
trading_dates = seq(start_str.date, end_str.date, by="days")

#remove weekends
trading_dates<- trading_dates[isWeekday(trading_dates)]; 
trading_dates

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
CleanInterestRates(path_BLData)
CleanFXSpot(path_BLData)
CleanVSDelta(trading_dates, path_BLData, ticker)
VolatilitySurfaceGridStrikes(trading_dates, path_BLData)

trade(trading_dates, path_BLData, strategy = "strategy_1")

