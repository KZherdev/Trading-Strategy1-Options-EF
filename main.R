library(plyr)
#library(RBloomberg)
library(timeDate)

#If to use git
source("Construct Trading Periods.R")
source("Construct Folders For Periods.R")
source("CleanData.R")
source("VolatilitySurfaceGrid.R")
source("BlackScholesFormulas.R")
source("Interpolation.R")


source("Algorithm.R")
source("Intervals_utility.R")
source("Strategy_1.R")
source("Strategy_2.R")
source("Strategy_3.R")


#Block 1: Specify time periods, for which we are going to test our trading strategy
all_trading_periods = construct_trading_periods()
View(all_trading_periods)

#Block 2: Create folders for all given periods; for convenience (if add)

#ALREADY CREATED
 mainDir = "C://Users//Konstantin//Desktop//On hand//Europe Finance//Data From BL//"
# for(i in 1:nrow(all_trading_periods))
#   construct_folder_for_period(mainDir, all_trading_periods$period[i], all_trading_periods$start_date[i], all_trading_periods$end_date[i])


path_rates = "C://Users//Konstantin//Desktop//On hand//Europe Finance//Data From BL//" #could have done = mainDir alternatively

#Block 3: Get Data from BL, interest rates and spot
conn <- blpConnect()

Interest_Rates_Get_Data(conn, start_hist.date, end_hist.date, path_BLData)
FXSpot_Get_Data(conn, start_hist.date, end_hist.date, path_BLData)

blpDisconnect(conn)
proc.time()

#Block 4 Clean interest rates, construct VS strike from VS deltas
#interest rates are taken from BL, weekends are omitted
#fx is taken hourly from Metastock, weekends are not omitted; generally can be taken from BL;

ticker = "EURUSD Curncy_"
CleanInterestRates(path_rates)

#clean spot, delta, construct strikes
for(i in 51:55)
{
  print(paste("i = ", i, sep = ""))
  period_number = all_trading_periods$period[i]
  first_date = all_trading_periods$start_date[i]
  end_date = all_trading_periods$end_date[i]
  trading_period = seq(first_date, end_date, by="days") #the general period where we test model
  trading_dates<- trading_period[isWeekday(trading_period)]; #actual trading dates within the period; remove weekends
  
  CleanFXSpot(mainDir, ticker, i, first_date, end_date)
  CleanVSDelta(mainDir, ticker, period_number, first_date, end_date, trading_dates)
  VolatilitySurfaceGridStrikes(mainDir, ticker, period_number, first_date, end_date, trading_dates, path_rates)
  
}



for(i in 1:50)
{
  print(paste("period ", i, sep = ""))
  period_number = all_trading_periods$period[i]
  first_date = all_trading_periods$first_day[i]
  end_date = all_trading_periods$last_day[i]
  trading_period = seq(first_date, end_date, by="days") #the general period where we test model
  trading_dates<- trading_period[isWeekday(trading_period)]; #actual trading dates within the period; remove weekends
  CleanVSDelta(mainDir, ticker, period_number, first_date, end_date, trading_period) #!!!changed to period
  
}

for(i in 1:39)
{
  print(paste("i = ", i, sep = ""))
  period_number = all_trading_periods$period[i]
  first_date = all_trading_periods$first_day[i]
  end_date = all_trading_periods$last_day[i]  
  
  CleanFXSpot(mainDir, ticker, i, first_date, end_date)
  
}

source("VolatilitySurfaceGrid.R")

#i != 31
for(i in 1:39)
{
  print(paste("i = ", i, sep = ""))
  period_number = all_trading_periods$period[i]
  first_date = all_trading_periods$first_day[i]
  end_date = all_trading_periods$last_day[i]  
  
  trading_period = seq(first_date, end_date, by="days") #the general period where we test model
  trading_dates<- trading_period[isWeekday(trading_period)]; #actual trading dates within the period; remove weekends
  
  VolatilitySurfaceGridStrikes(mainDir, ticker, period_number, first_date, end_date, trading_period, path_rates)
  
}


#Block 5 - apply strategy, trade

source("Construct Trading Periods.R")
source("Construct Folders For Periods.R")
source("CleanData.R")
source("VolatilitySurfaceGrid.R")
source("BlackScholesFormulas.R")
source("Interpolation.R")


source("Algorithm.R")
source("Intervals_utility.R")
source("Strategy_1.R")
source("Strategy_2.R")
source("Strategy_3.R")

i = 51
period_number = all_trading_periods$period[i]
first_date = all_trading_periods$first_day[i]
end_date = all_trading_periods$last_day[i]  

trading_period = seq(first_date, end_date, by="days") #the general period where we test model
trading_dates<- trading_period[isWeekday(trading_period)]; #actual trading dates within the period; remove weekends
#trading_dates

trade(ticker, period_number, first_date, end_date, mainDir, path_rates, strategy = "strategy_3")


#Period 55
# start_str.date <- as.Date("2013/09/04", format="%Y/%m/%d")
# end_str.date <- as.Date("2013/10/24", format="%Y/%m/%d")

# #Period 54
# start_str.date <- as.Date("2013/03/28", format="%Y/%m/%d")
# end_str.date <- as.Date("2013/05/02", format="%Y/%m/%d")

# #Period 53
# start_str.date <- as.Date("2013/03/10", format="%Y/%m/%d")
# end_str.date <- as.Date("2013/03/27", format="%Y/%m/%d")

# #Period 52
# start_str.date <- as.Date("2013/02/25", format="%Y/%m/%d")
# end_str.date <- as.Date("2013/03/06", format="%Y/%m/%d")

# #Period 51
# start_str.date <- as.Date("2012/12/10", format="%Y/%m/%d")
# end_str.date <- as.Date("2012/12/21", format="%Y/%m/%d")

# trading_period = seq(start_str.date, end_str.date, by="days") #the general period where we test model
# trading_dates<- trading_period[isWeekday(trading_period)]; #actual trading dates within the period; remove weekends
# path_BLData = paste("C://Users//Konstantin//Desktop//On hand//Europe Finance//Data From BL//Trading period ", trading_period[1], " to ", trading_period[length(trading_period)], "//", sep = "")
# path_BLData
# 
# CleanFXSpot(path_BLData, ticker, trading_period) #trading_period, because input format for prices is in form of periods; but we skip via it using trading dates
# CleanVSDelta(trading_dates, path_BLData, ticker) #trading dates, because we don't need weekends delta, even if BL provides them
# VolatilitySurfaceGridStrikes(trading_dates, ticker, path_BLData, path_rates)#trading dates, as we need spots to do this; spot input format is only for weekdays

#temp
# c = BlackScholes76CallPrice(1.1860, 1.202629, 0.078809, 0.00735, 0.0042, 20.0/365.0)*10000000
# c
# date = as.Date("2013/10/03", format="%Y/%m/%d")
# date
# 
# 
# 
# 
# write.csv(df_temp, file = path_test)
# df_temp
