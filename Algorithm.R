trade <- function(ticker, period_number, first_date, end_date, mainDir, path_rates, strategy) # third argument - for the strategy type
{
###This part doesn't depend on chosen strategy (begin)
  
  #Download all supporting data
  file_name = paste(path_rates, "Interest_Rates_historical_cleaned.csv", sep = "")
  rates = read.csv(file_name, header = TRUE)
  rates$date = as.Date(levels(rates$date), format="%Y-%m-%d")[rates$date] #remove levels before passing as argument, otw error

  #file_name = paste(path_BLData, "EURUSD_history_Sep_Oct_cleaned.csv", sep = "")
  path_strategy_folder = paste(mainDir, "Trading period_", period_number, " ", first_date, " to ", end_date, "//", sep = "")#created outside folder for our strategy
  
  file_name = paste(path_strategy_folder, ticker, "historical_", first_date, " to ", end_date, "_cleaned.csv", sep = "")
  fx_spot = read.csv(file_name, header = TRUE)
  fx_spot$date = as.Date(levels(fx_spot$date), format="%Y-%m-%d")[fx_spot$date] #Y should be capital, otherwise 2020; #remove levels before passing as argument, otw error
  
  #Initialize PL in derivative and physical position
  PL_d = 0
  PL_p = 0
  X_out = 0 #outstanding amount of currency
  res = 0 # resulting PL from the trading strategy on the given time horizon
  
  trading_period = seq(first_date, end_date, by="days") #the general period where we test model
  trading_dates<- trading_period[isWeekday(trading_period)]; #actual trading dates within the period; remove weekends
  
###This part doesn't depend on chosen strategy (end)
  if(strategy ==  "strategy_1")
  {
    file_name = paste(mainDir, ticker, " Trading Targets.csv", sep = "")
    trading_targets = read.csv(file_name, header = TRUE)
    target = trading_targets$target[period_number]
    res = strategy_1(path_strategy_folder, trading_dates, trading_period, rates, fx_spot, PL_d, PL_p, X_out, target)
    file_path_save_active_hedge_results = paste(path_strategy_folder, "Strategy_1 active hedge.csv", sep = "")
    write.csv(res, file = file_path_save_active_hedge_results, row.names = FALSE)
    
  }
  if(strategy ==  "strategy_2")
  {
    file_name = paste(mainDir, ticker, " Trading Targets.csv", sep = "")
    trading_targets = read.csv(file_name, header = TRUE)
    target = trading_targets$target[period_number]
    res = strategy_2(path_strategy_folder, trading_dates, trading_period, rates, fx_spot, PL_d, PL_p, X_out, target)
    View(res)
    file_path_save_active_hedge_results = paste(path_strategy_folder, "Strategy_2 active hedge.csv", sep = "")
    write.csv(res, file = file_path_save_active_hedge_results)
  }
  
  if(strategy ==  "strategy_3")
  {
    file_name = paste(mainDir, ticker, " Trading Targets.csv", sep = "")
    trading_targets = read.csv(file_name, header = TRUE)
    target = trading_targets$target[period_number]
    res = strategy_3(path_strategy_folder, trading_dates, trading_period, rates, fx_spot, PL_d, PL_p, X_out, target)
    View(res)
    file_path_save_active_hedge_results = paste(path_strategy_folder, "Strategy_3 active hedge.csv", sep = "")
    write.csv(res, file = file_path_save_active_hedge_results)
  }
  
  return()
}

