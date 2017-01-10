trade <- function(ticker, trading_dates, trading_period, path_BLData, path_rates, strategy) # third argument - for the strategy type
{
###This part doesn't depend on chosen strategy (begin)
  
  #Download all supporting data
  file_name = paste(path_rates, "Interest_Rates_historical_cleaned.csv", sep = "")
  rates = read.csv(file_name, header = TRUE)
  rates$date = as.Date(levels(rates$date), format="%Y-%m-%d")[rates$date] #remove levels before passing as argument, otw error
  
  #file_name = paste(path_BLData, "EURUSD_history_Sep_Oct_cleaned.csv", sep = "")
  file_name = paste(path_BLData, ticker, "historical_", trading_period[1], " to ", trading_period[length(trading_period)], "_cleaned.csv", sep = "")
  fx_spot = read.csv(file_name, header = TRUE)
  fx_spot$date = as.Date(levels(fx_spot$date), format="%Y-%m-%d")[fx_spot$date] #Y should be capital, otherwise 2020; #remove levels before passing as argument, otw error
  
  #Initialize PL in derivative and physical position
  PL_d = 0
  PL_p = 0
  X_out = 0 #outstanding amount of currency
  res = 0 # resulting PL from the trading strategy on the given time horizon
  
###This part doesn't depend on chosen strategy (end)
  if(strategy ==  "strategy_1")
      res = strategy_1(trading_dates, rates, fx_spot, PL_d, PL_p, X_out)
  return(res)
}

