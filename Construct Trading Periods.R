construct_trading_periods <- function()
{
  #create data frame to store dates
  trading_periods <- read.csv("C://Users//Konstantin//Desktop//On hand//Europe Finance//Data From BL//EURUSD Trading Periods.csv")

  trading_periods$first_day = as.Date(trading_periods$first_day, format="%m/%d/%Y")
  trading_periods$last_day = as.Date(trading_periods$last_day, format="%m/%d/%Y")


  return(trading_periods)
}
