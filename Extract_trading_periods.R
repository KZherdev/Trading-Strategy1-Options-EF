#this function reads trading dates and extracts periods to Metastock raw data
construct_trading_periods <- function()
{
  #create data frame to store dates
  m_data <- data.frame(Day= double(), Hour = double(), Open = double(), High = double(), Low = double(), Close = double())
  m_data <- read.csv("C://Users//Konstantin//Desktop//On hand//Europe Finance//Data From BL//EURUSD Spot 2001-2006.csv")
  m_data$Day = as.Date(m_data$Day, format="%m/%d/%Y")
  View(m_data)
  #trading_periods$Day = as.Date(levels(trading_periods$Day), format="%m/%d/%Y")[trading_periods$Day] #remove levels before passing as argument, otw error
  View(trading_periods)
  i = 1
  View(m_data[m_data$Day >= trading_periods$first_day[i] & m_data$Day <= trading_periods$last_day[i], ]) #problem, data is still read incorrrectly
  
  trading_periods$Day[1]
}