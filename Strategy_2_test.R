library(plyr)
library(timeDate)

start_str.date <- as.Date("2013/09/04", format="%Y/%m/%d")
end_str.date <- as.Date("2013/10/24", format="%Y/%m/%d")

start_str.date <- as.Date("2013/03/10", format="%Y/%m/%d")
end_str.date <- as.Date("2013/03/27", format="%Y/%m/%d")

trading_period = seq(start_str.date, end_str.date, by="days") #the general period where we test model
trading_dates<- trading_period[isWeekday(trading_period)]; #actual trading dates within the period; remove weekends

path_BLData = paste("C://Users//Konstantin//Desktop//On hand//Europe Finance//Data From BL//Trading period ", trading_period[1], " to ", trading_period[length(trading_period)], "//", sep = "")
path_BLData

file_name = paste(path_BLData, ticker, "historical_", trading_period[1], " to ", trading_period[length(trading_period)], "_cleaned.csv", sep = "")
fx_spot = read.csv(file_name, header = TRUE)
View(fx_spot)
fx_spot$date = as.Date(levels(fx_spot$date), format="%Y-%m-%d")[fx_spot$date] #Y should be capital, otherwise 2020; #remove levels before passing as argument, otw error

Nominal = 10000000 #100Евро
rate_1 = 0.05
rate_2 = 0.1
N1 = Nominal*rate_1
N2 = Nominal*rate_2
delta_s = 0.25

pi_p = "0"
pi_c = "0"
Spot_0 = fx_spot$Spot[1]
Spot_0

#so far
#CF = delta_s*Nominal*Spot_0 
#X_out = -delta_s*Nominal


CF = 0 
X_out = 0

create_intervals <- function(Spot_0, step, n_one_way)
{
  l = Spot_0 - n_one_way*step
  u = Spot_0 + n_one_way*step
  s = seq(l,u,step)
  return(s)
}

s = create_intervals(Spot_0, 0.005, 4) #for convenience to trade
s
l = 400
source("s2_trade.R")
s2_trade(fx_spot, X_out, CF, s, l)

View(fx_spot)


















