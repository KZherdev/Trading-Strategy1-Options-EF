#Download data from bloomberg block
library(RBloomberg)
conn <- blpConnect()

#Set dates
start.date <- as.Date("2016/12/01", format="%Y/%m/%d")
end.date <- as.Date("2017/03/06", format="%Y/%m/%d")
dates = seq(start.date, end.date, by="days")

#download data from BLM
Interest_Rates_Get_Data(dates) #stored in csv
FXSpot_Get_Data(dates) #stored in csv

Volatility_Surface_Delta_2(dates) #create delta VS for every date, stored in CSV
Volatility_Surface_Strikes_2(dates) #create strike VS for every date, stored in CSV

#Load interest rates and forex here
df_rates = load("Interest_Rates_All_Dates_.csv")
df_fxspot = load("Forex_Spot_All_Dates.csv")

#Choose option and trading date
Buy_Expiration = as.Date("2017/03/01", format="%Y/%m/%d")#option expiration, some fixed date, string format
Buy_Delta #option delta when we buy it
Buy_date = start.date #day when we buy option

#Check 1
#Set arguments for price and strike check


interp_method = "Linear Interpolation"
file_name = paste(Buy_date, "_Delta_VS.csv", sep = "")
Buy_VSDelta = load(file_name)


r_d_date = df_rates["us", where df_rates["date"] = Buy_date]#check
r_f_date = df_rates["eur", where df_rates["date"] = Buy_date]#check
fxspot_date = df_fxspot["spot", where df_fxspot["date"] = Buy_date ]#check

price = get_option_price_delta(Buy_Delta, Buy_date, Buy_Expiration, Buy_VSDelta , r_d_date, r_f_date, fxspot_date, interp_method)
Strike = get_option_strike_delta(Buy_Delta, Buy_date, Buy_Expiration, Buy_VSDelta , r_d_date, r_f_date, fxspot_date, interp_method)

#Check 2

#choose new date
today = "2016-12-06"

#calculate the price of the option with the same expiration, option type also 
file_name = paste(today, "_Strike_VS.csv", sep = "")
today_VSStrike = load(file_name)
price = get_option_price_strike(today, Buy_Expiration, Strike,  r_d_date, r_f_date, fxspot_date, interp_method)

#compare this price with Bl market price