#source("C://Users//Konstantin//Desktop//Version 3//BlackScholesFormulas.R")

VS_Grid_Strikes_BS_date <- function(VSDelta_date, spot, rate_d, rate_f)
{
  
  for (i in 1:nrow(VSDelta_date))
  {
    vol = VSDelta_date$ivol[i]
    delta = VSDelta_date$X[i]
    Expiration = VSDelta_date$Days_To_Expiry[i]/365
    
    strike_min <- uniroot(BlackScholesCallDelta - delta, c(0, 4), tol = 0.0001, Spot = spot,  r = rate_d, d = rate_f, Vol = vol, Expiry = Expiration)
    strike = strike_min$root
    
    VSDelta_date$Strike[i] = strike
    VSDelta_date$DeltaCheck[i] = BlackScholes76CallDelta(strike, fx_forward, vol, Expiration)
  }
  
  return(VSDelta_date)
}

#This function converts delta x-axis to forward strike. Need to knwow forward rate, calculate in this function 
VS_Grid_Strikes_BS76_date <- function(VSDelta_date, fx_spot, rate_d, rate_f)
{
  
  for (i in 1:nrow(VSDelta_date))
  {
    vol = VSDelta_date$ivol[i]
    delta = VSDelta_date$X[i]
    Expiration = VSDelta_date$Days_To_Expiry[i]/365
    fx_forward = fx_spot*exp((rate_d-rate_f)*Expiration)
    
    strike_min <- uniroot(BlackScholes76CallDeltaMDelta, c(0, 4), tol = 0.0001, Forward = fx_forward, Vol = vol, Expiry = Expiration, delta = delta)
    strike = strike_min$root
    VSDelta_date$Strike[i] = strike
    VSDelta_date$DeltaCheck[i] = BlackScholes76CallDelta(strike, fx_forward, vol, Expiration)
  }
  
  return(VSDelta_date)
}

#bloomberg gives data for weekends also; otw we would attempt to read weekend file and get an error
VolatilitySurfaceGridStrikes <- function(trading_dates, ticker, path_BLData, path_rates)
{
  #Load data for rates and for fx_spot
  #rates
  file_name = paste(path_rates, "Interest_Rates_historical_cleaned.csv", sep = "")
  rates = read.csv(file_name, header = TRUE)
  rates$date = as.Date(levels(rates$date), format="%Y-%m-%d")[rates$date] #Y should be capital, otherwise 2020
  #View(rates)
  
  #spot; in this setup fsxpot comes from metastock; hourly format; many hours for given date - we need only one  
  #Вообще лучше в любом формате иметь входные цены; разумно из обработать в clean, сохранить в формате день - цена, а здесь этот файл открыть.
  
  #file_name = paste(path_BLData, "Spot_history_cleaned_daily.csv", sep = "")
  file_name = paste(path_BLData, ticker, "daily_", trading_period[1], " to ", trading_period[length(trading_period)], "_cleaned.csv", sep = "")
  
  fx_spot_rd= read.csv(file_name, header = TRUE)
  fx_spot_rd$date = as.Date(levels(fx_spot_rd$date), format="%Y-%m-%d")[fx_spot_rd$date] #Y should be capital, otherwise 2020

  for(i in 1:length(trading_dates))
  {
    date = trading_dates[i]
    date_format = format(date, format = "%d.%m.%Y")
    file_name = paste(path_BLData, date_format, "_Delta_VS_cleaned.csv", sep = "")
    
    print(date)
    
    fx_spot = fx_spot_rd$Spot[fx_spot_rd$date == date]
    rate_d = rates$r_domestic[rates$date == date]
    rate_f = rates$r_foreign[rates$date == date]    
    VSDelta_date = read.csv(file_name, header = TRUE)
    
    df_strike = VS_Grid_Strikes_BS76_date(VSDelta_date, fx_spot, rate_d, rate_f)
    
    drops <- c("X", "DeltaCheck")
    df_strike = df_strike[, !(names(df_strike) %in% drops)]
    colnames(df_strike) <- c("Days_To_Expiry", "ivol", "X")
    
    file_name = paste(path_BLData, date_format, "_Strike_VS.csv", sep = "")
    write.csv(df_strike, file_name, row.names=FALSE)#modify for each table has its own name or write to one only?
  }
}  

