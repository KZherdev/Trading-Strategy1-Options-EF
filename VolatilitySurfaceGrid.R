source(BlackScholesFormulas.R)

#This function converts delta x-axis to forward strike. Need to knwow forward rate, calculate in this function 
ConstructVolatilitySurfaceGridStrikes_date <- function(df_temp, fxspot, rates)
{

  n_workingdays = 252
  for (i in 0:nrow(df_temp))#skip through all points in the volatility surface
  {
    
    Expiration = df_temp[i, "Horizon"] #convert to days/365
    vol = df_temp[i, "Vols"] #different for every row
    delta = df_temp[i, "Delta"]
    r_usd = rates[1,"usd"]
    r_eur = rates[1,"eur"]
    fx_spot = df_fxspot[1, "Spot"]
    
    fx_forward = fx_spot*exp((r_usd-r_eur)*Expiration/n_workingdays)
    
    strike_min <- uniroot(BlackScholes76CallDelta - delta, c(0.5,3), tol = 0.0001, Forward = fx_forward, Vol = vol, Expiry = Expiration)
    strike = strike_min$root
    df_temp[i, "Strike"] = strike;
  }
  #remove delta column
  return(df_temp)
}

VolatilitySurfaceGridStrikes_1<- function()
{
  df_vs <- read.csv(file = "Volatility_Surface_All_Dates_Deltas.csv", header=TRUE, sep=",") #in deltas
  df_fxspot <- read.csv(file = "Forex_Spot_All_Dates.csv", header=TRUE, sep=",") #fx spot
  df_rates <- read.csv(file = "Interest_Rates_All_Dates_.csv", header=TRUE, sep=",") #domestic and foreign rates
  
  for(i in 1:nrow(df_vs))
  {
    df_temp = ConstructTableFromBLString()#some temporary dataframe for given date, vs data for given date
    #something like from dataframe to string
    fxspot;
    rates;
    df_strike = construct_VS_Grid_Strikes_date(df_temp, fxspot, rates)
    write.csv(df_strike, file_name, row.names=FALSE)#modify for each table has its own name or write to one only?
  }
}  

VolatilitySurfaceGridStrikes_2 <- function(dates)
{
  df_rates = load("Interest_Rates_All_Dates_.csv")
  df_fxspot = load("Forex_Spot_All_Dates.csv")
  
  for(i in 1:length(dates))
  {
    date = dates[i];
    file_name = paste(date, "_Delta_VS.csv", "")
    df_temp = load(file_name) #load delta VS for given date
    rates = df_rates[i] #both rates
    fxspot = df_fxspot[i]
    df_strike = construct_VS_Grid_Strikes_date(df_temp, fxspot, rates)
    file_name = paste(date, "_Strike_VS.csv", "")
    write.csv(df_strike, file_name, row.names=FALSE)#modify for each table has its own name or write to one only?
  }
}  

