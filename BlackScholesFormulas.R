BlackScholesCall<-function(Spot, Strike, r, d, Vol, Expiry)
{
  standardDeviation = Vol*sqrt(Expiry);
  moneyness = log(Spot/Strike);
  d1 =( moneyness +  (r-d)*Expiry+0.5* standardDeviation*standardDeviation)/standardDeviation;
  d2 = d1 - standardDeviation;
  return (Spot*exp(-d*Expiry) * pnorm(d1, 0, 1) - Strike*exp(-r*Expiry)*pnorm(d2, 0, 1));
}

BlackScholesCallDelta<-function(Strike, Spot, r, d, Vol, Expiry)
{
  standardDeviation = Vol*sqrt(Expiry);
  moneyness = log(Spot/Strike);
  d1 =( moneyness +  (r-d)*Expiry+0.5* standardDeviation*standardDeviation)/standardDeviation;
  return (exp(-d*Expiry) * pnorm(d1, 0, 1) );
}



BlackScholes76CallPrice <-function(fxspot, Strike, Vol, r_d, r_f, Expiry)
{
  
  Forward = fxspot*exp((r_d-r_f)*Expiry)
  standardDeviation = Vol*sqrt(Expiry);
  moneyness = log(Forward / Strike);
  #moneyness
  d1 = (moneyness + 0.5* standardDeviation*standardDeviation) / standardDeviation;
  d2 = d1 - standardDeviation;
  #return(fxspot*exp(-r_f*Expiry) * pnorm(d1, 0, 1) - Strike*exp(-r_d*Expiry)*pnorm(d2, 0, 1))
  return(exp(-r_d*Expiry)*(Forward*pnorm(d1, 0, 1) - Strike*pnorm(d2, 0, 1)))
}

BlackScholes76PutPrice <-function(Forward, Strike, Vol, Expiry)
{
  Forward = fxspot*exp((r_d-r_f)*Expiry)
  standardDeviation = Vol*sqrt(Expiry);
  moneyness = log(Forward / Strike);
  d1 = (moneyness + 0.5* standardDeviation*standardDeviation) / standardDeviation;
  d2 = d1 - standardDeviation;
  return(-1*fxspot*exp(-r_f*Expiry) * pnorm(-1*d1, 0, 1) + Strike*exp(-r_d*Expiry)*pnorm(-1*d2, 0, 1))
  
}

BlackScholes76CallDelta <-function(Strike, Forward, Vol, Expiry)
{
  standardDeviation = Vol*sqrt(Expiry);
  moneyness = log(Forward / Strike);
  d1 = (moneyness + 0.5* standardDeviation*standardDeviation) / standardDeviation;
  return(pnorm(d1,0,1))
}

BlackScholes76PutDelta<-function(Forward, Strike, Vol, Expiry)
{
  standardDeviation = Vol*sqrt(Expiry);
  moneyness = log(Forward / Strike);
  d1 = (moneyness + 0.5* standardDeviation*standardDeviation) / standardDeviation;
  return(-1*pnorm(-1*d1,0,1))
}

BlackScholes76CallDeltaMDelta <-function(Strike, Forward, Vol, Expiry, delta)
{
  standardDeviation = Vol*sqrt(Expiry);
  moneyness = log(Forward / Strike);
  d1 = (moneyness + 0.5*standardDeviation*standardDeviation)/standardDeviation;
  return(pnorm(d1,0,1) - delta)
}

BlackScholes76CallPercDelta <-function(Strike, Forward, Vol, Expiry)
{
  standardDeviation = Vol*sqrt(Expiry);
  moneyness = log(Forward / Strike);
  d1 = (moneyness + 0.5*standardDeviation*standardDeviation)/standardDeviation;
  d2 = d1 - standardDeviation 
  return(Strike/Forward*pnorm(d2,0,1))
}


#for given date, option initial delta_0, expiration and market parameters determine strike for the given option
Determine_Zero_Strike <- function(date, path_BLData, delta_0, expiration, rates, fx_spot)
{
  #rates$date = as.Date(levels(rates$date), format="%Y-%m-%d")[rates$date]
  #fx_spot$date = as.Date(levels(fx_spot$date), format="%Y-%m-%d")[fx_spot$date] #Y should be capital, otherwise 2020
  
  start_str.date = date
  start_str.date_format = format(start_str.date, format = "%d.%m.%Y")
  
  file_name = paste(path_BLData, start_str.date_format, "_Delta_VS_cleaned.csv", sep = "")
  VS_Delta = read.csv(file_name, header = TRUE)
  vol = VSInterpolation(VS_Delta, delta_0, expiration, method = "linear")
  #print(vol)
  
  Expiration = expiration/365
  rate_d = rates$r_domestic[rates$date == start_str.date]
  rate_f = rates$r_foreign[rates$date == start_str.date]
  fxspot = fx_spot$Spot[1] # take spot directly 
  fx_forward = fxspot*exp((rate_d-rate_f)*Expiration)
  
  print(paste("rate_d = ", rate_d, sep = ""))
  print(paste("rate_f = ", rate_f, sep = ""))
  #Determine strike for call option with delta_0 and calculated volatility
  strike_min <- uniroot(BlackScholes76CallDeltaMDelta, c(0, 4), tol = 0.0001, Forward = fx_forward, Vol = vol, Expiry = Expiration, delta = delta_0)
  Strike_0 = strike_min$root
  print(paste("Initial strike = ", Strike_0, sep = ""))
  return(Strike_0)
}

Calculate_Option_Price_Delta<- function(date, rates, fx_spot, delta_0, expiration) #fx_spot - hourly data
{
  print("ok")
  #View(rates)
  #print(rates$date[2])
  #rates$date = as.Date(levels(rates$date), format="%Y-%m-%d")[rates$date]
  #fx_spot$date = as.Date(levels(fx_spot$date), format="%Y-%m-%d")[fx_spot$date] #Y should be capital, otherwise 2020
  
  fx_spot_temp = fx_spot[fx_spot$date == date, ] #subset for given date in hours
  fxspot = fx_spot_temp$Spot[1] #the first hourly price
  rate_d = rates$r_domestic[rates$date == date]
  rate_f = rates$r_foreign[rates$date == date]
  Expiration = expiration/365
  fx_forward = fxspot*exp((rate_d-rate_f)*Expiration) #for illustration
  
  print(paste("date: ", date, sep = ""))
  print(paste("rate_d = ", rate_d, " rate_f = ", rate_f, sep = ""))
  print(paste("fx_forward = ", fx_forward, sep = ""))
  Strike_0 <- Determine_Zero_Strike(date, path_BLData, delta_0, expiration, rates, fx_spot)#might be levels in rates
  
  
  date_format = format(date, format = "%d.%m.%Y")
  file_name = paste(path_BLData, date_format, "_Delta_VS_cleaned.csv", sep = "")
  
  VS_Delta = read.csv(file_name, header = TRUE)
  vol = VSInterpolation(VS_Delta, delta_0, expiration, method = "linear")
  Expiration = expiration/365
  
  c = BlackScholes76CallPrice(fxspot, Strike_0, vol, rate_d, rate_f, Expiration);
  return(c)
}

Calculate_Option_Price_Strike<- function(date, rates, fx_spot, k, Strike_0, expiration) #fx_spot - hourly data
{
  #rates$date = as.Date(levels(rates$date), format="%Y-%m-%d")[rates$date]
  #fx_spot$date = as.Date(levels(fx_spot$date), format="%Y-%m-%d")[fx_spot$date] #Y should be capital, otherwise 2020
  
  fx_spot_temp = fx_spot[fx_spot$date == date, ] #subset for given date in hours
  fxspot = fx_spot_temp$Spot[k] #the first hourly price
  rate_d = rates$r_domestic[rates$date == date]
  rate_f = rates$r_foreign[rates$date == date]
  
  print("Calculate derivative price with the folowing parameters: ")
  print(paste("date: ", date, sep = ""))
  print(paste("fxspot = ", fxspot, sep = ""))
  print(paste("rate_d = ", rate_d, " rate_f = ", rate_f, sep = ""))
  
  date_format = format(date, format = "%d.%m.%Y")
  file_name = paste(path_BLData, date_format, "_Strike_VS.csv", sep = "")
  
  VS_Strike = read.csv(file_name, header = TRUE)
  vol = VSInterpolation(VS_Strike, Strike_0, expiration, method = "linear")
  print(paste("vol = ", vol, sep = ""))
  print(paste("Strike for open derivative = ", Strike_0, sep = ""))
  
  Expiration = expiration/365
  print(paste("Expiration in days = ", expiration, sep = ""))
  
  c = BlackScholes76CallPrice(fxspot, Strike_0, vol, rate_d, rate_f, Expiration);
  #print(paste("Derivative current price: = ", c, sep = ""))
  return(c)
  
}

Determine_Option_Vol_Strike<- function(date, Strike_0, expiration) #fx_spot - hourly data
{

  date_format = format(date, format = "%d.%m.%Y")
  file_name = paste(path_BLData, date_format, "_Strike_VS.csv", sep = "")
  
  VS_Strike = read.csv(file_name, header = TRUE)
  vol = VSInterpolation(VS_Strike, Strike_0, expiration, method = "linear")
  return(vol)
  
}

