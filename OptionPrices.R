#general function to price some option given current date, expiration date (string format date when given option expires) and current market delta
#This function might be useful at the beginning of our program; also for testing purposes and at some arbitrary date we might be interested in the option price with some particular delta
source(VolatilitySurfaceGrid.R)
source(Interpolation.R)
source(BlackScholesFormulas.R)


#calculate the strike using current date, Expiration date, delta and interp. method
get_option_strike_and_vol_delta <- function(delta_date, date, Expire, df_vsdelta_date, r_d_date, r_f_date, fxspot_date, interp_method)
{

  ndays = as.numeric(Expire - date, units="days")
  n_workingdays = 252
  Expiration = ndays/n_workingdays;
  vol = 0
  
  if(interp_method == "Linear Interpolation")
    vol = LinearInterpolation(df_vsdelta_date, delta_date, ndays) #option type, easier to search
  
  fx_forward = fxspot_date*exp((r_d_date-r_f_date)*Expiration)
  
  strike_min <- uniroot(BlackScholes76CallDelta - delta, c(0.5,3), tol = 0.0001, Forward = fx_forward, Vol = vol, Expiry = Expiration)
  strike = strike_min$root
  
  res = c(vol, strike)
  return(res)
}


#get_option_price_strike - the same, except that iv curve is plotted against strikes, or cleaned data
get_option_price_delta <- function(delta_date, date, Expire, df_vsdelta_date, r_d_date, r_f_date, fxspot_date, interp_method) #add option type should be here
{
  res = get_option_strike_and_vol_delta(delta_date, date, Expire, df_vsdelta_date, r_d_date, r_f_date, fxspot_date, interp_method)
  
  vol = res[1]
  strike = res[2]
  price = BlackScholes76CallPrice(fxspot_date, strike, vol, r_d_date, r_f_date, Expiry)
  
  return(price)
}


#calculate the price using current date, Expiration date, Strike and interp. method
#get_option_price_delta - the same, except that iv curve is plotted against deltas, or raw blm data
get_option_price_strike <- function(date, Expire, Strike, df_vsstrike_date, r_d_date, r_f_date, fxspot_date, interp_method) #add option type should be here
{
  
  ndays = as.numeric(Expiration - date, units="days")
  n_workingdays = 252
  Expiration = ndays/n_workingdays;
  vol = 0
  
  if(interp_method == "Linear Interpolation")
    vol = LinearInterpolation(df_vsstrike_date, Strike, ndays) #option type, easier to search

  price = BlackScholes76CallPrice(fxspot_date, strike, vol, r_d_date, r_f_date, Expiry)
  return(price)
}


