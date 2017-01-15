strategy_1 <- function(trading_dates, trading_period, rates, fx_spot, PL_d, PL_p, X_out)
{
  #Period  55 ok
  #Target = 1.365
  
  #Period  54
  #Target = 1.32
  
  #Period  53 ok
  Target = 1.345
  
  #Period  52  ok
  #Target = 1.35
  
  #Period  51 ok
  #Target = 1.330
  
  Nominal = 10000000 #100Евро
  rate_1 = 0.05
  rate_2 = 0.1
  N1 = Nominal*rate_1
  N2 = Nominal*rate_2
  delta_0 = 0.35
  delta_s = 0.25
  expiration = 30
  
  
  #Initialize PL in derivative and physical position, initialize Strike_0
  date = trading_dates[1]
  #print(date)
  Strike_0 = Determine_Zero_Strike(date, path_BLData, delta_0, expiration, rates, fx_spot)
  vol_0 = Determine_Option_Vol_Strike(date, Strike_0, expiration)
  Strike_initial = Strike_0
  c = Calculate_Option_Price_Delta(date, rates, fx_spot, delta_0, expiration)*Nominal
  
  PL_d = -1*c
  initial_price = c
  #print(paste("option initial price = ", PL_d, sep = ""))
  PL_p = 0;
  X_out = 0;
  
  #create intervals and determine some triggers when we enter trade 
  Spot_0  = fx_spot$Spot[1]
  print(paste("spot at beginning = ", Spot_0, sep = ""))
  s = create_intervals(Spot_0, 0.005, 3)
  print(s)
  int_0 = determine_interval(Spot_0, s)
  int_u = int_0 + 1
  int_d = int_0 - 1
  s_u = s[int_u]
  s_d = s[int_d + 1]
  
  #if rolling took place
  c_close_if_rolling_took_place = 0
  c_open_if_rolling_took_place = 0
  number_of_rolling_times = 0
  PL_d_after_rolling = 0
  
  count  = 0

  print("Start trading!")
  
  #if starting date in periods is holiday, then we exclude this day
  #for saturday
  if(is.na(match(trading_period[1], trading_dates)))
  {
    trading_period = trading_period[2:length(trading_period)]
  }
  
  #for sunday
  if(is.na(match(trading_period[1], trading_dates)))
  {
    trading_period = trading_period[2:length(trading_period)]
  }
  
  #for(i in 1:10)
  for(i in 1:length(trading_period)) #move along trading dates, then adjust hours; could have done the opposite
  {
    print(paste("______________Trading period day_______________ ", i, sep = "" ))
    #print(date)
    #print(paste("expiration = ", expiration, sep = ""))
    #print(paste("Strike for open derivative = ", Strike_0, sep = ""))
    
    count = count + 1
    
    
    #if given trading period day is working day - then trade; 
    #we should walk through trading_period days, since it is easier to control for expiration in this case
    date_period = trading_period[i]
    if(!is.na(match(date_period, trading_dates)))
    {
    index = findInterval(date_period, trading_dates)
    date = trading_dates[index]
    date_format = format(date, format = "%d.%m.%Y")

    print(paste("trading date: ", date, sep = ""))
    print(paste("trading period date: ", trading_period[i], sep = ""))
    #print(trading_period)
    #print(paste("date_format: ", date_format, sep = ""))
    
    fx_spot_i = fx_spot[fx_spot$date == date, ] #convenient way not to avoid deals with hours
    #View(fx_spot_i)
    # print(paste("rate_d = ", rate_d, " rate_f = ", rate_f, sep = ""))
    # print(paste("vol = ", vol, sep = ""))

    
    if(i == 1)
    {
      PL_p = fx_spot_i$Spot[1]*Nominal*delta_s
      X_out = -1*Nominal*delta_s

    }
    #if( count%% 14 == 0)
    if( count%% 14 == 0 & (fx_spot_i$Spot[1] - Spot_0 <= 0))#for call option, if spot increases, then call also increases in value and we don't want to roll the option after 2 weeks
    {
      #if(fx_spot_i$Spot[1] - Spot_0 <= 0)
      #{
      #expiration updated at the end
      c_2w = Calculate_Option_Price_Strike(date, rates, fx_spot, 1, Strike_0, expiration)*Nominal #fx_spot - hourly data
      c_close_if_rolling_took_place = c_2w
      
      #buy new - update strike_0 for the second rolling
      Strike_0 = Determine_Zero_Strike(date, path_BLData, delta_0, expiration = 30, rates, fx_spot)
      c_4w = Calculate_Option_Price_Strike(date, rates, fx_spot, 1, Strike_0, expiration = 30)*Nominal
      
      c_open_if_rolling_took_place = c_4w
      
      expiration = 30
      PL_d = PL_d + c_2w - c_4w
      
      PL_d_after_rolling = PL_d
      number_of_rolling_times = number_of_rolling_times + 1
     # }
    }

    #spot_c = 0
    #spot_p = 0
    for(j in 1:(nrow(fx_spot_i)))
    {
      print(paste("______________Trading hour_______________ ", j, sep = "" ))
      #print(paste("Trading date: ", date))
      #print(trading_period)
      #print(s)
      fxspot = fx_spot_i$Spot[j]
      print(paste("fxspot = ", fxspot, sep = ""))
      
      #condition when we exit trade; Why do we need i >= 2 here?
      if(i >= 2 & (abs(fx_spot_i$Spot[j] - Target) <= 0.001 || (i == length(trading_period) & j == nrow(fx_spot_i) ) ) )
      {

        if(i >= 2 & (abs(fx_spot_i$Spot[j] - Target) <= 0.001))
        {
          print("______________Trading terminated after the spot triggered upper barier________________")
          #print("                                                                                      ")
        }
        
        if(i == length(trading_period))
        {
          print("______________Trading terminated due to the end of the sample period________________")
          #print("                                                                                    ")
        }
        print(paste("Trading terminated at ", date, "; Trading periods are ", trading_period[1], " - ", trading_period[length(trading_period)], sep = "" ))
        
        print("______________Rolling Results________________")
        #print("                                             ")
        print(paste("number_of_rolling_times = ", number_of_rolling_times, sep = ""))
        print(paste("Close price at rolling = ", c_close_if_rolling_took_place, sep = ""))
        print(paste("Open price at rolling = ", c_open_if_rolling_took_place, sep = ""))
        print(paste("Derivative PL after rolling = ", PL_d_after_rolling, sep = ""))
        print("       ")
        
        #close physical and derivative position; close in current price or j+1
        print(paste("initial spot = ", Spot_0, sep = ""))
        print(paste("initial vol = ", vol_0, sep = ""))
        print(paste("initial strike = ", Strike_initial, sep = ""))
        print(paste("Derivative price at the beginning of trading = ", initial_price,  sep = ""))
        
        #print("    ")
        c_cur = Calculate_Option_Price_Strike(date, rates, fx_spot, j, Strike_0, expiration)*Nominal #calc price fir given optin
        print(paste("Derivative current price: = ", c_cur, sep = ""))
        print(paste("PL on derivative before closing position = ", PL_d, sep = ""))
        
        
        
        #print("                                             ")

        print(paste("______________ TRADING RESULTS FOR GIVEN PERIOD: from ", trading_period[1], " to ", trading_period[length(trading_period)], "________________", sep = ""))
        print("                                                                       ")
        
        PL_d = PL_d + c_cur #close derivative position
        PL_p = PL_p + X_out*fx_spot_i$Spot[j] #close physical position
        print(paste("PL on derivative Position = ", PL_d, sep = ""))
        print(paste("PL on physical Position = ", PL_p, sep = ""))
        print(paste("Total PL on trading = ", PL_d + PL_p, sep = ""))
        
        
        x = 0
        #stopifnot(x = 1)
        if(x == 0)#http://r.789695.n4.nabble.com/how-to-stop-without-error-message-td866604.html
        {
          opt <- options(show.error.messages=FALSE) 
          on.exit(options(opt)) 
          stop()
        }
      }
      
      if((i == 1 & j >= 2)|| i >= 2) #do not do anything the fist time we buy option
      {
      
        if(j != nrow(fx_spot_i) & j != 1) #this condition is hit first; assume more than one hourly observation
        {
          spot_c = fx_spot_i$Spot[j]
          spot_p = fx_spot_i$Spot[j-1]
          
          #print("state 2")
          #print(paste("spot_p =", spot_p, sep = ""))
          #print(paste("spot_c =", spot_c, sep = ""))
          
          #PL_p = PL_p + buy_sell_flow(spot_p, spot_c, s, int_u, int_d, N1, N2, X_out)
          
        }
        
        if(j == nrow(fx_spot_i)) #this cobdition is hit second
        {
          spot_c = fx_spot_i$Spot[j]
          spot_p = fx_spot_i$Spot[j-1]
          
          #print("state 3") 
          #print(paste("spot_p =", spot_p, sep = ""))
          #print(paste("spot_c =", spot_c, sep = ""))
          #PL_p = PL_p + buy_sell_flow(spot_p, spot_c, s, int_u, int_d, N1, N2, X_out)
        }
        
        if(j == 1) #this condition is hit third
        {
          spot_c = fx_spot_i$Spot[j] #spot_p is determined from previous condition, it should remain till here
          
          date_temp = trading_dates[index-1] #always exists, if index exists, it does
          fx_spot_temp = fx_spot[fx_spot$date == date_temp, ]
          n = nrow(fx_spot_temp)
          spot_p = fx_spot_temp$Spot[n]
          
          #print("state 4")
          #print(paste("spot_p =", spot_p, sep = ""))
          #print(paste("spot_c =", spot_c, sep = ""))
          
          #PL_p = PL_p + buy_sell_flow(spot_p, spot_c, s, int_u, int_d, N1, N2, X_out)
        }
        X_out = X_out + Change_In_Physical_Position(spot_p, spot_c, s, int_0, int_u, int_d, N1, N2)
        PL_p = PL_p + buy_sell_flow(spot_p, spot_c, s, int_0, int_u, int_d, N1, N2)
        #print(paste("total physical PL to now = ", PL_p, sep = ""))
        #print(paste("total open position in Euro to now = ", X_out, sep = ""))
        
      }
      else
      {
        print("state 1")
      }
    
    }

    }
    expiration = expiration - 1
  }
  #print(s)
}


#creates a list with interval bounds
create_intervals <- function(Spot_0, step, n_one_way)
{
  l = Spot_0 - n_one_way*step
  u = Spot_0 + n_one_way*step
  s = seq(l,u,step)
  remove <- c(Spot_0)
  s = s[! s %in% remove] #http://stackoverflow.com/questions/9665984/how-to-delete-multiple-values-from-a-vector
  #print(s)
  return(s)
  
}

#determine interval number for given spot value
determine_interval <- function(Spot, s)
{
  x = findInterval(Spot,s) #returns index of those element, that is lower bound
  return(x)
}


buy_sell_flow<- function(Spot_p, Spot_c, s, int_0, int_u, int_d, N1, N2)#CF from given stock movement
{
  omega_p = determine_interval(Spot_p, s)
  #print(paste("interval =",  omega_p, sep = ""))
  
  omega_c = determine_interval(Spot_c, s)
  #print(paste("interval =",  omega_c, sep = ""))
  
  
  
  #Calculate number of intervals the stock moved in one period
  l = min(omega_c, omega_p)
  h = max(omega_c, omega_p)
  
  I = 0
  if(omega_c > omega_p)  #different for call and put options 
    I = 1 #we short if spot goes up, positive cash inflow
  else if (omega_c < omega_p)
    I = -1
  
  res = 0
  
  if(h > l) # if the stock triggered at least one of our bariers; here h and ll are absolut values
  {
    if( (omega_c == int_u & I == 1) || (omega_c == int_d & I == -1) || (omega_c == int_0 & I == -1) || (omega_c == int_0 & I == 1))
    {
      res = N1*Spot_c
      #X_out = X_out + -1*I*N1 #if goes up - we sell euro
    }
    else
    {
      res = N2*Spot_c
      #X_out = X_out + -1*I*N2
    }
    res = I*res
  }
  
  #print(paste("CF for given movement = ", res, sep = ""))
  return(res)
  
}

Change_In_Physical_Position <- function(Spot_p, Spot_c, s, int_0, int_u, int_d, N1, N2)
{
  omega_p = determine_interval(Spot_p, s)
  omega_c = determine_interval(Spot_c, s)
  
  #Calculate number of intervals the stock moved in one period
  l = min(omega_c, omega_p)
  h = max(omega_c, omega_p)
  
  I = 0
  if(omega_c > omega_p)  #different for call and put options 
    I = 1 #we short if spot goes up, positive cash inflow
  else if (omega_c < omega_p)
    I = -1
  
  change = 0
  if(h > l) # if the stock triggered at least one of our bariers; here h and l are absolut values; condition to check if we have switched at least interval
  {
    if( (omega_c == int_u & I == 1) || (omega_c == int_d & I == -1) || (omega_c == int_0 & I == -1) || (omega_c == int_0 & I == 1))
    {
      change = -1*I*N1 #if goes up - we sell euro
    }
    else
    {
      change = -1*I*N2
    }
  }
  
    #print(paste("X_out after new trade = ", change, sep = ""))
    return(change)
}