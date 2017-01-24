strategy_3 <- function(path_strategy_folder, trading_dates, trading_period, rates, fx_spot, PL_d, PL_p, X_out, Target)
{

  #in this version I don't want to use trading_dates and trading_period
  
  expiration = 30
  Nominal = 10000000 #100Åâğî
  rate_1 = 0.05
  rate_2 = 0.1
  N1 = Nominal*rate_1
  N2 = Nominal*rate_2
  delta_s = 0.25
  delta_0 = 0.35
  pi_p = "0" #state variable
  pi_c = "0" #state variable
  
  #Initialize PL in derivative and physical position, initialize Strike_0
  #date = trading_dates[1]
  
  date = fx_spot$date[1]
  date_enter = date #for rolling purpose; initialize with first date 
  
  Strike_0 = Determine_Zero_Strike(date, path_strategy_folder, delta_0, expiration, rates, fx_spot)
  vol_0 = Determine_Option_Vol_Strike(path_strategy_folder, date, Strike_0, expiration)
  Strike_initial = Strike_0
  c = Calculate_Option_Price_Delta(path_strategy_folder, date, rates, fx_spot, delta_0, expiration)*Nominal
  
  PL_d = -1*c
  initial_price = c
  #print(paste("option initial price = ", PL_d, sep = ""))
  # fx_spot_temp_temp = fx_spot[fx_spot$date == trading_dates[1], ]
  # Spot_0 = fx_spot_temp_temp$Spot[1]
  
  Spot_0 = fx_spot$Spot[1]
  
  CF = 0
  X_out = 0
  CF_0 = delta_s*Nominal*Spot_0;
  X_out_0 = -delta_s*Nominal
  
  
  #create intervals
  s = create_intervals_str_2(Spot_0, 0.005, 4) #for convenience to trade
  
  #if rolling took place
  c_close_if_rolling_took_place = 0
  c_open_if_rolling_took_place = 0
  number_of_rolling_times = 0
  PL_d_after_rolling = 0
  rolling_date = 0
  rolling_spot = 0
  rolling_expiration = 0
  rolling_open_vol = 0
  rolling_close_vol = 0
  count  = 0
  
  #expiration_date = trading_dates[1] + 30 #fixed for the period of trading; in case of rolling we will change expiration date
  
  expiration_date = date + 30 #first date available for spot prices
  
  print("Start trading!")
  
  #here we store the results only for hours within trading days
  df_temp <- data.frame(date = double(), Trade_period = double(), Previous_state  = double(), Spot = double(), Interval_id = double(), New_state = double(), CF = double(), X_out = double())
  t_hours = 1 #physical number of days we trade

  df_temp <- rbind(df_temp, data.frame(date = date, Trade_period = t_hours, Previous_state  = "NA", Spot = Spot_0, Interval_id = 5, New_state = "0", CF = CF, X_out = X_out))
  
  date_prev = date-1 #previous trading day - at the beginning it is out of our scope
  switch = 0

  df_rolling <- data.frame(ROlling_date = double(), Rolling_spot = double(), Strike_old = double(), Rolling_expiration_old = double(), Close_vol = double(), Close_price = double(), Strike_new = double(), Rolling_expiration_new = double(), Open_vol = double(), Open_price = double())
  
  #add derivative purchase at the beginning
  df_rolling <- rbind(df_rolling, data.frame(Rolling_date = fx_spot$date[1], Rolling_spot = fx_spot$Spot[1], Strike_old = 0, Rolling_expiration_old = 0, Close_vol = 0, Close_price = 0, Strike_new = Strike_0, Rolling_expiration_new = 30, Open_vol = vol_0, Open_price = c))
  
  #for(j in 1:451)
  for(j in 2:nrow(fx_spot)) #fx_spot is already determined for trading period
  {
    
    date = fx_spot$date[j]#This is trading date, because prices are available
    spot = fx_spot$Spot[j]
    
    date_c = date #current trading day
    if(date_c != date_prev) #whether we have switched to new day; need to update expiration
    {
       switch = 1
    }
    else
    {
      switch = 0
    }
    
    x = findInterval(spot, s) #returns index of those element, that is lower bound
    omega = determine_interval(spot, s)
      
    print("")
    print(paste("Iteration number ", t_hours, "; date = ", date, sep = ""))
    print(paste("expiration in days for open derivative: ", as.numeric(expiration_date - date, unit = "days"), sep = ""))
    print(paste("spot = ", spot, sep = ""))
    print(paste("previous state: ", pi_p, "; Spot movement: ", spot, " Interval id: ", omega, sep = ""))
    print(paste("date enter: ", date_enter, sep = ""))
    print(s)
    count = as.numeric(date - fx_spot$date[1], units = "days") #we buy option in first trading day - therefore in two weeks this will also be a trading day; this function counts weekends
    #count is independent of expiration
    
    #rolling condition - roll in the beginning of new day
    if( (count%% 14 == 0 & (spot - Spot_0 <= 0)  & switch == 1 & date != fx_spot$date[1]) || expiration_date - date == 1 )#for call option, if spot increases, then call also increases in value and we don't want to roll the option after 2 weeks
    {
        #expiration updated at the end
      expiration = 30 - as.numeric(date - date_enter, units = "days")
      rolling_date = date
      rolling_spot = spot
      rolling_expiration = expiration
      rolling_close_vol = Determine_Option_Vol_Strike(path_strategy_folder, date, Strike_0, expiration)
      #print(paste("close vol = ", rolling_close_vol, sep = "")) 
      #print(paste("exp = ", expiration_date - date, sep = ""))
      c_2w = Calculate_Option_Price_Strike(path_strategy_folder, date, rates, fx_spot, 1, Strike_0, expiration)*Nominal #fx_spot - hourly data
      c_close_if_rolling_took_place = c_2w
      Strike_old = Strike_0
      
      # rolling_date = date
      # rolling_spot = spot
      # rolling_expiration = expiration
      date_enter = date #update date_enter to account for the second and next rolling  
      
        #buy new - update strike_0 for the second rolling
      Strike_0 = Determine_Zero_Strike(date, path_strategy_folder, delta_0, expiration = 30, rates, fx_spot) #here the strike is determined at the beginning of trading period
      rolling_open_vol = Determine_Option_Vol_Strike(path_strategy_folder, date, Strike_0, expiration = 30)
      c_4w = Calculate_Option_Price_Strike(path_strategy_folder, date, rates, fx_spot, 1, Strike_0, expiration = 30)*Nominal
      c_open_if_rolling_took_place = c_4w
      Strike_new = Strike_0  
      
      #expiration = 30
      
      PL_d = PL_d + c_2w - c_4w
        
      PL_d_after_rolling = PL_d
      number_of_rolling_times = number_of_rolling_times + 1
      
      expiration_date = date + 30 #new updated expiration date
      df_rolling <- rbind(df_rolling, data.frame(Rolling_date = date, Rolling_spot = spot, Strike_old = Strike_old, Rolling_expiration_old = rolling_expiration, Close_vol = rolling_close_vol, Close_price = c_close_if_rolling_took_place, Strike_new = Strike_new, Rolling_expiration_new = 30, Open_vol = rolling_open_vol, Open_price = c_open_if_rolling_took_place))
    }
      
      
      #condition when we exit trade;
    if( abs(spot - Target) <= 0.001 || date == fx_spot$date[nrow(fx_spot)] ) #fx_spot is already determined for trading period
    {
        
      if(abs(spot - Target) <= 0.001 )
      {
        print(" ")
        print(" ")
        print("______________Trading terminated after the spot triggered upper barier________________")
          #print("                                                                                      ")
      }
        
      if( date == trading_dates[length(trading_dates)] )
      {
        print(" ")
        print(" ")
        print("______________Trading terminated due to the end of the sample period________________")
          #print("                                                                                    ")
      }
      print(paste("Trading terminated at ", date, "; Trading periods are ", fx_spot$date[1], " - ", fx_spot$date[nrow(fx_spot)], sep = "" ))
      print(paste("Trigger = ", Target, sep = "")) 
      print("______________Rolling Results________________")
        #print("                                             ")
      print(paste("number_of_rolling_times = ", number_of_rolling_times, sep = ""))
      # print(paste("rolling date ", rolling_date, sep = ""))
      # print(paste("rolling spot ", rolling_spot, sep = ""))
      # print(paste("rolling close vol ", rolling_close_vol, sep = ""))
      # print(paste("rolling open vol ", rolling_open_vol, sep = ""))
      # print(paste("rolling expiration ", rolling_expiration, sep = ""))
      # print(paste("Close price at rolling = ", c_close_if_rolling_took_place, sep = ""))
      # print(paste("Open price at rolling = ", c_open_if_rolling_took_place, sep = ""))
      # print(paste("Derivative PL after rolling = ", PL_d_after_rolling, sep = ""))
      # print("       ")
        
        #close physical and derivative position; close in current price or j+1
      # print(paste("initial spot = ", Spot_0, sep = ""))
      # print(paste("initial vol = ", vol_0, sep = ""))
      # print(paste("initial strike = ", Strike_initial, sep = ""))
      # print(paste("Derivative price at the beginning of trading = ", initial_price,  sep = ""))
      #   
        #close option the FIRST DAY PRICE if exit without trigger and on current price, if triggered
      expiration = as.numeric(expiration_date - date, units = "days")
      
      c_cur = Calculate_Option_Price_Strike_spot(path_strategy_folder, date, rates, spot, Strike_0, expiration)*Nominal #calc price fir given optin
      
      #add derivative purchase at the beginning
      close_vol = Determine_Option_Vol_Strike(path_strategy_folder, date, Strike_0, expiration)
      
      df_rolling <- rbind(df_rolling, data.frame(Rolling_date = date, Rolling_spot = spot, Strike_old = Strike_0, Rolling_expiration_old = expiration, Close_vol = close_vol, Close_price = c_cur, Strike_new = 0, Rolling_expiration_new = 0, Open_vol = 0, Open_price = 0))
      
      #for call?
      df_rolling <- rbind(df_rolling, data.frame(Rolling_date = date, Rolling_spot = 0, Strike_old = 0, Rolling_expiration_old = 0, Close_vol = 0, Close_price = sum(df_rolling$Close_price), Strike_new = Strike_0, Rolling_expiration_new = 30, Open_vol = vol_0, Open_price = -1*sum(df_rolling$Open_price)))
      
      
      
      # print(paste("Derivative current price: = ", c_cur, sep = ""))
      # print(paste("PL on derivative before closing position = ", PL_d, sep = ""))
      #   
      print(paste("______________ TRADING RESULTS FOR GIVEN PERIOD: from ", trading_period[1], " to ", trading_period[length(trading_period)], "________________", sep = ""))
      print("                                                                       ")
        
      PL_d = PL_d + c_cur #close derivative position
      CF = CF + X_out*spot#close physical position
      print(paste("PL on derivative Position = ", PL_d, sep = ""))
      print(paste("PL on physical Position (active hedge) = ", CF, sep = ""))
        
      print(paste("PL on physical Position (inactive hedge) = ", CF_0 + X_out_0*spot, sep = ""))
      print(paste("PL on physical Position (total) = ", CF + CF_0 + X_out_0*spot, sep = ""))
        
      total = PL_d + CF + CF_0 + X_out_0*spot
      print(paste("Total PL on trading = ", total, sep = ""))
        
      df_results <- data.frame(PL_derivative = double(), PL_ph_active = double(), PL_ph_inactive  = double(), PL_ph_total = double(), PL_trade_total = double())
      df_results <- rbind(df_results, data.frame(PL_derivative = PL_d, PL_ph_active = CF, PL_ph_inactive  = CF_0 + X_out_0*spot, PL_ph_total = CF + CF_0 + X_out_0*spot, PL_trade_total = total))
      write.csv(df_results, file = paste(path_strategy_folder, "Strategy 3 trading results.csv", sep = ""), row.names = FALSE)
        #here we wright CF for trading excluding initial flow only
      df_temp <- rbind(df_temp, data.frame(date = fx_spot$date[j], Trade_period = t_hours, Previous_state  = pi_p, Spot = spot, Interval_id = omega, New_state = pi_c, CF = CF, X_out = X_out))
      x = 0
        #stopifnot(x = 1)
      if(x == 0)#http://r.789695.n4.nabble.com/how-to-stop-without-error-message-td866604.html
      {
      #View(df_temp)
      write.csv(df_rolling, file = paste(path_strategy_folder, "Strategy 3 Rolling results.csv", sep = ""))
      View(df_rolling)
        return(df_temp)
      opt <- options(show.error.messages=FALSE) 
      on.exit(options(opt)) 
      stop()
      }
      }
      
      
      ############################## 0
      if(pi_p == "0")
      {
        if(omega == 8)
        {
          X_out = X_out  - N2
          CF = CF + N2*spot
          pi_c = "pi_3"
        }
        if(omega == 7)
        {
          X_out = X_out  - N2
          CF = CF + N2*spot
          pi_c = "pi_2"
        }
        if(omega == 6)
        {
          X_out = X_out  - N1
          CF = CF + N1*spot
          pi_c = "pi_1"
        }
        if(omega == 3)
        {
          X_out = X_out  + N1
          CF = CF - N1*spot
          pi_c = "g_1"
        }
        if(omega == 2)
        {
          X_out = X_out  + N2
          CF = CF - N2*spot
          pi_c = "g_2"
        }
        if(omega == 1)
        {
          X_out = X_out  + N2
          CF = CF - N2*spot
          pi_c = "g_3"
        }
      }
      
      ############################## pi_1
      if(pi_p == "pi_1")
      {
        if(omega == 8)
        {
          X_out = X_out  - N2
          CF = CF + N2*spot
          pi_c = "pi_1 & pi_3"
        }
        if(omega == 7)
        {
          X_out = X_out  - N2
          CF = CF + N2*spot
          pi_c = "pi_1 & pi_2"
        }
        if(omega == 4)
        {
          X_out = X_out  + N1
          CF = CF - N1*spot
          pi_c = "0"
        }
        if(omega == 3)
        {
          X_out = X_out  + N1 + N1
          CF = CF - (N1 + N1)*spot
          pi_c = "g_1"
        }
        if(omega == 2)
        {
          X_out = X_out  + N1 + N2
          CF = CF - (N1 + N2)*spot
          pi_c = "g_2"
        }
        if(omega == 1)
        {
          X_out = X_out  + N1 + N2
          CF = CF - (N1 + N2)*spot
          pi_c = "g_3"
        }
        if(omega == 0)
        {
          X_out = X_out  + N1
          CF = CF - (N1)*spot
          pi_c = "0"
        }
      }
      
      ############################## pi_2
      if(pi_p == "pi_2")
      {
        if(omega == 8)
        {
          X_out = X_out  - N2
          CF = CF + N2*spot
          pi_c = "pi_2 & pi_3"
        }
        if(omega == 6)
        {
          X_out = X_out  - N1
          CF = CF + N1*spot
          pi_c = "pi_1 & pi_2"
        }
        if(omega == 4)
        {
          X_out = X_out  + N2
          CF = CF - N2*spot
          pi_c = "0"
        }
        if(omega == 3)
        {
          X_out = X_out  + N2 + N1
          CF = CF - (N2 + N1)*spot
          pi_c = "g_1"
        }
        if(omega == 2)
        {
          X_out = X_out  + N2 + N2
          CF = CF - (N2 + N2)*spot
          pi_c = "g_2"
        }
        if(omega == 1)
        {
          X_out = X_out  + N2 + N2
          CF = CF - (N2 + N2)*spot
          pi_c = "g_3"
        }
        if(omega == 0)
        {
          X_out = X_out  + N2
          CF = CF - (N2)*spot
          pi_c = "0"
        }
      }
      
      
      ############################## pi_3
      if(pi_p == "pi_3")
      {
        if(omega == 7)
        {
          X_out = X_out  - N2
          CF = CF + N2*spot
          pi_c = "pi_2 & pi_3"
        }
        if(omega == 6)
        {
          X_out = X_out  - N1
          CF = CF + N1*spot
          pi_c = "pi_1 & pi_3"
        }
        if(omega == 5 || omega == 4)
        {
          X_out = X_out  + N2
          CF = CF - N2*spot
          pi_c = "0"
        }
        if(omega == 3)
        {
          X_out = X_out  + N2 + N1
          CF = CF - (N2 + N1)*spot
          pi_c = "g_1"
        }
        if(omega == 2)
        {
          X_out = X_out  + N2 + N2
          CF = CF - (N2 + N2)*spot
          pi_c = "g_2"
        }
        if(omega == 1)
        {
          X_out = X_out  + N2 + N2
          CF = CF - (N2 + N2)*spot
          pi_c = "g_3"
        }
        if(omega == 0)
        {
          X_out = X_out  + N2
          CF = CF - (N2)*spot
          pi_c = "0"
        }
      }
      
      ############################## pi_1 & pi_2
      if(pi_p == "pi_1 & pi_2")
      {
        if(omega == 8)
        {
          X_out = X_out  - N2
          CF = CF + N2*spot
          pi_c = "pi_1 & pi_2 & pi_3"
        }
        if(omega == 4)
        {
          X_out = X_out  + N1 + N2
          CF = CF - (N1 + N2)*spot
          pi_c = "0"
        }
        if(omega == 3)
        {
          X_out = X_out + N1 + N2 + N1
          CF = CF - (N1+ N2 + N1)*spot
          pi_c = "g_1"
        }
        if(omega == 2)
        {
          X_out = X_out + N1 + N2 + N2
          CF = CF - (N1+ N2 + N2)*spot
          pi_c = "g_2"
        }
        if(omega == 1)
        {
          X_out = X_out + N1 + N2 + N2
          CF = CF - (N1+ N2 + N2)*spot
          pi_c = "g_3"
        }
        if(omega == 0)
        {
          X_out = X_out  + N1 + N2
          CF = CF - (N1 + N2)*spot
          pi_c = "0"
        }
      }
      
      
      ############################## pi_1 & pi_3
      if(pi_p == "pi_1 & pi_3")
      {
        if(omega == 7)
        {
          X_out = X_out  - N2
          CF = CF + N2*spot
          pi_c = "pi_1 & pi_2 & pi_3"
        }
        if(omega == 5)
        {
          X_out = X_out  + N2
          CF = CF - (N2)*spot
          pi_c = "pi_1"
        }
        if(omega == 4)
        {
          X_out = X_out  + N1 + N2
          CF = CF - (N1 + N2)*spot
          pi_c = "0"
        }
        if(omega == 3)
        {
          X_out = X_out + N1 + N2 + N1
          CF = CF - (N1+ N2 + N1)*spot
          pi_c = "g_1"
        }
        if(omega == 2)
        {
          X_out = X_out + N1 + N2 + N2
          CF = CF - (N1+ N2 + N2)*spot
          pi_c = "g_2"
        }
        if(omega == 1)
        {
          X_out = X_out + N1 + N2 + N2
          CF = CF - (N1+ N2 + N2)*spot
          pi_c = "g_3"
        }
        if(omega == 0)
        {
          X_out = X_out + N1 + N2
          CF = CF - (N1 + N2)*spot
          pi_c = "0"
        }
      }
      
      
      ############################## pi_2 & pi_3
      if(pi_p == "pi_2 & pi_3")
      {
        if(omega == 6)
        {
          X_out = X_out  - N1
          CF = CF + N1*spot
          pi_c = "pi_1 & pi_2 & pi_3"
        }
        if(omega == 5)
        {
          X_out = X_out  + N2
          CF = CF - (N2)*spot
          pi_c = "pi_2"
        }
        if(omega == 4)
        {
          X_out = X_out  + N2 + N2
          CF = CF - (N2 + N2)*spot
          pi_c = "0"
        }
        if(omega == 3)
        {
          X_out = X_out + N2 + N2 + N1
          CF = CF - (N2 + N2 + N1)*spot
          pi_c = "g_1"
        }
        if(omega == 2)
        {
          X_out = X_out + N2 + N2 + N2
          CF = CF - (N2 + N2 + N2)*spot
          pi_c = "g_2"
        }
        if(omega == 1)
        {
          X_out = X_out + N2 + N2 + N2
          CF = CF - (N2 + N2 + N2)*spot
          pi_c = "g_3"
        }
        if(omega == 0)
        {
          X_out = X_out  + N2 + N2
          CF = CF - (N2 + N2)*spot
          pi_c = "0"
        }
      }
      
      
      ############################## pi_1 & pi_2 & pi_3
      if(pi_p == "pi_1 & pi_2 & pi_3")
      {
        if(omega == 5)
        {
          X_out = X_out  + N2
          CF = CF - (N2)*spot
          pi_c = "pi_1 & pi_2"
        }
        if(omega == 4)
        {
          X_out = X_out  + N1 + N2 + N2
          CF = CF - (N1 + N2 + N2)*spot
          pi_c = "0"
        }
        if(omega == 3)
        {
          X_out = X_out + N1 + N2 + N2 + N1
          CF = CF - (N1 + N2 + N2 + N1)*spot
          pi_c = "g_1"
        }
        if(omega == 2)
        {
          X_out = X_out + N1 + N2 + N2 + N2
          CF = CF - (N1 + N2 + N2 + N2)*spot
          pi_c = "g_2"
        }
        if(omega == 1)
        {
          X_out = X_out + N1 + N2 + N2 + N2
          CF = CF - (N1 + N2 + N2 + N2)*spot
          pi_c = "g_3"
        }
        if(omega == 0)
        {
          X_out = X_out  + N1 + N2 + N2
          CF = CF - (N1 + N2 + N2)*spot
          pi_c = "0"
        }
      }
      
      
      ############################## g_1
      if(pi_p == "g_1")
      {
        if(omega == 9)
        {
          X_out = X_out  - N1
          CF = CF + N1*spot
          pi_c = "0"
        }
        if(omega == 8)
        {
          X_out = X_out  - N1 - N2
          CF = CF + (N1 + N2)*spot
          pi_c = "pi_3"
        }
        if(omega == 7)
        {
          X_out = X_out  - N1 - N2
          CF = CF + (N1 + N2)*spot
          pi_c = "pi_2"
        }
        if(omega == 6)
        {
          X_out = X_out  - N1 - N1
          CF = CF + (N1 + N1)*spot
          pi_c = "pi_1"
        }
        if(omega == 5)
        {
          X_out = X_out  - N1
          CF = CF + N1*spot
          pi_c = "0"
        }
        if(omega == 2)
        {
          X_out = X_out + N2
          CF = CF - N2*spot
          pi_c = "g_1 & g_2"
        }
        if(omega == 1)
        {
          X_out = X_out + N2
          CF = CF - N2*spot
          pi_c = "g_1 & g_3"
        }
      }
      
      
      ############################## g_2
      if(pi_p == "g_2")
      {
        if(omega == 9)
        {
          X_out = X_out  - N2
          CF = CF + N2*spot
          pi_c = "0"
        }
        if(omega == 8)
        {
          X_out = X_out  - N2 - N2
          CF = CF + (N2 + N2)*spot
          pi_c = "pi_3"
        }
        if(omega == 7)
        {
          X_out = X_out  - N2 - N2
          CF = CF + (N2 + N2)*spot
          pi_c = "pi_2"
        }
        if(omega == 6)
        {
          X_out = X_out  - N2 - N1
          CF = CF + (N2 + N1)*spot
          pi_c = "pi_1"
        }
        if(omega == 5)
        {
          X_out = X_out  - N2
          CF = CF + N2*spot
          pi_c = "0"
        }
        if(omega == 3)
        {
          X_out = X_out + N1
          CF = CF - N1*spot
          pi_c = "g_1 & g_2"
        }
        if(omega == 1)
        {
          X_out = X_out + N2
          CF = CF - N2*spot
          pi_c = "g_2 & g_3"
        }
      }
      
      
      ############################## g_3
      if(pi_p == "g_3")
      {
        if(omega == 9)
        {
          X_out = X_out  - N2
          CF = CF + N2*spot
          pi_c = "0"
        }
        if(omega == 8)
        {
          X_out = X_out  - N2 - N2
          CF = CF + (N2 + N2)*spot
          pi_c = "pi_3"
        }
        if(omega == 7)
        {
          X_out = X_out  - N2 - N2
          CF = CF + (N2 + N2)*spot
          pi_c = "pi_2"
        }
        if(omega == 6)
        {
          X_out = X_out  - N2 - N1
          CF = CF + (N2 + N1)*spot
          pi_c = "pi_1"
        }
        if(omega == 4 || omega == 5)
        {
          X_out = X_out  - N2
          CF = CF + N2*spot
          pi_c = "0"
        }
        if(omega == 3)
        {
          X_out = X_out + N1
          CF = CF - N1*spot
          pi_c = "g_1 & g_3"
        }
        if(omega == 2)
        {
          X_out = X_out + N2
          CF = CF - N2*spot
          pi_c = "g_2 & g_3"
        }
      }
      
      ############################## g_1 & g_2
      if(pi_p == "g_1 & g_2")
      {
        if(omega == 9)
        {
          X_out = X_out - N1 - N2
          CF = CF + (N1 + N2)*spot
          pi_c = "0"
        }
        if(omega == 8)
        {
          X_out = X_out  - N1 - N2 - N2
          CF = CF + (N1 + N2 + N2)*spot
          pi_c = "pi_3"
        }
        if(omega == 7)
        {
          X_out = X_out  - N1 - N2 - N2
          CF = CF + (N1 + N2 + N2)*spot
          pi_c = "pi_2"
        }
        if(omega == 6)
        {
          X_out = X_out  - N1 - N2 - N1
          CF = CF + (N1 + N2 + N1)*spot
          pi_c = "pi_1"
        }
        if(omega == 5)
        {
          X_out = X_out  - N1 - N2
          CF = CF + (N1 + N2)*spot
          pi_c = "0"
        }
        if(omega == 1)
        {
          X_out = X_out + N2
          CF = CF - N2*spot
          pi_c = "g_1 & g_2 & g_3"
        }
      } 
      
      
      ############################## g_1 & g_3
      if(pi_p == "g_1 & g_3")
      {
        if(omega == 9)
        {
          X_out = X_out - N1 - N2
          CF = CF + (N1 + N2)*spot
          pi_c = "0"
        }
        if(omega == 8)
        {
          X_out = X_out  - N1 - N2 - N2
          CF = CF + (N1 + N2 + N2)*spot
          pi_c = "pi_3"
        }
        if(omega == 7)
        {
          X_out = X_out  - N1 - N2 - N2
          CF = CF + (N1 + N2 + N2)*spot
          pi_c = "pi_2"
        }
        if(omega == 6)
        {
          X_out = X_out  - N1 - N2 - N1
          CF = CF + (N1 + N2 + N1)*spot
          pi_c = "pi_1"
        }
        if(omega == 5)
        {
          X_out = X_out  - N1 - N2
          CF = CF + (N1 + N2)*spot
          pi_c = "0"
        }
        if(omega == 4)
        {
          X_out = X_out - N2
          CF = CF + N2*spot
          pi_c = "g_1"
        }
        if(omega == 2)
        {
          X_out = X_out + N2
          CF = CF - N2*spot
          pi_c = "g_1 & g_2 & g_3"
        }
      }
      
      ############################## g_2 & g_3
      if(pi_p == "g_2 & g_3")
      {
        if(omega == 9)
        {
          X_out = X_out - N2 - N2
          CF = CF + (N2 + N2)*spot
          pi_c = "0"
        }
        if(omega == 8)
        {
          X_out = X_out  - N2 - N2 - N2
          CF = CF + (N2 + N2 + N2)*spot
          pi_c = "pi_3"
        }
        if(omega == 7)
        {
          X_out = X_out  - N2 - N2 - N2
          CF = CF + (N2 + N2 + N2)*spot
          pi_c = "pi_2"
        }
        if(omega == 6)
        {
          X_out = X_out  - N2 - N2 - N1
          CF = CF + (N2 + N2 + N1)*spot
          pi_c = "pi_1"
        }
        if(omega == 5)
        {
          X_out = X_out  - N2 - N2
          CF = CF + (N2 + N2)*spot
          pi_c = "0"
        }
        if(omega == 4)
        {
          X_out = X_out - N2
          CF = CF + N2*spot
          pi_c = "g_2"
        }
        if(omega == 3)
        {
          X_out = X_out + N1
          CF = CF - N1*spot
          pi_c = "g_1 & g_2 & g_3"
        }
      }
      
      
      ############################## g_1 & g_1 & g3
      if(pi_p == "g_1 & g_2 & g_3")
      {
        if(omega == 9)
        {
          X_out = X_out - N1 - N2 - N2
          CF = CF + (N1 + N2 + N2)*spot
          pi_c = "0"
        }
        if(omega == 8)
        {
          X_out = X_out  - N1 - N2 - N2 - N2
          CF = CF + (N1 + N2 + N2 + N2)*spot
          pi_c = "pi_3"
        }
        if(omega == 7)
        {
          X_out = X_out  - N1 - N2 - N2 - N2
          CF = CF + (N1 + N2 + N2 + N2)*spot
          pi_c = "pi_2"
        }
        if(omega == 6)
        {
          X_out = X_out  - N1 - N2 - N2 - N1
          CF = CF + (N1 + N2 + N2 + N1)*spot
          pi_c = "pi_1"
        }
        if(omega == 5)
        {
          X_out = X_out  - N1 - N2 - N2
          CF = CF + (N1 + N2 + N2)*spot
          pi_c = "0"
        }
        if(omega == 4)
        {
          X_out = X_out - N2
          CF = CF + N2*spot
          pi_c = "g_1 & g_2"
        }
      }
      print(paste("New state = ", pi_c, "; CF = ", CF, "; X_out = ", X_out, sep = ""))
      df_temp <- rbind(df_temp, data.frame(date = fx_spot$date[j], Trade_period = t_hours, Previous_state  = pi_p, Spot = spot, Interval_id = omega, New_state = pi_c, CF = CF, X_out = X_out))
      
      #print(X_out)
      pi_p = pi_c
      date_prev = date_c
      t_hours = t_hours + 1
    #problem with saving from here, don't understand
  }  
  
}


