strategy_2_trade <- function(trading_dates, fx_spot, X_out, CF, s)
{
  
pi_p == "0"
pi_c = "0"
View(fx_spot)
  

#here we store the results only for hours within trading days
df_temp <- data.frame(date = double(), Trade_period = double(), Previous_state  = double(), Spot = double(), Interval_id = double(), New_state = double(), CF = double(), X_out = double())

fx_spot_temp = fx_spot[fx_spot$date == trading_dates[1], ]
date = fx_spot_temp$date[1]
spot = fx_spot_temp$Spot[1]
t_days = 1 #physical number of days we trade

df_temp <- rbind(df_temp, data.frame(date = date, Trade_period = t_days, Previous_state  = "NA", Spot = spot, Interval_id = 5, New_state = "0", CF = CF, X_out = X_out))


for(j in 2:nrow(fx_spot)) #fx_spot is already determined for trading period
{
    
  date_temp = fx_spot$date[j]
    
  #if we are within trading date
  if(!is.na(match(date_temp, trading_dates)))
  {
    t_days = t_days + 1 #physical number of days we trade
    index = findInterval(date_temp, trading_dates)
    date = trading_dates[index]
    date_format = format(date, format = "%d.%m.%Y")

    spot = fx_spot$Spot[j]
    x = findInterval(spot, s) #returns index of those element, that is lower bound
    omega = determine_interval(spot, s)
      
    print("")
    print(paste("Iteration number ", t_days, "; date = ", fx_spot$date[j], sep = ""))
    print(s)
    print(paste("previous state: ", pi_p, "; Spot movement: ", spot, " Interval id: ", omega, sep = ""))
    
  
    #rolling condition
    if( count%% 14 == 0 & (spot - Spot_0 <= 0))#for call option, if spot increases, then call also increases in value and we don't want to roll the option after 2 weeks
    {
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
    }
        
          
    #condition when we exit trade;
    if( abs(spot - Target) <= 0.001 || date == trading_dates[length(trading_dates)] ) #fx_spot is already determined for trading period
    {
            
      if(abs(spot - Target) <= 0.001 )
      {
          print("______________Trading terminated after the spot triggered upper barier________________")
              #print("                                                                                      ")
      }
            
      if( date == trading_dates[length(trading_dates)] )
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
            
      c_cur = Calculate_Option_Price_Strike(date, rates, fx_spot, j, Strike_0, expiration)*Nominal #calc price fir given optin
      print(paste("Derivative current price: = ", c_cur, sep = ""))
      print(paste("PL on derivative before closing position = ", PL_d, sep = ""))
            
      print(paste("______________ TRADING RESULTS FOR GIVEN PERIOD: from ", trading_period[1], " to ", trading_period[length(trading_period)], "________________", sep = ""))
      print("                                                                       ")
            
      PL_d = PL_d + c_cur #close derivative position
      CF = CF + X_out*fx_spot_i$Spot[j] #close physical position
      print(paste("PL on derivative Position = ", PL_d, sep = ""))
      print(paste("PL on physical Position = ", CF, sep = ""))
      print(paste("Total PL on trading = ", PL_d + CF, sep = ""))
            
      df_temp <- rbind(df_temp, data.frame(date = fx_spot$date[j], Trade_period = t_days, Previous_state  = pi_p, Spot = spot, Interval_id = omega, New_state = pi_c, CF = CF, X_out = X_out))
      x = 0
      #stopifnot(x = 1)
      if(x == 0)#http://r.789695.n4.nabble.com/how-to-stop-without-error-message-td866604.html
      {
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
    if(pi_p == "g1 & g_2 & g_3")
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
    df_temp <- rbind(df_temp, data.frame(date = fx_spot$date[j], Trade_period = t_days, Previous_state  = pi_p, Spot = spot, Interval_id = omega, New_state = pi_c, CF = CF, X_out = X_out))
    
    #print(X_out)
    pi_p = pi_c
  }#here ends trading hour
  #problem with saving from here, don't understand
}  

  return(df_temp)
}
  
  
