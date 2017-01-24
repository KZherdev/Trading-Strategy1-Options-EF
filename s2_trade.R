s2_trade <- function(fx_spot, X_out, CF, s)
{
  pi_p == "0"
  pi_c = "0"
  
  df_temp <- data.frame(date = double(), Iteration_number = double(), Previous_state  = double(), Spot = double(), Interval_id = double(), New_state = double(), CF = double(), X_out = double())
  df_temp <- rbind(df_temp, data.frame(date = fx_spot$date[1], Iteration_number = 1, Previous_state  = "0", Spot = 0, Interval_id = 5, New_state = 0, CF = 0, X_out = 0))

for(j in 2: nrow(fx_spot))
{
  spot = fx_spot$Spot[j]
  x = findInterval(spot, s) #returns index of those element, that is lower bound
  omega = determine_interval(spot, s)
  print("")
  print(paste("Iteration number ", j, "; date = ", fx_spot$date[j], sep = ""))
  print(s)
  print(paste("previous state: ", pi_p, "; Spot movement: ", spot, " Interval id: ", omega, sep = ""))
  
  if(j == nrow(fx_spot))
  {
    
    print("Period finished")
    CF = CF + X_out*spot
    df_temp <- rbind(df_temp, data.frame(date = fx_spot$date[j], Iteration_number = j, Previous_state  = pi_p, Spot = spot, Interval_id = omega, New_state = pi_c, CF = CF, X_out = X_out))
    print(paste("CF = ", CF))
    
    x = 0
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
  df_temp <- rbind(df_temp, data.frame(date = fx_spot$date[j], Iteration_number = j, Previous_state  = pi_p, Spot = spot, Interval_id = omega, New_state = pi_c, CF = CF, X_out = X_out))

  #print(X_out)
  pi_p = pi_c
}
  #problem with saving from here, don't understand

  return(df_temp)
}