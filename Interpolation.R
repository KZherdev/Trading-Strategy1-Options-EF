#Linear interpolation

VSInterpolation <- function(df, x, expiration, method = "linear")
{
  vol = 0;
  if(method == "linear")
    vol = VS_Liner_Interpolation(df, x, expiration)

  return(vol);
}

VS_Liner_Interpolation <- function(df, x, ndays) #expiration in form of integer number of days
{
 
  res = 0; 
  
  u_dates = unique(sort(df$Days_To_Expiry)) #unique dates
  u_X = unique(sort(df$X)) #unique deltas
  
  #print("IV CALCULATION PART BEGIN")
  #print(paste("ndays = ", ndays, sep = ""))
  # print(u_dates)
  # print(u_X)
  
  add = 0
  #in case the first date is not 1 day
  if(u_dates[1] >= 7)
  {
    u_dates = c(1, u_dates)
    u_X = c(u_X[1], u_X)
    add = 1
  }
  
  # print(u_dates)
  # print(u_X)
  # 
  lower_date_index = findInterval(ndays, u_dates) #find index (start from 1) element in list; returns lower bounds 
  upper_date_index = findInterval(ndays, u_dates) + 1
  
  lower_date = u_dates[lower_date_index]
  upper_date = u_dates[upper_date_index]
  
  
  # print(lower_date)
  # print(upper_date)
  # 
  #print("Up to here norm")

  set_X_lower_date = df$X[df$Days_To_Expiry == lower_date]#set of X for lower date
  set_X_upper_date = df$X[df$Days_To_Expiry == upper_date]#set of X for upper date
  
  if(add == 1)
  {
    set_X_lower_date = c(u_X[1])

  }
  #Determine lower and upper strikes for lower T
  set_X_lower_date = sort(set_X_lower_date)
  
  # print(set_X_lower_date)
  # print(set_X_upper_date)
  # print(x)
  # print(set_X_lower_date[1])
  # print(set_X_upper_date[1])
  # 

  X_index_lower_down = findInterval(x, set_X_lower_date)
  X_index_lower_up = findInterval(x, set_X_lower_date) + 1
  
  # print(X_index_lower_down)
  # print(X_index_lower_up)
  # 
  if(X_index_lower_down == 0 & X_index_lower_up == 1)
  {
    X_index_lower_down = 1
  }
  
  if(X_index_lower_down == length(set_X_lower_date) & X_index_lower_up == length(set_X_lower_date) + 1)
  {
    #print("here!")
    X_index_lower_up = length(set_X_lower_date)
  }
  # print("here!")
  X_lower_down = set_X_lower_date[X_index_lower_down]
  X_lower_up = set_X_lower_date[X_index_lower_up]
  
  # print(X_lower_down)
  # print(X_lower_up)
  # print("here!!")
  # 
  # if(is.na(X_lower_down) == TRUE)
  # {
  #   X_lower_down = X_lower_up
  # }
  # 
  # if(is.na(X_lower_up) == TRUE)
  # {
  #   X_lower_up = X_lower_down
  # }
  # 
  # print(X_lower_down)
  # print(X_lower_up)
  # 
  #Determine lower and upper strikes for higher T
  set_X_upper_date = sort(set_X_upper_date)
  
  # print(set_X_upper_date)
  
  X_index_upper_down = findInterval(x, set_X_upper_date)
  X_index_upper_up = findInterval(x, set_X_upper_date) + 1
  # print(X_index_upper_down)
  # print(X_index_upper_up)
  # 
  if(X_index_upper_down == 0 & X_index_upper_up == 1)
  {
    X_index_upper_down = 1
  }
  
  if(X_index_upper_down == length(set_X_upper_date) & X_index_upper_up == length(set_X_upper_date) + 1)
  {
    X_index_upper_up = length(set_X_upper_date)
  }
  
  X_upper_down = set_X_upper_date[X_index_upper_down]
  X_upper_up = set_X_upper_date[X_index_upper_up]
  
  # if(is.na(X_upper_down) == TRUE)
  # {
  #   X_upper_down = X_upper_up
  # }
  # 
  # if(is.na(X_upper_up) == TRUE)
  # {
  #   X_upper_up = X_upper_down
  # }
  
  # print("here!!!")
  # print(X_upper_down)
  # print(X_upper_up)
  # 


  vol_1 = df$ivol[df$Days_To_Expiry == lower_date & df$X == X_lower_down]
  vol_2 = df$ivol[df$Days_To_Expiry == lower_date & df$X == X_lower_up]
  
  vol_3 = df$ivol[df$Days_To_Expiry == upper_date & df$X == X_upper_down]
  vol_4 = df$ivol[df$Days_To_Expiry == upper_date & df$X == X_upper_up]
    
  if(length(vol_1) == 0 & length(vol_2) == 0)
  {
    vol_1 = vol_3
    vol_2 = vol_4
  }
  # print(vol_1)
  # print(vol_2)
  # print(vol_3)
  # print(vol_4)
  
  res = 0.25*(vol_1 + vol_2 + vol_3 + vol_4)
  #print("IV VALCULATION PART END")
  
  #print(res)
  return(res);
  
}
