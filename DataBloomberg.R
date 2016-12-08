
SaveCSV <- function(df, name)
{
  
  file_name = ".csv"
  file_name = paste(name, file_name, sep = "") #merge two strings, inverse format
  write.csv(df, file_name, row.names=FALSE)
}

#Get Volatility Surface data from OVDV
Volatility_Surface_Delta_2 <- function(dates)
{
  for (i in 1:length(dates))
  {
    date = dates[i]
    df <- BDS(conn, "EURUSD  Curncy","DFLT_VOL_SURF_MID", date)
    name = paste(date, "_Delta_VS", sep = "")
    SaveCSV(df, name)
  }
  return(df)
}


#Get Volatility Surface data from another source
Volatility_Surface_Delta_1 <- function(dates)
{
  
  #df <- BDS(conn, "EURUSD  Curncy","DFLT_VOL_SURF_MID", dates)
  file_name = "Volatility_Surface_All_Dates_Deltas.csv" #in deltas
  write.csv(df, file_name, row.names=FALSE)
  
}

Interest_Rates_Get_Data <- function(dates)
{
  
  df <-  BDH(conn, c("US0012M Index", "EE0012M Index"), "px_last", dates)
  file_name = "Interest_Rates_All_Dates_.csv"
  write.csv(df, file_name, row.names=FALSE)
  
}

FXSpot_Get_Data <- function(dates)
{
  
  df <- BDH(conn, "EURUSD Curncy", "px_last", dates)
  file_name = "Forex_Spot_All_Dates.csv"
  write.csv(df, file_name, row.names=FALSE)
}

blpDisconnect(conn)
proc.time()

