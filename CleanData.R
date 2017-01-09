library(plyr)
CleanInterestRates <- function(path_BLData)
{
  file_name_f = paste(path_BLData, "EE0012M Index_historical.csv", sep = "")
  file_name_d = paste(path_BLData, "US0012M Index_historical.csv", sep = "")
  rates_f = read.csv(file_name_f, header = TRUE)
  rates_d = read.csv(file_name_d, header = TRUE)
  rates = merge(rates_d, rates_f, by.x = "date", by.y = "date")
  rates = rename(rates, c("px_last.x"="r_domestic", "px_last.y"="r_foreign"))
  
  rates$r_domestic = rates$r_domestic/100
  rates$r_foreign = rates$r_foreign/100
  
  file_name = paste(path_BLData, "Interest_Rates_historical_cleaned.csv", sep = "")
  write.csv(rates, file_name, row.names=FALSE)
}


CleanFXSpot <- function(path_BLData)
{
  file_name = paste(path_BLData, "EURUSD History_Sep_Oct.csv", sep = "")
  fxspot = read.csv(file_name, header = TRUE)

  fxspot$Day = as.Date(levels(fxspot$Day), format="%m/%d/%Y")[fxspot$Day] #Y should be capital, otherwise 2020
  fxspot$Day_MTStock <- NULL
  fxspot$X <- NULL
  fxspot$Open <- NULL
  fxspot$High <- NULL
  fxspot$Low <- NULL
  colnames(fxspot) <- c("Hour", "Spot", "date")
  fxspot$Period <- 1:nrow(fxspot)
  
  file_name = paste(path_BLData, "EURUSD_history_Sep_Oct_cleaned.csv", sep = "")
  write.csv(fxspot, file_name, row.names=FALSE)
  
  
  fx_spot_rd = fxspot[!duplicated(fxspot[,3]),] #http://stackoverflow.com/questions/6835753/how-to-remove-duplicated-rows-by-a-column-in-a-matrix-in-r
  fx_spot_rd$Hour <- NULL
  fx_spot_rd$Period <- NULL
  
  file_name = paste(path_BLData, "Spot_history_cleaned_daily.csv", sep = "")
  write.csv(fx_spot_rd, file_name, row.names=FALSE)
    
}


CleanVSDelta <- function(trading_dates, path_BLData, ticker)
{
  
  for(i in 1:length(trading_dates))
  {
    date = trading_dates[i];
    date = format(date, format = "%d.%m.%Y")
    print(date)
    file_name = paste(path_BLData, ticker, date, "_day.csv", sep = "")
    VSDelta_date = read.csv(file_name, header = FALSE)
    
    #Rename columns and delete unnecessary for our purposes
    VSDelta_date = rename(VSDelta_date, c("V1"="Days_To_Expiry", "V3"="Call_Put_Delta", "V4" = "Call_Delta", "V5" = "ivol"))
    VSDelta_date$V2<-NULL
    
    #Dates are in form of levels. Firstly, rename them to integers, secondly - convert from factors to numeric
    VSDelta_date$Days_To_Expiry = revalue( VSDelta_date$Days_To_Expiry, c("1D"=1, "10Y" = 10*365, "18M" = 18*30,  "1M" = 1*30,  "1W" = 1*7,  "1Y" = 1*365,  "2M" = 2*30,   "2W" = 2*7,  "2Y" = 2*365,  "3M" = 3*30,  "3W" = 3*7,  "3Y" = 3*365,  "4M" = 4*30,  "4Y" = 4*365,  "5Y" = 5*365,  "6M" = 6*30,  "7Y" = 7*365,  "9M" = 9*30, "15Y" = 15*365))
    VSDelta_date$Days_To_Expiry = as.numeric(levels(VSDelta_date$Days_To_Expiry))[VSDelta_date$Days_To_Expiry]
    
    VSDelta_date$ivol =  VSDelta_date$ivol/100
    
    file_name = paste(path_BLData, date, sep = "")
    file_name = paste(file_name, "_Delta_VS_cleaned.csv", sep = "")
    
    drops <- c("Call_Put_Delta")
    VSDelta_date = VSDelta_date[, !(names(VSDelta_date) %in% drops)]
    colnames(VSDelta_date) <- c("Days_To_Expiry", "X", "ivol")
    
    write.csv(VSDelta_date, file_name, row.names=FALSE)
  }
}  