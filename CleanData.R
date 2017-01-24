library(plyr)
#input - path to the directory where interest rates are stored
#output - create file from two bloomberg inputs; no rows are included or excluded
CleanInterestRates <- function(path_rates)
{
  file_name_f = paste(path_rates, "EE0012M Index_historical.csv", sep = "")
  file_name_d = paste(path_rates, "US0012M Index_historical.csv", sep = "")
  rates_f = read.csv(file_name_f, header = TRUE)
  rates_d = read.csv(file_name_d, header = TRUE)
  rates = merge(rates_d, rates_f, by.x = "date", by.y = "date")
  rates = rename(rates, c("px_last.x"="r_domestic", "px_last.y"="r_foreign"))
  
  rates$r_domestic = rates$r_domestic/100
  rates$r_foreign = rates$r_foreign/100
  
  file_name = paste(path_rates, "Interest_Rates_historical_cleaned.csv", sep = "")
  write.csv(rates, file_name, row.names=FALSE)
}


#input - common path to BL data, ticker we are considering, strategy first and end dates, some particular period number - to read from Metastock folder
#output - we copy raw metastock data from "Metastock EURUSD raw" to Folder related to particular trading period; assume already created
CleanFXSpot <- function(path_BLData, ticker, period_number, first_date, end_date)
{
 
  file_name = paste(path_BLData, "Metastock ", ticker, " raw//", "period_", period_number, ".csv", sep = "")
  fxspot = read.csv(file_name, header = TRUE)
  
  fxspot$Day = as.Date(levels(fxspot$Day), format="%m/%d/%Y")[fxspot$Day] #Y should be capital, otherwise 2020
  fxspot$Open <- NULL
  fxspot$High <- NULL
  fxspot$Low <- NULL
  colnames(fxspot) <- c("date", "Hour", "Spot")
  #fxspot$Period <- 1:nrow(fxspot)
  
  path_strategy_folder = paste(path_BLData, "Trading period_", period_number, " ", first_date, " to ", end_date, "//", sep = "")#created outside folder for our strategy
  file_name = paste(path_strategy_folder, ticker, "historical_", first_date, " to ", end_date, "_cleaned.csv", sep = "")
  
  write.csv(fxspot, file_name, row.names=FALSE)
  
  #fx_spot_rd = fxspot[!duplicated(fxspot[,3]),] #http://stackoverflow.com/questions/6835753/how-to-remove-duplicated-rows-by-a-column-in-a-matrix-in-r
  
  fx_spot_rd = fxspot[!duplicated(fxspot$date),]
  fx_spot_rd$Hour <- NULL
  fx_spot_rd$Period <- NULL
  
  #file_name = paste(path_BLData, "Spot_history_cleaned_daily.csv", sep = "")
  file_name = paste(path_strategy_folder, ticker, "daily_", first_date, " to ", end_date, "_cleaned.csv", sep = "")
  write.csv(fx_spot_rd, file_name, row.names=FALSE)
}

#Input - general path where all data folders are stored, period number and period dates for period of interest, trading dates - for convenience make external
#Trading dates here are passed as trading period to modify all available surface
CleanVSDelta <- function(path_BLData, ticker, period_number, first_date, end_date, trading_dates) #general period - to refer to folder and trading dates - to clean trading dates only
{
  #refer to strategy folder
  path_strategy_folder = paste(path_BLData, "Trading period_", period_number, " ", first_date, " to ", end_date, "//", sep = "")
  
  for(i in 1:length(trading_dates))
  {
    date = trading_dates[i];
    date = format(date, format = "%d.%m.%Y")
    #print(date)
    file_name = paste(path_strategy_folder, ticker, date, "_day.csv", sep = "")
    VSDelta_date = read.csv(file_name, header = FALSE)
    
    #Rename columns and delete unnecessary for our purposes
    VSDelta_date = rename(VSDelta_date, c("V1"="Days_To_Expiry", "V3"="Call_Put_Delta", "V4" = "Call_Delta", "V5" = "ivol"))
    VSDelta_date$V2<-NULL
    
    #Dates are in form of levels. Firstly, rename them to integers, secondly - convert from factors to numeric
    VSDelta_date$Days_To_Expiry = revalue( VSDelta_date$Days_To_Expiry, c("1D"=1, "10Y" = 10*365, "18M" = 18*30,  "1M" = 1*30,  "1W" = 1*7,  "1Y" = 1*365,  "2M" = 2*30,   "2W" = 2*7,  "2Y" = 2*365,  "3M" = 3*30,  "3W" = 3*7,  "3Y" = 3*365,  "4M" = 4*30,  "4Y" = 4*365,  "5Y" = 5*365,  "6M" = 6*30,  "7Y" = 7*365,  "9M" = 9*30, "15Y" = 15*365))
    VSDelta_date$Days_To_Expiry = as.numeric(levels(VSDelta_date$Days_To_Expiry))[VSDelta_date$Days_To_Expiry]
    
    VSDelta_date$ivol =  VSDelta_date$ivol/100
    
    file_name = paste(path_strategy_folder, date, sep = "")
    file_name = paste(file_name, "_Delta_VS_cleaned.csv", sep = "")
    
    drops <- c("Call_Put_Delta")
    VSDelta_date = VSDelta_date[, !(names(VSDelta_date) %in% drops)]
    colnames(VSDelta_date) <- c("Days_To_Expiry", "X", "ivol")
    
    write.csv(VSDelta_date, file_name, row.names=FALSE)
  }
}  

#input format as of 2001
CleanVSDelta_2001 <- function(path_BLData, ticker, period_number, first_date, end_date, trading_dates) #general period - to refer to folder and trading dates - to clean trading dates only
{
  #refer to strategy folder
  path_strategy_folder = paste(path_BLData, "Trading period_", period_number, " ", first_date, " to ", end_date, "//", sep = "")
  
  for(i in 1:length(trading_dates))
  {
    date = trading_dates[i];
    date = format(date, format = "%d.%m.%Y")
    print(date)
    file_name = paste(path_strategy_folder, ticker, date, "_day.csv", sep = "")
    VSDelta_date = read.csv(file_name, header = FALSE)
    
    #Rename columns and delete unnecessary for our purposes
    VSDelta_date = rename(VSDelta_date, c("V1"="Days_To_Expiry", "V4" = "Call_Delta", "V5" = "ivol"))
    VSDelta_date$V2<-NULL #time to expity in years
    VSDelta_date$V3<-NULL #time to expity in years
    
    #Dates are in form of levels. Firstly, rename them to integers, secondly - convert from factors to numeric
    VSDelta_date$Days_To_Expiry = revalue( VSDelta_date$Days_To_Expiry, c("1D"=1, "1M" = 1*30,  "1W" = 1*7,  "1Y" = 1*365,  "2M" = 2*30,   "2W" = 2*7, "3M" = 3*30, "6M" = 6*30, "9M" = 9*30))
    VSDelta_date$Days_To_Expiry = as.numeric(levels(VSDelta_date$Days_To_Expiry))[VSDelta_date$Days_To_Expiry]
    
    VSDelta_date$ivol =  VSDelta_date$ivol/100
    
    file_name = paste(path_strategy_folder, date, sep = "")
    file_name = paste(file_name, "_Delta_VS_cleaned.csv", sep = "")
    
    drops <- c("Call_Put_Delta")
    VSDelta_date = VSDelta_date[, !(names(VSDelta_date) %in% drops)]
    colnames(VSDelta_date) <- c("Days_To_Expiry", "X", "ivol")
    
    write.csv(VSDelta_date, file_name, row.names=FALSE)
  }
} 

#problem with input - dates are downloaded incorrectly from Metastock
#Difficult to do once for all available data; Hence we also clean fxspot for some period we are currently interested in
#Input - path with raw metastock data, ticker name, trading period we
# CleanFXSpot <- function(path_BLData, ticker, trading_period)
# {
#   file_name = paste(path_BLData, ticker, "historical ", trading_period[1], " to ", trading_period[length(trading_period)], ".csv", sep = "")
#   fxspot = read.csv(file_name, header = TRUE)
# 
#   fxspot$Day = as.Date(levels(fxspot$Day), format="%m/%d/%Y")[fxspot$Day] #Y should be capital, otherwise 2020
#   #fxspot$Day_MTStock <- NULL
#   #fxspot$X <- NULL
#   fxspot$Open <- NULL
#   fxspot$High <- NULL
#   fxspot$Low <- NULL
#   #colnames(fxspot) <- c("Hour", "Spot", "date")
#   colnames(fxspot) <- c("date", "Hour", "Spot")
#   fxspot$Period <- 1:nrow(fxspot)
#   
#   #file_name = paste(path_BLData, "EURUSD_history_Sep_Oct_cleaned.csv", sep = "")
#   file_name = paste(path_BLData, ticker, "historical_", trading_period[1], " to ", trading_period[length(trading_period)], "_cleaned.csv", sep = "")
#   
#   write.csv(fxspot, file_name, row.names=FALSE)
#   
#   
#   #fx_spot_rd = fxspot[!duplicated(fxspot[,3]),] #http://stackoverflow.com/questions/6835753/how-to-remove-duplicated-rows-by-a-column-in-a-matrix-in-r
#   fx_spot_rd = fxspot[!duplicated(fxspot$date),]
#   fx_spot_rd$Hour <- NULL
#   fx_spot_rd$Period <- NULL
#   
#   #file_name = paste(path_BLData, "Spot_history_cleaned_daily.csv", sep = "")
#   file_name = paste(path_BLData, ticker, "daily_", trading_period[1], " to ", trading_period[length(trading_period)], "_cleaned.csv", sep = "")
#   write.csv(fx_spot_rd, file_name, row.names=FALSE)
#     
# }

#bloomberg gives data for weekends also; otw we would attempt to read weekend file and get an error
# CleanVSDelta <- function(trading_dates, path_BLData, ticker)
# {
#   
#   for(i in 1:length(trading_dates))
#   {
#     date = trading_dates[i];
#     date = format(date, format = "%d.%m.%Y")
#     print(date)
#     file_name = paste(path_BLData, ticker, date, "_day.csv", sep = "")
#     VSDelta_date = read.csv(file_name, header = FALSE)
#     
#     #Rename columns and delete unnecessary for our purposes
#     VSDelta_date = rename(VSDelta_date, c("V1"="Days_To_Expiry", "V3"="Call_Put_Delta", "V4" = "Call_Delta", "V5" = "ivol"))
#     VSDelta_date$V2<-NULL
#     
#     #Dates are in form of levels. Firstly, rename them to integers, secondly - convert from factors to numeric
#     VSDelta_date$Days_To_Expiry = revalue( VSDelta_date$Days_To_Expiry, c("1D"=1, "10Y" = 10*365, "18M" = 18*30,  "1M" = 1*30,  "1W" = 1*7,  "1Y" = 1*365,  "2M" = 2*30,   "2W" = 2*7,  "2Y" = 2*365,  "3M" = 3*30,  "3W" = 3*7,  "3Y" = 3*365,  "4M" = 4*30,  "4Y" = 4*365,  "5Y" = 5*365,  "6M" = 6*30,  "7Y" = 7*365,  "9M" = 9*30, "15Y" = 15*365))
#     VSDelta_date$Days_To_Expiry = as.numeric(levels(VSDelta_date$Days_To_Expiry))[VSDelta_date$Days_To_Expiry]
#     
#     VSDelta_date$ivol =  VSDelta_date$ivol/100
#     
#     file_name = paste(path_BLData, date, sep = "")
#     file_name = paste(file_name, "_Delta_VS_cleaned.csv", sep = "")
#     
#     drops <- c("Call_Put_Delta")
#     VSDelta_date = VSDelta_date[, !(names(VSDelta_date) %in% drops)]
#     colnames(VSDelta_date) <- c("Days_To_Expiry", "X", "ivol")
#     
#     write.csv(VSDelta_date, file_name, row.names=FALSE)
#   }
# } 



