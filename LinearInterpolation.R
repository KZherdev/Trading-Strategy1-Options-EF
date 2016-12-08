#Perform linear interpolation on the constructed VS Grid with Strikes
#Input - date, for which we want to call already constructed Grid with strikes and interpolate, also some expiration and strike


VS_Liner_Interpolation <- function(date, Expiration, Strike) #expiration in form of integer number of days
{
  file_name = "_VS_Strikes.csv";
  file_name = paste(date, file_name, sep = ""); #merge two strings, inverse format
  df_cleanbldata = load(file_name);

  
  #Assumption: Suppose, all strikes are determined correctly for every expiration
  
  #Determine between which dates our expiration lies, column dates
  
  #step 1 - Create temporary file to store unique dates - sth like  df_cleanbldata[Expiration].unique(); sort it;
  #step 2 - find two location indices for lower and upper bounds of our expiration date T
  #step 3 - store these values - we will need them as coordinates to do linear interpolation
  
  
  #Determine between which strikes our strike lies, for both lower date strikes and upper date strikes
  #step 1 - Create temporary file to store unique strikes - sth like  df_cleanbldata[Strikes].unique(); sort it;
  #step 2 - find two location indices for lower and upper bounds of our strike K
  #step 3 - store these values
  
  #as a result we have 2 border strikes and 2 border dates - 4 points in total. Take some average for volatilities
  #sth like
  v1 = df_cleanbldata[df_cleanbldata, where strike = strike1 and exp = exp1]
  v = 0.5*(v1 + v2 + v3 + v4)
  return v;
  
}
