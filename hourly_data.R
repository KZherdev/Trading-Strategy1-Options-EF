library(plyr)

path_Scripts = "C://Users//Konstantin//Desktop//On hand//europe finance//Version 4//"
path_BLData = "C://Users//Konstantin//Desktop//On hand//europe finance//Data From BL//"

file_name = paste(path_BLData, "EURUSD History_Sep_Oct.csv", sep = "")
fxspot = read.csv(file_name, header = TRUE)

fxspot$Day = as.Date(levels(fxspot$Day), format="%m/%d/%Y")[fxspot$Day] #Y should be capital, otherwise 2020
fxspot$Day_MTStock <- NULL
fxspot$X <- NULL
fxspot$Open <- NULL
fxspot$High <- NULL
fxspot$Low <- NULL

View(fxspot)
fxspot$Day[10]

file_name = paste(path_BLData, "EURUSD History_March-May.csv", sep = "")
fxspot = read.csv(file_name, header = TRUE)

fxspot$Day = as.Date(levels(fxspot$Day), format="%m/%d/%Y")[fxspot$Day] #Y should be capital, otherwise 2020
fxspot$Day_MTStock <- NULL
fxspot$X <- NULL
fxspot$Open <- NULL
fxspot$High <- NULL
fxspot$Low <- NULL

View(fxspot)
fxspot$Day[10]
