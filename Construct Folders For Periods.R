construct_folder_for_period <- function(mainDir, period_number, start_date, end_date)
{
  subDir = paste("Trading period_", period_number, " ", start_date, " to ", end_date, sep = "")
  dir.create(file.path(mainDir, subDir))
}