create_intervals <- function(Spot_0, step, n_one_way)
{
  l = Spot_0 - n_one_way*step
  u = Spot_0 + n_one_way*step
  s = seq(l,u,step)
  remove <- c(Spot_0)
  s = s[! s %in% remove] #http://stackoverflow.com/questions/9665984/how-to-delete-multiple-values-from-a-vector
  #print(s)
  return(s)
  
}

#determine interval number for given spot value
determine_interval <- function(Spot, s)
{
  x = findInterval(Spot,s) #returns index of those element, that is lower bound
  return(x)
}


create_intervals_str_2 <- function(Spot_0, step, n_one_way)
{
  l = Spot_0 - n_one_way*step
  u = Spot_0 + n_one_way*step
  s = seq(l,u,step)
  return(s)
}