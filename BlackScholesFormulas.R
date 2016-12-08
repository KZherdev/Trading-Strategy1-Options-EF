BlackScholes76CallPrice <-function(fxspot, Strike, Vol, r_d, r_f, Expiry)
{
  Forward = fxspot*exp((r_d-r_f)*Expiry)
  standardDeviation = Vol*sqrt(Expiry);
  moneyness = log(Forward / Strike);
  d1 = (moneyness + 0.5* standardDeviation*standardDeviation) / standardDeviation;
  d2 = d1 - standardDeviation;
  return(fxspot*exp(-r_f*Expiry) * pnorm(d1, 0, 1) - Strike*exp(-r_d*Expiry)*pnorm(d2, 0, 1))
}

BlackScholes76PutPrice <-function(Forward, Strike, Vol, Expiry)
{
  Forward = fxspot*exp((r_d-r_f)*Expiry)
  standardDeviation = Vol*sqrt(Expiry);
  moneyness = log(Forward / Strike);
  d1 = (moneyness + 0.5* standardDeviation*standardDeviation) / standardDeviation;
  d2 = d1 - standardDeviation;
  return(-1*fxspot*exp(-r_f*Expiry) * pnorm(-1*d1, 0, 1) + Strike*exp(-r_d*Expiry)*pnorm(-1*d2, 0, 1))
  
}

BlackScholes76CallDelta <-function(Strike, Forward, Vol, Expiry)
{
  standardDeviation = Vol*sqrt(Expiry);
  moneyness = log(Forward / Strike);
  d1 = (moneyness + 0.5* standardDeviation*standardDeviation) / standardDeviation;
  return(pnorm(d1,0,1))
}

BlackScholes76PutDelta<-function(Forward, Strike, Vol, Expiry)
{
  standardDeviation = Vol*sqrt(Expiry);
  moneyness = log(Forward / Strike);
  d1 = (moneyness + 0.5* standardDeviation*standardDeviation) / standardDeviation;
  return(-1*pnorm(-1*d1,0,1))
}
