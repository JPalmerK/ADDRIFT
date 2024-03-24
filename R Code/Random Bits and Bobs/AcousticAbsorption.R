# Estimate max detection range using simplified metrics
rm(list=ls())
library(stats)

# frequency dependent acoustic absorption
AcousticAbsorption <-function(f, Z=0, Temp=5, S=35, pH=8){
  
  # Following Kinsler, et al "Fundamentals of Acoustics, Fourth Edition" p. 226-228.
  # f = frequency in Hz
  # Z = depth in km
  # Temp = temperature in C
  # S = salinity in ppt
  # pH = pH
  
  
  f_1 = 780*exp(Temp/29)
  f_2 = 42000*exp(Temp/18)
  A = 0.083*(S/35)*exp(Temp/31 - Z/91 + 1.8*(pH-8))
  B = 22*(S/35)*exp(Temp/14-Z/6)
  C = 4.9e-10*exp(-Temp/26 - Z/25)
  boric_acid = A/(f_1^2+f^2) # contribution from boric acid
  MgSO4 = B/(f_2^2+f^2) # contribution from MgSO4
  hydrostatic = C # contribution from hydrostatic pressure
  alpha = (boric_acid + MgSO4 + hydrostatic)*f^2 #db/KM
  
  return(mean(alpha))
}

# Sonar equation over which to optimize r given, SL, NL, depth, frequency
# and SNR threshold
logfun <- function(SL, NL, SNRthresh, h, f) {
  function(r) {
    alpha = AcousticAbsorption(f)
    abs(SL - (10*log10(r)+10*log10(h/2)+(alpha/1000)*r) - NL - SNRthresh)
  }
}

# source, level, noise level, frequen(ies), and SNR threshold
f=5000:8000
SL=180
NL=80
SNRthresh=1
h=1000

# estimate the maximum range in meters using simplified sonar equations
maxRange = optim(par=1000, 
      fn=logfun(SL, NL, 1, h, f), 
      method='Brent', lower=1, upper=10e8)$par

print(paste('Max detection range estimate:', round(maxRange/1000),
            'km assuming SL=', SL, 'NL=', NL, ' SNR threshold=', 
            SNRthresh, 'and depth=', h, 'm'))
