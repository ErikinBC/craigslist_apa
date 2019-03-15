# THis is all the functions needed for analysis

# Capitalize the first letters of any words
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Haversine formula (hf)
gcd.hf <- function (lon1,lat1,lon2,lat2, r = 6378137) {
  toRad <- pi/180
  p1 <- c(lon1,lat1)
  p2 <- c(lon2,lat2)
  p1 <- p1 * toRad
  p2 <- p2 * toRad
  p = c(p1[1], p1[2], p2[1], p2[2], r)
  dLat <- p[4] - p[2]
  dLon <- p[3] - p[1]
  a <- sin(dLat/2) * sin(dLat/2) + cos(p[2]) * cos(p[4]) * 
    sin(dLon/2) * sin(dLon/2)
  dist <- 2 * atan2(sqrt(a), sqrt(1 - a)) * p[5]
  return(dist)
}

# gcd.hf(lon1=t.centre[2],lat1=t.centre[1],lon2=-79.31078,lat2=43.66714)

# Implement the two-sided t-test!
t.test2 <- function(m,s,n1,n2,m0=0,equal.variance=FALSE) {
  m1=m[1]
  m2=m[2]
  s1=s[1]
  s2=s[2]
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(diff=m1-m2, se=se, t=t, p=2*pt(-abs(t),df))    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat)
}

# Implement the Chow test for two regressions: unrestricted(u) and restricted(r)
chow.test <- function(r1,r2,u) {
  # Get the number of parameters
  k <- u$coefficients %>% length
  # Get the number of observations for n1 and n2
  n1 <- nobs(r1)
  n2 <- nobs(r2)
  # Get the Sum of squared residuals from the unrestricted (Sc), restricted 1 (S1) and restricted 2 (S2) model
  Sc <- deviance(u)
  S1 <- deviance(r1)
  S2 <- deviance(r2)
  # Calculated the F-statistics
  stat.F <- ((Sc-(S1+S2))/k)/((S1+S2)/(n1+n2-2*k))
  # The F stat has k and n1+n2-2*k degrees of freedom
  f1 <- k
  f2 <- n1+n2-2*k
  p.F <-pf(stat.F,f1,f2,lower.tail = F)
  print(p.F)
  # Return p-value
  return(p.F)
}
