library(runner)


#date <- as.POSIXct(Sys.Date() + cumsum(sample(1:5, 20, replace = TRUE))) # unequally spaced time series
date <- as.POSIXct(seq(1, 86400, 60), origin = "2022-01-01")

x <- seq(1, length(date))

runner(
  x, 
  k = "1 hour",
  idx = date, 
  f = sum
)