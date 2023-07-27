# chatgpt prompt
#'I have two time series (the time is given as datetime in POSIXct) in 
#'data frames, df_min, which is sampled once per minute, and df_h which is 
#'sampled once per hour. Note that df_h is not necessarily sampled Based on 
#'the datetime values in df_h, per value I want to find cell with the same 
#'timestamp in df_min. From that timestamp on, I want to add up the 60 values 
#'(one per minute) that came before and store it in a new dataframe df_sum. 
#'The resulting dataframe should have the same timestamps as df_h.



if (FALSE) { # original code
  # Sample data frames
  df_min <- data.frame(Time = as.POSIXct(c("2022-01-01 00:00:00", "2022-01-01 00:01:00", "2022-01-01 00:02:00", "2022-01-01 01:00:00", "2022-01-01 01:01:00", "2022-01-01 01:02:00")), Value = c(1, 2, 3, 4, 5, 6))
  df_h <- data.frame(Time = as.POSIXct(c("2022-01-01 00:00:00", "2022-01-01 01:00:00", "2022-01-01 02:00:00")), Value = c(10, 20, 30))
  
  # Create new data frame with hourly timestamps
  df_sum <- data.frame(Time = df_h$Time)
  
  # Loop through each hour in df_h
  for (i in seq_along(df_h$Time)) {
    # Find the index of the timestamp in df_min that matches the current hour in df_h
    min_index <- which(df_min$Time == df_h$Time[i])
    
    # If there is no exact match, find the index of the closest timestamp before the current hour in df_h
    if (length(min_index) == 0) {
      min_index <- max(which(df_min$Time < df_h$Time[i]))
    }
    
    # Get the previous 60 values from df_min
    values <- df_min$Value[(min_index - 59):min_index]
    
    # Calculate the sum of the previous 60 values and store it in the corresponding row of df_sum
    df_sum$Value[i] <- sum(values)
  }
  
  # Print the resulting data frame
  print(df_sum)
}


# my adjusted version -----------------------------------------------------

library(tidyverse)
library(runner) # for rolling functions

# Sample data frames
df_min <- seq(0, 86400, by = 60) |> 
  as.POSIXct(origin = "2022-01-01") %>%
  tibble(Datetime = ., Value = seq(0, 86400, by = 60)) 

df_h <- seq(3600+60, 86400, by = 3600) |> 
  as.POSIXct(origin = "2022-01-01") %>%
  tibble(Datetime = ., Value = seq(3600+60, 86400, by = 3600))

# Create new data frame with hourly timestamps
df_sum <- tibble(Time = df_h$Datetime, Value = rep(NA, nrow(df_h)))

rw <- runner(
  df_min$Value, 
  k = "1 hour",
  idx = df_min$Datetime, 
  f = sum
)


# Loop through each hour in df_h
for (i in seq_along(df_h$Time)) {
  # Find the index of the timestamp in df_min that matches the current hour in df_h
  min_index <- which(df_min$Time == df_h$Time[i])
  
  # If there is no exact match, find the index of the closest timestamp before the current hour in df_h
  if (length(min_index) == 0) {
    min_index <- max(which(df_min$Time < df_h$Time[i]))
  }
  
  # Get the previous 60 values from df_min
  values <- df_min$Value[(min_index - 59):min_index]
  
  # Calculate the sum of the previous 60 values and store it in the corresponding row of df_sum
  df_sum$Value[i] <- sum(values)
}

# Print the resulting data frame
print(df_sum)

