meanna <- function(x) {
  #' Calculate average. Ignore NA, but return NA if all elements are NA 
  if(all(is.na(x))) NA
  else mean(x, na.rm = TRUE)
}


sumna <- function(x) {
  #' Calculate sum. Ignore NA, but return NA if all elements are NA 
  if(all(is.na(x))) NA
  else sum(x, na.rm = TRUE)
}


get_stat_per_day <- function(df, stat, obs_id) {
  #' df:    two column data frame with the first column being POSIXct dates 
  #'        and the second column being values 
  #' stat:  stat to compute ("sum" or "mean")
  #' obs:   observation id of the stat per day
  
  # convert datetime to date and group by day
  df <- df |> mutate(
    Date = as.Date(Datetime),
    .keep = "unused",
    .before = 1
  ) |> 
    group_by(Date) 
  
  value_col <- names(df)[2] # necessary to index second column
  
  # compute stat
  if (stat == "sum") {
    df <- df |> summarize(
      value_day = sumna(.data[[value_col]]), # ignore NA
      n = n()
    )
  } else if (stat == "mean") {
    df <- df |> summarize(
      value_day = round(meanna(.data[[value_col]]), 1), # ignore NA
      n = n()
    )
  }
  
  # add observation id column & arrange by date
  df <- df %>%
    mutate(beep = rep(obs_id, times = nrow(.))
    ) |> 
    arrange(Date)
  
  return(df)
}


get_time_window_before <- function(datetime, secs_before, unit, step_size) {
  #' takes a datetime input and creates a vector of datetimes before that datetime
  #' resulting time stamps are rounded down to the nearest minute, second etc., depending 
  #' on the chosen resolution
  #' 
  #' Arguments
  #' secs_before:   number of seconds before the input datetime
  #' unit:          Resolution of the rounding in seconds -> string:("mins", "secs")
  #' step_size:     Step size of the resulting time vector. whole multiple of the resolution
  #' 
  #' Output
  #' window:        Vector of time stamps before datetime
  
  difference_in_secs <- {if (unit=="mins") 60 
    else if (unit=="secs") 1 
    else NA
  }
  
  steps = paste(step_size, unit)
  
  window <- seq.POSIXt(
    from = floor_date( # round so that timestamps match with fitbit timestamps
      datetime - secs_before + difference_in_secs, # remove beep field
      unit
    ), 
    to = datetime,
    by = steps
  )
  
  return(window)
}