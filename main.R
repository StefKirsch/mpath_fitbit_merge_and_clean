#' This script takes the data downloaded from 
#' - the m-path server and
#' - fitabase 
#' 
#' The data is merged transferred into the desired tidy format. 
#' Some base statistics (mean per day, minute and hour) are computed as well, 
#' some of them in the time window before the ESM beeps 
#' 
#' Requirements:
#' You need to have all the libraries below installed
#' It is recommended to stick to the proposed structure
#' 
#' base directory
#' |--input
#' |--|--esm
#' |--|--fitbit
#' |--|--|--ID_heartrate_1_min_.csv
#' |--|--|--ID_heartrate_seconds_.csv
#' |--|--|--ID_minuteStepsNarrow_.csv
#' |--output
#' |--main.R
#' |--translation_key.xlsx
#' |--ideal_dataset_ESM.xlsx (just for reference)
#'  
#' At the moment, you have to manually adjust the file names per time 
#' window/individual. The script will return one excel file each time it is run.
#' Joining the excel files can be done manually or you can write your own 
#' script. 
#' 
#' Execution of this script can take a while. Make sure to wait until you 
#' get the message "done" in the console.
#' 
#' #' I tested the script with the sample data I received and also tested it 
#' with incomplete data sets. It seems reasonably robust to those. There might
#' be some edge cases though where the script breaks down.  
#' 
#' Tip: You can collapse the sections below in R and jump to a section with
#' with the navigation pane on the right (you might have to make it visible)


# File names --------------------------------------------------------------

# update these per participant
file_name_esm <- "ESM_006_week1.xlsx" 

file_name_steps <- "006_minuteStepsNarrow_20211130_20230117.csv"
file_name_hr <- "006_heartrate_seconds_20211201_20230117.csv"

# libraries ---------------------------------------------------------------
library(tidyverse)
library(janitor) # to rename columns
library(fs) # file system
library(readxl) # read excel sheets
library(openxlsx) # write to xlsx
library(anytime) # parse datetime strings
library(hms)  # convert POSIXct objects to hms format
library(data.table) # to rename columns with new/old name index
library(tools) # for file path manipulations
library(lubridate)
 
# functions ---------------------------------------------------------------
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

# data patterns -----------------------------------------------------------

INPUT_PATH <- "input"
OUTPUT_PATH <- "output"

# metadata per row
IDS_VARS <- c(
  "id",
  "age",
  "sex",
  "status",
  "day",
  "beep",
  "obs",
  "Date",
  "Time"
)

# ESM
ESM_FOLDER <- "esm"
ESM_VARS <- c(
  "TimeCategory",
  "Fitbit_steps_day", 
  "Fitbit_steps_hour_before", 
  "Fitbit_HR_day", 
  "Fitbit_HR_min_before"
  )
TIME_CATEGORIES <- c("Morning", "ESM",	"ESM",	"ESM",	"ESM",	"ESM",	"Evening")
N_BEEPS <- length(TIME_CATEGORIES)
# duration to consider before beep 
WIN_STEPS_BEFORE_BEEP <- 3600 # seconds. 
WIN_HR_BEFORE_BEEP <- 60  # seconds. 

# converts elemts of variable names to regexp pattern to match
PAT_MUL_CHOICE <- str_escape( 
    c("(multipleChoice)")
  )

PAT_NEG_POS  <- str_escape(
    c( "(sliderNegPos)")
  )

PAT_YESNO <- c(
  "(yesno)", 
  "_janee"
  ) |> 
  str_escape() |> 
  paste(collapse = "|") %>% # magrittr pipe to allow 
                            # changing the location of the main argument
  paste("(", ., ")", sep = "")

# fitabase
FITBIT_FOLDER <- "fitbit"
#FITBIT_FOLDER <- "fitbit/Incomplete datasets for tests" # for testing

DATE_FORMAT_TARGET <- "%d-%m-%Y"

# read data ---------------------------------------------------------------

# ESM
#ROWS_MAX_FITBIT <-  60*60*24 # set to inf for production
ROWS_MAX_FITBIT <- Inf

df_esm_raw <- read_excel(
  file.path(INPUT_PATH, ESM_FOLDER, file_name_esm),
  col_names = TRUE,
  skip = 1
  ) |>
  mutate( # add POSIXct Datetime
     Datetime = utctime(`Date and time`, tz = "UTC"), # assume UTC as default
    .before = 1
  )

#' read variable names translation file. 
#' This also contains all desired ESM variable names
TRANSLATION_KEY <- read_excel( 
  # must be an excel file to read special characters like "ï"
  file.path(INPUT_PATH, "translation_key.xlsx")
)

# concatenate vector of all target variable names to be selected
ALL_VARS = c(IDS_VARS, ESM_VARS, TRANSLATION_KEY$english)

# fitbit
df_steps <- read_csv( # steps
  file.path(INPUT_PATH, FITBIT_FOLDER, file_name_steps),
  col_names = TRUE,
  n_max = ROWS_MAX_FITBIT 
  ) |> 
  rename(Datetime = ActivityMinute) |> 
  mutate( # convert to posixct
    Datetime = mdy_hms(Datetime, truncated = 1, tz = "UTC")
  ) |> 
  arrange(Datetime) # sort by datetime


df_hr <- read_csv( # heart rate
  file.path(INPUT_PATH, FITBIT_FOLDER, file_name_hr),
  col_names = TRUE,
  n_max = ROWS_MAX_FITBIT 
  ) |> 
  rename(Datetime = Time) |> 
  rename(Hr = Value) |> 
  mutate( # convert to posixct
    Datetime = mdy_hms(Datetime, truncated = 1, tz = "UTC")
  ) |> 
  arrange(Datetime) # sort by datetime

# Stats per day -----------------------------------------------------------

obs_per_day <- 7

df_steps_per_day <- df_steps |> 
  get_stat_per_day(
    stat = "sum", 
    obs_id = obs_per_day
    ) |> 
  rename(Fitbit_steps_day = value_day)

df_hr_per_day <- df_hr |> 
  get_stat_per_day(
    stat = "mean", 
    obs_id = obs_per_day
  ) |> 
  rename(Fitbit_HR_day = value_day)

df_fitbit_per_day <- merge( # merge df, matched with date
  df_steps_per_day,
  df_hr_per_day,
  by = c("Date", "beep"), # if timestamp is missing in one df, value will be NA in respective column
  all = TRUE,
  suffixes = c(".steps",".hr")
  ) |>  
  mutate(  
    Date = format(Date, DATE_FORMAT_TARGET),
    .before = 1,
    .keep = "unused"
  )
  
# stats per beep (rolling window before each beep) ------------------------
df_windows_steps <- df_windows_hr <- tibble( # empty dfs
  Datetime = POSIXct(), 
  obs = numeric()
  )
 
for (obs in 1:nrow(df_esm_raw)) { # loop over time stamps in ESM data
  
  # generate time vector with time stamps to be considered before each ESM beep
  cur_window_steps <- get_time_window_before(
    datetime = df_esm_raw$Datetime[obs],
    secs_before = WIN_STEPS_BEFORE_BEEP, 
    unit = "mins", # per minute
    step_size = 1  # one minute
    )
  
  cur_window_hr <- get_time_window_before(
    datetime = df_esm_raw$Datetime[obs],
    secs_before = WIN_HR_BEFORE_BEEP, 
    unit = "secs", # per second
    step_size = 3  # 3 seconds
  )
  
  # append the current window of steps to be considered to the vector of of all
  # time stamps to be considered. Which observation the window belongs to is 
  # identified by the obs_per_timestamp column, which is built below.
  df_windows_steps <- bind_rows( 
    df_windows_steps, 
    tibble(
      Datetime = cur_window_steps,
      obs = rep(obs, length(cur_window_steps))
      )
  )
  
  df_windows_hr <- bind_rows( 
    df_windows_hr, 
    tibble(
      Datetime = cur_window_hr,
      obs = rep(obs, length(cur_window_hr))
    )
  )
}

df_steps_per_beep <- merge( # merge df, matched with time stamp
  df_steps, 
  df_windows_steps, 
  by = "Datetime", # if timestamp is missing in one df, value will be NA in respective column
  all = TRUE
  ) |> 
  arrange(Datetime) |>  # sort by datetime
  drop_na(obs) |>  # drop timestamps that don't belong to an observation
  group_by(obs) |> # group by observation for summarize
  summarize( # rolling window for steps, heart rate and timestamps per observation
    Datetime = max(Datetime), # last timestamp per group
    Fitbit_steps_hour_before = sumna(Steps), # ignore NA
    n = n()
    )

df_hr_per_beep <- merge( # merge df, matched with time stamp
  df_hr, 
  df_windows_hr, 
  by = "Datetime", # if timestamp is missing in one df, value will be NA in respective column
  all = TRUE
  ) |> 
  arrange(Datetime) |>  # sort by datetime
  drop_na(obs) |>  # drop timestamps that don't belong to an observation
  group_by(obs) |> # group by observation for summarize
  summarize( # rolling window for steps, heart rate and timestamps per observation
    Datetime = max(Datetime), # last timestamp per group
    Fitbit_HR_min_before = round(meanna(Hr), 1), # ignore NA
    n = n()
  )

# Merge together fitbit data per beep
# leave in n and datetime for debugging
df_fitbit_per_beep <- merge(
  df_steps_per_beep,
  df_hr_per_beep,
  by = c("obs"),
  all = TRUE
)

# Tidy up ESM data frame ----------------------------------------------------

df_esm_tidy <- df_esm_raw |>
  # split up "time and date" column to separate columns
  # anytime functions to parse datetime strings
  mutate(  
    Date = format(anydate(`Date and time`), DATE_FORMAT_TARGET),
    Time = as_hms(anytime(`Date and time`)),
    .after = Datetime,
    .keep = "unused"
  ) |>    
  # factor columns: transfer to factor by containing substring 
  # and remove substring from name
  mutate(
    across(
      matches(PAT_MUL_CHOICE), 
      as.factor 
    ),
    .keep = "unused"
  ) |>
  rename_with(
    ~ str_trim(str_remove(., PAT_MUL_CHOICE)) # remove substring and whitespace
  ) |> 
  # Same for NegPos slider replies (convert to numeric)
  mutate(
    across(
      matches(PAT_NEG_POS), 
      as.numeric
    ),
    .keep = "unused"
  ) |> 
  rename_with(  # remove variable tag
    ~ str_trim(str_remove_all(., PAT_YESNO)) 
  ) |> 
  mutate( # Same for yes-no replies (convert to factor)
    across(
      matches(PAT_YESNO), 
      as.factor
    ),
    .keep = "unused"
  ) |> 
  rename_with( # remove variable tag
    ~ str_trim(str_remove(., PAT_NEG_POS)) 
  ) |> 
  # Translate names
  setnames(
    old = TRANSLATION_KEY$dutch,
    new = TRANSLATION_KEY$english,
    skip_absent = FALSE
  ) |> 
  # observation number (= row number)
  mutate( 
    obs = row_number(),
    .before = Date
  ) |> 
  # add beep ids by grouping for days
  group_by(
    Date
  ) |> 
  # add beep id column
  mutate( 
    beep = row_number(),
    .before = obs
  ) |> 
  # add day id column
  mutate( 
    day = cur_group_id(),
    .before = beep
  )

# Merge ESM and fitbit dataframes -----------------------------------------

df_both <- merge( # merge df, matched with time stamp
    df_esm_tidy, 
    df_fitbit_per_beep, 
    by = "obs", # if timestamp is missing in one df, value will be NA in respective column
    all = FALSE,
    sort = TRUE # sort by column in "by" argument, for debugging
  ) |> 
  select(!contains("Datetime")) |>  # remove all Datetime columns
  relocate( # for debugging
    Fitbit_steps_hour_before:Fitbit_HR_min_before, 
    .after = Time
    ) |> 
  mutate( # add time categories per observation
    TimeCategory = TIME_CATEGORIES[beep],
    .after =Time
  ) |>
  merge( # add per day columns and rows
    df_fitbit_per_day,
    by = c("Date", "beep"),
    all = TRUE # keep all rows
  ) |>
  arrange("obs") |>  # sort rows by observation
  select(any_of(ALL_VARS)) |>  # keep target vars only
  drop_na("obs") # remove fitbit rows with no matching ESM observation

# write result to excel sheet
file.path(OUTPUT_PATH, file_name_esm) |> 
  file_path_sans_ext() |> 
  paste("_fitbit.xlsx", sep = "") %>% 
  write.xlsx(df_both, ., keepNA = TRUE) # this transfers NA into 
  # Excel's own convention for missing values (#N/A). You can change this to
  # FALSE, then missing values will be represented as empty cells
  # In that case, you have to be careful that they won't be read as 0, though!
  
print("done")