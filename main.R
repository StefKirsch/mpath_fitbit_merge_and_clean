
# libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table) # just for some operations, because it's faster than tibble manipulation
library(janitor) # to rename columns
library(fs) # file system
library(readxl) # read excel sheets
library(openxlsx) # write to xlsx
library(anytime) # parse datetime strings
library(hms)  # convert POSIXct objects to hms format
library(data.table) # to rename columns with new/old name index
library(tools) # for file path manipulations
library(lubridate) 
library(rstudioapi) # for selectDirectory()
library(glue)

 
# load helper function ---------------------------------------------------------------
source("R/stat_helpers.R")
source("R/io_helpers.R")
source("R/col_helpers.R")


# functions --------------------------------------------

debug_file_exists <- function() {
  # Define the path to the DEBUG file in the root folder
  debug_file_path <- "DEBUG"
  
  # Check if the file exists
  file_exists <- file.exists(debug_file_path)
  
  return(file_exists)
}


# Debug options --------------------------------------

DEBUG <- debug_file_exists()

#ROWS_MAX_ <-  60*60*24 # set to inf for production
ROWS_MAX <- Inf
FILES_MAX <- Inf 

# data patterns -----------------------------------------------------------

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

# observations per day
obs_per_day <- 7

## fitabase --------------------------------

FILE_PATTERN <- "csv"

steps_filename_pattern <- "minuteStepsNarrow"
hr_filename_pattern <- "heartrate_seconds"

PATTERN_IN_PATH_FITBIT <- "(\\d{3})(?=_)" # matches three numeric characters and an underscore
NAME_MATCH_IN_PATH_FITBIT <- "id"

DATE_FORMAT_TARGET <- "%d-%m-%Y"


## mpath/ESM ----------------------------------------

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
  "_janee",
  "(yesno)" 
) |> 
  str_escape() |> 
  paste(collapse = "|") %>% # magrittr pipe to allow changing the location of the main argument
  paste("(", ., ")", sep = "")

PAT_BODYPARTS  <- str_escape(
  c( "(bodyParts)")
)

PATTERN_IN_PATH_ESM <- "(?<=ESM_)(\\d{3})(?=_week)" # e.g. ESM_002_week matches 002
NAME_MATCH_IN_PATH_ESM <- "id"

# File pickers ------------------------------------------------------

if (!(DEBUG && exists("provided_user_input"))) { # avoid having to re-specify input during debugging
  input_folder_esm <- selectDirectory(
    caption = "Select the input folder for the M-PATH ESM data.",
    label = "Select",
    path = getActiveProject()
  )
  
  input_folder_fitbit <- selectDirectory(
    caption = "Select the input folder for the FITBIT data.",
    label = "Select",
    path = getActiveProject()
  )
  
  # Output folder
  output_folder <- selectDirectory(
    caption = "Select the root directory where the OUTPUT should be stored.",
    label = "Select",
    path = getActiveProject()
  )
  
  provided_user_input <- TRUE
}


# ESM ------------------------------------------------

folder_name_esm <- gsub( # name for resulting file
  pattern = "\\s+",
  replacement = "_",
  x = basename(input_folder_esm)
)

esm_files <- list.files(
  path = input_folder_esm,
  pattern = ".xlsx$",
  full.names = TRUE,
  recursive = FALSE
)

df_esm_no_ext <- esm_files |> 
  map(function(x) { # map read_excel() so that a list of file paths can be read
    
    # extract participant id
    match_in_path <- str_extract(x, PATTERN_IN_PATH_ESM)
    if (is.na(match_in_path)) {
      stop(paste("Pattern", PATTERN_IN_PATH_ESM, "not found in path", x))
    }
    
    read_excel(
      path = x,
      col_names = TRUE,
      skip = 1
    ) |> 
    mutate(
      {{NAME_MATCH_IN_PATH_ESM}} := match_in_path
    ) |> 
    format_cols_by_pat(
      pattern = PAT_MUL_CHOICE,
      target_type = "factor"
    ) |> 
    format_cols_by_pat(
      pattern = PAT_BODYPARTS,
      target_type = "character"
    ) |> 
    format_cols_by_pat(
      pattern = PAT_YESNO,
      target_type = "factor"
    ) |> 
    format_cols_by_pat(
      pattern = PAT_NEG_POS,
      target_type = "numeric"
    )
  }) |> 
  list_rbind() |> # bind to dataframe according to deprecated map_dfr documentation 
  relocate( # move id to the beginning
    id,
    .before = 1
  ) |>
  mutate( # add POSIXct Datetime
    Datetime = utctime(`Date and time`, tz = "UTC"), # assume UTC as default
    .after = id
  )


## Translation key ----------------------

#' read variable names translation file. 
#' This also contains all desired ESM variable names
TRANSLATION_KEY <- read_excel( 
  # must be an excel file to read special characters like "ï"
  file.path("translation_key.xlsx")
)

# concatenate vector of all target variable names to be selected
ALL_VARS = c(IDS_VARS, ESM_VARS, TRANSLATION_KEY$english)


cols_to_translate <- setdiff(
  colnames(df_esm_no_ext), 
  c("Datetime","Date and time")
)

cols_no_translation <- setdiff(
  cols_to_translate, 
  TRANSLATION_KEY$dutch
)

if (length(cols_no_translation)) {
  warning(paste(
    "Some columns in ESM data don't have a translation:", 
    paste(cols_no_translation, collapse = "\n"),
    "Check the translation key file and add the missing columns.",
    sep = "\n"
  ))
}


## Tidy up ESM data frame ----------------------------------------------------

df_esm_tidy <- df_esm_no_ext |> 
  # split up "time and date" column to separate columns
  # anytime functions to parse datetime strings
  mutate(  
    Date = anydate(`Date and time`),
    Time = as_hms(anytime(`Date and time`)),
    .after = Datetime,
    .keep = "unused"
  ) |>  
  # Translate names
  setnames(
    old = TRANSLATION_KEY$dutch,
    new = TRANSLATION_KEY$english,
    skip_absent = TRUE # FALSE produces an error if a column is missing in the dataframe
  ) |> 
  arrange(
    id,
    Datetime
  ) |> 
  # observation number (= row number)
  mutate( 
    obs = row_number(),
    .before = Date
  ) |> 
  group_by(Date) |>  # make sure to interpret "Date" as date, so the group order is correct. This should fix issue #1
  # add beep id column
  mutate( 
    beep = row_number(),
    .before = obs
  ) |> 
  # add day id column
  mutate( 
    day = cur_group_id(),
    .before = beep
  ) |> 
  mutate(Date = format(Date, DATE_FORMAT_TARGET)) # converts to character


# fitbit ------------------------------------

## steps ------------------------------------

df_steps <- list.files(
    path = input_folder_fitbit,
    pattern = steps_filename_pattern,
    full.names = TRUE,
    recursive = TRUE
  ) |> 
  get_data_from_path_list_dt(pattern_in_path = PATTERN_IN_PATH_FITBIT) |> 
  rename( # rename id id
    {{NAME_MATCH_IN_PATH_FITBIT}} := match.in.path
    ) |> 
  rename(Datetime = ActivityMinute) |> 
  mutate( # convert to posixct
    Datetime = mdy_hms(Datetime, truncated = 1, tz = "UTC")
  ) |> 
  arrange(Datetime) # sort by datetime


## heart rate ------------------------------------

df_hr <- list.files(
    path = input_folder_fitbit,
    pattern = hr_filename_pattern,
    full.names = TRUE,
    recursive = TRUE
  ) |> 
  get_data_from_path_list_dt(pattern_in_path = PATTERN_IN_PATH_FITBIT) |> 
  rename( # rename id number
    {{NAME_MATCH_IN_PATH_FITBIT}} := match.in.path
  ) |> 
  rename(Datetime = Time) |> 
  rename(Hr = Value) |> 
  mutate( # convert to posixct
    Datetime = mdy_hms(Datetime, truncated = 1, tz = "UTC")
  ) |> 
  arrange(Datetime) # sort by datetime


##  Stats per day -----------------------------------------------------------

df_steps_per_day <- df_steps |> 
  get_stat_per_day(
    value_col = "Steps",
    stat = "sum", 
    obs_id = obs_per_day
    ) |> 
  rename(Fitbit_steps_day = value_day)

df_hr_per_day <- df_hr |> 
  get_stat_per_day(
    value_col = "Hr",
    stat = "mean", 
    obs_id = obs_per_day
  ) |> 
  rename(Fitbit_HR_day = value_day)

df_fitbit_per_day <- merge( # merge df, matched with date
  df_steps_per_day,
  df_hr_per_day,
  by = c("id", "Date", "beep"), # if timestamp is missing in one df, value will be NA in respective column
  all = TRUE,
  suffixes = c(".steps",".hr")
  ) |>  
  mutate(  
    Date = format(Date, DATE_FORMAT_TARGET),
    .before = 1,
    .keep = "unused"
  )
  
## stats per beep (rolling window before each beep) ------------------------
df_windows_steps <- df_windows_hr <- tibble( # empty dfs
  id = character(),
  Datetime = POSIXct(), 
  obs = numeric()
  )
 
for (obs in 1:nrow(df_esm_tidy)) { # loop over time stamps in ESM data
  
  cur_id <- df_esm_tidy$id[obs]
  
  # generate time vector with time stamps to be considered before each ESM beep
  cur_window_steps <- get_time_window_before(
    datetime = df_esm_tidy$Datetime[obs],
    secs_before = WIN_STEPS_BEFORE_BEEP, 
    unit = "mins", # per minute
    step_size = 1  # one minute
    )
  
  cur_window_hr <- get_time_window_before(
    datetime = df_esm_tidy$Datetime[obs],
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
      id = cur_id,
      Datetime = cur_window_steps,
      obs = rep(obs, length(cur_window_steps))
      )
  )
  
  df_windows_hr <- bind_rows( 
    df_windows_hr, 
    tibble(
      id = cur_id,
      Datetime = cur_window_hr,
      obs = rep(obs, length(cur_window_hr))
    )
  )
}

# merge df, matched with time stamp and id id
df_steps_per_beep <- merge( 
  as.data.table(df_steps), # use data.table merge() method, because it's much faster 
  as.data.table(df_windows_steps), 
  by = c("id", "Datetime")#, # if timestamp is missing in one df, value will be NA in respective column
  #all = TRUE
  ) |> 
  as_tibble() |>
  arrange(Datetime) |>  # sort by datetime
  drop_na(obs) |>  # drop timestamps that don't belong to an observation
  group_by(id, obs) |> # group by participants and observation for summarize
  summarize( # rolling window for steps, heart rate and timestamps per observation
    Datetime = max(Datetime), # last timestamp per group
    Fitbit_steps_hour_before = sumna(Steps), # ignore NA
    n = n(),
    .groups = "drop"
    )

# merge df, matched with time stamp and participant id
df_hr_per_beep <- merge( 
  as.data.table(df_hr), # use data.table merge() method, because it's much faster 
  as.data.table(df_windows_hr), 
  by = c("id", "Datetime") #, # if timestamp is missing in one df, value will be NA in respective column
  #all = TRUE
  ) |> 
  as_tibble() |> # seems to convert dt implicitly to tibble when using dplyr commands, but this is clearer 
  arrange(Datetime) |>  # sort by datetime
  drop_na(obs) |>  # drop timestamps that don't belong to an observation
  group_by(id, obs) |> # group by participants and observation for summarize
  summarize( # rolling window for steps, heart rate and timestamps per observation
    Datetime = max(Datetime), # last timestamp per group
    Fitbit_HR_min_before = round(meanna(Hr), 1), # ignore NA
    n = n(),
    .groups = "drop"
  )

# Merge together fitbit data per beep
# leave in n and datetime for debugging
df_fitbit_per_beep <- merge(
  df_steps_per_beep,
  df_hr_per_beep,
  by = c("id", "obs"),
  all = TRUE
)


# Merge ESM and fitbit dataframes -----------------------------------------

# TODO Make sure participant stays in there

df_both <- merge( # merge df, matched with time stamp
    df_esm_tidy, 
    df_fitbit_per_beep, 
    by = c("id", "obs"), # if timestamp is missing in one df, value will be NA in respective column
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
    .after = Time
  ) |>
  merge( # add per day columns and rows
    df_fitbit_per_day,
    by = c("id", "Date", "beep"),
    all = TRUE # keep all rows
  ) |> 
  select(any_of(ALL_VARS)) |>  # keep target vars only
  drop_na("obs") |>  # remove fitbit rows with no matching ESM observation
  arrange(obs) |>   # sort rows by observation
  mutate(
    week = floor((day - 1)/7) + 1,
    .after = id
  )

# Export result --------------------------------

# write result to excel sheet
file.path(output_folder, folder_name_esm) |> 
  file_path_sans_ext() |> 
  paste("_fitbit.xlsx", sep = "") %>% 
  write.xlsx(df_both, ., keepNA = TRUE) # this transfers NA into 
  # Excel's own convention for missing values (#N/A). You can change this to
  # FALSE, then missing values will be represented as empty cells
  # In that case, you have to be careful that they won't be read as 0, though!
  
message("Script completed! 🎉🎉")
