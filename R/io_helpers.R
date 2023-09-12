# TODO Import this file from a package on GitLab

read_data_catergory_and_cleanup <- function(folder_list, in_folder = NA, cleanup_temp_dir = FALSE) {
  
  matching_files <- folder_list |>
    list.files(
      pattern = FILE_PATTERN, # pattern in filename to look for. Can be an extension or something else
      full.names = TRUE,
      recursive = TRUE
    ) |>
    remove_elements_without(in_folder) # remove all that are not in the desired folder
  
  message(paste0(
    Sys.time(),
    ": Reading ",
    min(FILES_MAX, length(matching_files)),
    " of ",
    length(matching_files),
    " discovered files for ",
    in_folder,
    " ..."
  ))
  
  df <- get_data_from_path_list_dt(matching_files)
  
  if (cleanup_temp_dir) {
    # clean up temporary files
    print(paste0(
      Sys.time(),
      ": Cleaning up temporary data..."
    ))
    
    # filter the paths to only those in the session's temporary folder
    files_to_delete <- matching_files[grepl(
      paste0("^", tempdir()), # starts with temp directory path
      matching_files
    )]
    
    unlink(files_to_delete)
  }
  
  message(paste0(Sys.time(),": Done."))
  return(df)
}


# remove path names that don't contain a certain substring (or folder) name
remove_elements_without <- function(paths, substring) {
  
  if (is.na(substring)) {
    return(paths)
  } else {
    non_matching <- grep(
      pattern = substring,
      x = paths,
      invert = TRUE,
      value = FALSE
    )
    
    return(paths[-non_matching])
  }
}


get_data_from_path_list_df <- function(path_list) {
  # with map
  # Very slow, so it's not used anymore in favor of
  # the data.table version of this function
  progressr::handlers("txtprogressbar") # Add a text progress bar
  p <- progressr::progressor(
    steps = min(FILES_MAX, length(path_list))
  )
  
  df <- path_list |>
    head(n = FILES_MAX) |>
    map(function(x) {
      p()  # increment progress bar
      
      match_in_path <- str_extract(x, PATTERN_IN_PATH)
      file_name <- tools::file_path_sans_ext(basename(x))
      
      # set study number to unknown if pattern was not found
      if (is.na(match_in_path)) {match_in_path <- "Pattern not found"}
      
      suppressWarnings( # suppress "The following named parsers don't match the column names: value.source.id" warnings
        read_csv(
          x,
          n_max = ROWS_MAX,
          show_col_types = FALSE,
          col_types = cols(
            value.source.id = col_character(), # prevent data type conflict in activity log
            .default = col_guess()
          )
        ) |>
          mutate(
            study.number = match_in_path,
            debug.file.name = file_name,
            .before = 1
          ))
    })  |>
    (\(data) { # output the message before list_rbind()
      print(paste0(
        Sys.time(),
        ": Binding resulting data frame..."
      ));
      data # pass data through
    })() |>
    list_rbind() # bind to dataframe according to deprecated map_dfr documentation
}


get_data_from_path_list_dt <- function(path_list) {
  # with fread and data.table
  
  df <- path_list |>
    head(n = FILES_MAX) |>
    pbapply::pblapply(
      function(x) {
        match_in_path <- str_extract(x, PATTERN_IN_PATH)
        file_name <- tools::file_path_sans_ext(
          basename(x),
          compression = TRUE # remove .gz first
        )
        
        # set study number to unknown if pattern was not found
        if (is.na(match_in_path)) {match_in_path <- "Pattern not found"}
        
        # browser()
        
        dt_i <- data.table::fread(x, nrows = ROWS_MAX)
        dt_i[,
             c("match.in.path", "file.name") := .(match_in_path, file_name)
        ] # adds columns to the end
        data.table::setcolorder( # move match.in.path and file.name to front, which is a bit tricky with data.table
          dt_i,
          c(
            "match.in.path",
            "file.name",
            setdiff(names(dt_i), c("match.in.path", "file.name"))
          )
        )
        return(dt_i)
      }
    ) |>
    (\(data) { # output the message before rbindlist()
      print(paste0(
        Sys.time(),
        ": Binding resulting data table..."
      ));
      data # pass through data as return
    })() |>
    data.table::rbindlist(
      fill=TRUE # there are some files that have additional meta data columns. Fill these with NA here for rows that don't have them
    ) |>
    as_tibble()
}


# Shorten the actual search path, so it can be printed to the console
get_search_path_for_print <- function(dir_path) {
  # Replace all "\\" and "/" with the current OS's file separator
  normalized_path_string <- gsub("\\\\|/", .Platform$file.sep, dir_path)
  
  # Split the normalized string at the file separator
  split_path_string <- strsplit(normalized_path_string, .Platform$file.sep)[[1]] |> unlist()
  
  # Find the indices of all "Temp" elements
  temp_indices <- which(split_path_string == "Temp")
  
  if (!length(temp_indices)) {
    return(dir_path)
  }
  
  # Skip everything before the first "Temp" and the temp folder
  new_path_string <- split_path_string[-(1:(temp_indices[1] + 1))]
  
  # Reconstruct the path from new_path_string with the platform separator
  new_path <- paste(new_path_string, collapse = .Platform$file.sep)
  
  return(new_path)
}



