
# set type and remove pattern from variables
format_cols_by_pat <- function(df, pattern, target_type) {
  
  #browser()
  
  df |>
    mutate(
      across(
        matches(pattern),
        function(x) {convert_type(x, target_type)}
      ),
      .keep = "unused"
    ) |>
    rename_with(
      ~ str_trim(str_remove_all(., pattern)) # remove substring and whitespace
    )
}


convert_type <- function(x, target_type) {
  switch(
    target_type,
    "numeric" = as.numeric(x),
    "integer" = as.integer(x),
    "character" = as.character(x),
    "logical" = as.logical(x),
    "factor" = as.factor(x),
    stop(paste0("Invalid target type \'", target_type ,"\' specified"))
  )
}