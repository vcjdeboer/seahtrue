# This is a common place to define small utilities that are used inside multiple package functions.
# Since they serve as helpers to multiple functions, placing them in R/utils.R makes them easier to re-discover
# when you return to your package after a long break.


between_min_max <- function(col_1, col_2, min, max){
  false_list <- list()
  true_false = between(as.numeric(col_2), min, max)
  if(true_false == FALSE){
    false_list <- append(false_list, col_1)
    false_list <- append(false_list, col_2)
    false_list <- append(false_list, true_false)
    false_list <- append(false_list, glue::glue(paste("Given column value for {col_1} {col_2} is not between {min} and {max}.")))
    return(false_list)
  }}

check_range <- function(col_1, col_2, min, max){

  # loop over 2 columns, check if value between min and max.
  x <- map2(.x = col_1,
            .y = col_2,
            ~between_min_max(.x, as.numeric(.y), min, max))

  # remove NULL values from nested list.
  x[sapply(x, is.null)] <- NULL

  for(value in x){
    cli::cli_alert_warning(value[4])
  }

  return(x)
}

