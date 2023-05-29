# This is a common place to define small utilities that are used inside multiple package functions.
# Since they serve as helpers to multiple functions, placing them in R/utils.R makes them easier to re-discover
# when you return to your package after a long break.

check_excel_positions <- function(df, pos_vector, name_vector){

  logger::log_info("Check if excel df contains data name on certain position.")
  tf_values <- mapply(function(pos_vector, name_vector) {
    true_false <- name_vector %in% df[[1]][pos_vector]
    if(true_false == FALSE){return(FALSE)} else{
      return(TRUE)
    }
  }, pos_vector, name_vector)

  tf <- check_tf_list(tf_values)

  return(tf)
}

check_tf_list <- function(tf_values){
  if(all((tf_values)) == FALSE){
    logger::log_error("Sheet doesn't contain all values.")
    stop()
  } else{
    return(TRUE)
  }
}
