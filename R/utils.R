# This is a common place to define small utilities that are used inside multiple package functions.
# Since they serve as helpers to multiple functions, placing them in R/utils.R makes them easier to re-discover
# when you return to your package after a long break.

#' Check if excel df contains data name on certain position.
#'
#' @param df Dataframe
#' @param pos_vector Position of cell in dataframe
#' @param name_vector Name of cell data frame
#'
#' @return True when all names of dataframe exists at the corresponding postion.
#'
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

#' Helper - Check if excel df contains data name on certain position.
#'
#' @param tf_values Values are true when cell exists on corresponding position. Otherwise value is false.
#'
#' @return True if when all names of dataframe exists at the corresponding position.
check_tf_list <- function(tf_values){
  if(all((tf_values)) == FALSE){
    logger::log_error("Sheet doesn't contain all values.")
    stop()
  } else{
    return(TRUE)
  }
}
