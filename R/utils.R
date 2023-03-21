# This is a common place to define small utilities that are used inside multiple package functions.
# Since they serve as helpers to multiple functions, placing them in R/utils.R makes them easier to re-discover
# when you return to your package after a long break.

check_range <- function(data, column_name, range) {
  # Check if dataset has required columns
  if (!all(c("well", column_name) %in% names(data))) {
    stop(paste("Input dataset must have 'well' and", column_name, "columns"))
  }

  # Check if values are within range
  out_of_range <- data[[column_name]] < range[1] | data[[column_name]] > range[2]

  # If any values are out of range, give warning message with cli package
  if (any(out_of_range)) {
    map2(data$well[out_of_range], data[[column_name]][out_of_range], ~ {
      well_name <- .x
      value <- .y
      message <- glue::glue("{column_name} value {value} for well {well_name} is out of range ({range[1]} - {range[2]})")
      cli::cli_alert_warning(message)
    })
  }

  # Return dataset
  return(data)
}
