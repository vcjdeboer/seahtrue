# Assertion seahorse xf_plate
# Use this function before reading the xf_plate.
val_xfplate <- function(filepath_seahorse){
  # check if arguments function are present.
  rlang::check_required(filepath_seahorse)
  # Check if file with specific file path exists.
  path_not_found_boolean <- path_not_found(filepath_seahorse)
  # Check if Excel sheet contains the required Seahorse sheets.
  check_sheets_boolean <- check_sheets(filepath_seahorse,
                                       list(
                                         "Assay Configuration",
                                         "Rate",
                                         "Raw",
                                         "Calibration",
                                         "Operation Log")
                                       )
  # If all Excel sheet validations return TRUE,
  # we suppose it's a Seahorse xf file and we can continue with reading the data.
  meets_criteria <- all(path_not_found_boolean, check_sheets_boolean)
  return(meets_criteria)
}


#' Check if file with specific file path exists.
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.
#'
#' @return TRUE or FALSE, based the existence of the filepath.
#'
#' @examples path_not_found(here::here("inst", "extdata", "20191219 SciRep PBMCs donor A.xlsx"))
path_not_found <- function(filepath_seahorse){
  rlang::try_fetch({
    if (file.exists(filepath_seahorse)) {
      logger::log_info(glue::glue("File exists:
                                  {filepath_seahorse}"))
      return(TRUE)
    } else {
      logger::log_info(glue::glue("File does not exist:
                       {filepath_seahorse}"))
      return(FALSE)
    }
  }, otherwise = function(e) {
    cli_alert_error("An error occurred:", conditionMessage(e))
  })
}

#' Check sheets
#'
#' @param filepath_excel Absolute path to the Seahorse Excel file.
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.
#' @param sheets_predicted
#'
#' @return TRUE or FALSE, based on existence of the required Seahorse sheets.
#' @examples check_sheets(here::here("inst", "extdata", "20191219 SciRep PBMCs donor A.xlsx"), list("Assay Configuration","Rate","Rw", "Calibration","Operation Log"))
check_sheets <- function(filepath_excel, sheets_predicted, call = rlang::caller_env()){
  rlang::try_fetch({

    logger::log_info(glue::glue("Check if Excel sheet contains the required Seahorse sheets:
                                {filepath_excel}"))

    excel_sheets <- readxl::excel_sheets(filepath_excel)

    for (value in sheets_predicted) {
      x <- sheets_predicted[[2]]
      stopifnot(value %in% excel_sheets)
    }

    logger::log_info(glue::glue("The excel sheet contains all required sheets."))
    return(TRUE)
  },
  error = function(cnd) {
    cli::cli_abort("Can't find sheet {x}.", parent = cnd, call = call)
    cli::cli_alert_info("Check your excel file.")
  }
  )
}
