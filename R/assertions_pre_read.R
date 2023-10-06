# Assertion seahorse xf_plate
# Use this function before reading the xf_plate.

#' This is a wrapper function around path_not_found and check_sheets
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.
#'
#' @return TRUE or FALSE, based the existence of the filepath and sheets in excel file
#' @noRd
#' @keywords internal
#' @examples validate_xf_input(system.file("extdata", "20191219 SciRep PBMCs donor A.xlsx", package = "seahtrue"))

validate_xf_input <- function(filepath_seahorse){
  cli::cli_alert_info("Start seahorse input validation.")
  logger::log_info(glue::glue("Start seahorse input validation."))
  
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
  
  logger::log_info(glue::glue("Finished seahorse input validation."))
  
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
#' @noRd
#' @keywords internal
#'
#' @examples path_not_found(system.file("extdata", "20191219 SciRep PBMCs donor A.xlsx", package = "seahtrue"))
#'
path_not_found <- function(filepath_seahorse){

    if (file.exists(filepath_seahorse)) {
      logger::log_info(glue::glue("The following file exists:
                                  {filepath_seahorse}"))
      return(TRUE)
    } else {
      logger::log_info(glue::glue("The following file does not exist: {filepath_seahorse}"))
      stop()
    }
}

#' Check sheets
#'
#' @param filepath_excel Absolute path to the Seahorse Excel file.
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.
#' @param sheets_predicted Sheets that the input seahorse excel file should contain.
#'
#' @noRd
#' @keywords internal
#'
#' @return TRUE or FALSE, based on existence of the required Seahorse sheets.
#' @examples check_sheets(system.file("extdata", "20200110 SciRep PBMCs donor B.xlsx", package = "seahtrue"), list("Assay Configuration","Rate","Raw", "Calibration","Operation Log"))
check_sheets <- function(filepath_excel, sheets_predicted = list("Assay Configuration","Rate","Raw", "Calibration","Operation Log"), call = rlang::caller_env){
    formalArgs(check_sheets)
    arguments <- formals(check_sheets)
    
    logger::log_info(glue::glue("Check if Excel input contains the required Seahorse sheets"))
    
    cli::cli_alert_info(glue::glue("Performing a check on the following seahorse sheets: 
                                   {arguments[2]}"))

    excel_sheets <- readxl::excel_sheets(filepath_excel)
    
    sheet_not_found_list <- c()
    
    # Iterate through each value in sheets_predicted.
    for (value in sheets_predicted) {
      if (!(value %in% excel_sheets)) {
        cli::cli_alert_danger(value)
        sheet_not_found_list <- append(sheet_not_found_list, value)  # Append the value to the list.
      }
    }
  
    if (length(sheet_not_found_list) > 0) {
      result <- FALSE
      sheet_not_found_list_ch <- as.character(sheet_not_found_list)
      cli::cli_abort(glue::glue("Stopping analysis, because sheets were not found."))
    } else {
      result <- TRUE
      logger::log_info(glue::glue("The excel sheet contains all required sheets."))
    }

    return(result)
  }

