#' Running the read, validate and preprocess
#'
#' @description
#' This function takes the Seahorse Wave excel file and computes it through
#' read, validate and preprocess
#'
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#'
#' @return A preprocessed seahorse dataset is returned as an output. This is a nested tibble with the following 7 columns: \cr
#'  * plate_id = Barcode from the 'Assay Configuration' sheet from the Seahorse Excel file. \cr
#'  * filepath_seahorse = Path to Seahorse Excel file. \cr
#'  * date = Date derived from 'Assay Configuration' sheet of the Seahorse Excel file. \cr
#'  * assay_info = Dataframe with information from the 'Assay Configuration' sheet from the Seahorse Excel file. \cr
#'  * injection_info = Dataframe with information derived from 'Operation log' sheet. \cr
#'  * raw_data = Preprocessed raw dataframe from Seahorse 'Raw' sheet. \cr
#'  * rate_data = Preprocessed rate data from 'Rate' sheet.
#' @keywords internal
#' @export
#'
#' @examples
#' run_seahtrue(system.file("extdata", "20191219 SciRep PBMCs donor A.xlsx", package = "seahtrue"))
#' run_seahtrue(system.file("extdata", "20200110 SciRep PBMCs donor B.xlsx", package = "seahtrue"))
#' run_seahtrue(system.file("extdata", "20200110 SciRep PBMCs donor C.xlsx", package = "seahtrue"))
#' \dontrun{
#' run_seahtrue(here::here("inst", "extdata", "20191219 SciRep PBMCs donor A.xlsx"))
#' run_seahtrue(here::here("inst", "extdata", "20200110 SciRep PBMCs donor B.xlsx"))
#' run_seahtrue(here::here("inst", "extdata", "20200110 SciRep PBMCs donor C.xlsx"))
#' }
#'
run_seahtrue <- function(filepath_seahorse, ...) {

  cli::cli_inform("Starting Seahtrue analysis")


  logger::log_info("Checking if file meets criteria for data reading.")

  # check if arguments function are present.
  rlang::check_required(filepath_seahorse)
  

  if (length(list(...)) > 0) {
    
    formalArgs(run_seahtrue)
    f <- formals(run_seahtrue)
    
    cli::cli_alert_info("You provided multiple arguments.")
    cli::cli_alert_danger(glue::glue("Only {length(formals(run_seahtrue)) -1} argument(s) allowed."))
    cli::cli_abort(glue::glue("Only {length(formals(run_seahtrue)) -1} argument(s) allowed."))
  }
  

  if (is.na(filepath_seahorse) || filepath_seahorse == "") {
    cli::cli_alert_info("The path you provided either doesn't exist or is empty.")
    cli::cli_alert_danger("Couldn't find path to Excel file.")
    cli::cli_abort("An error occured because an unexpected path is provided. Stopping analyis.")
  }

  if (missing(filepath_seahorse) || !is.character(filepath_seahorse)) {
    cli::cli_alert_info(glue::glue("{f[1]} must be provided as a character vector"))
    cli::cli_abort("'filepath_seahorse' must be provided as a character vector")
  }

  if (missing(filepath_seahorse) || !isSingleString(filepath_seahorse)) {
    cli::cli_alert_info(glue::glue("{f[1]} must be provided as a character vector"))
    cli::cli_abort("filepath_seahorse must be a single, non-NA string")
  }

  # Check if file with specific file path exists.
  path_not_found_boolean <- path_not_found(filepath_seahorse)
  # Check if Excel sheet contains the required Seahorse sheets.


  # Check seahorse Excel sheets
  check_sheets_boolean <- check_sheets(
    filepath_seahorse,
    list(
      "Assay Configuration",
      "Rate",
      "Raw",
      "Calibration",
      "Operation Log"
    )
  )

  logger::log_info("Finished seahorse input file validation.")
  logger::log_info("Reading seahorse input with input file.")

  logger::log_info(glue::glue("Start function to read seahorse plate data from Excel file:
                                {filepath_seahorse}"))

  # read data
  xf_raw <- get_xf_raw(filepath_seahorse)
  xf_rate <- get_xf_rate(filepath_seahorse) # outputs list of 2
  xf_norm <- get_xf_norm(filepath_seahorse) # outputs list of 2
  xf_buffer <- get_xf_buffer(filepath_seahorse)
  xf_inj <- get_xf_inj(filepath_seahorse)
  xf_pHcal <- get_xf_pHcal(filepath_seahorse)
  xf_O2cal <- get_xf_O2cal(filepath_seahorse)
  xf_flagged <- get_xf_flagged(filepath_seahorse)
  xf_assayinfo <- get_xf_assayinfo(filepath_seahorse,
    norm_available = xf_norm[[2]],
    xls_ocr_backgroundcorrected = xf_rate[[2]]
  )
  xf_norm <- xf_norm[[1]]
  xf_rate <- xf_rate[[1]]


  # make the output list
  xf <- list(
    raw = xf_raw,
    rate = xf_rate,
    assayinfo = xf_assayinfo,
    inj = xf_inj,
    pHcal = xf_pHcal,
    O2cal = xf_O2cal,
    norm = xf_norm,
    flagged = xf_flagged,
    buffer = xf_buffer,
    filepath_seahorse = filepath_seahorse
  )
  

  if (is.null(xf) == FALSE) {
    # Proceed with further checks and analysis
  } else {
    cli::cli_abort("The dataset is empty.")
  }
  
  if (!(any(is.na(xf)))) {
    # Proceed if there are no missing values
  } else {
    cli::cli_abort("The datset contains missing values.")
  }
  
  # Proceed to preprocessing and validation.
  preprocessed_file <- xf %>%
    preprocess_xfplate() %T>%
    validate_preprocessed()

  logger::log_info(glue::glue("Parsing all collected seahorse information from file: {filepath_seahorse}"))

  return(preprocessed_file)
}


isSingleString <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}
