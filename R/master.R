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

  # Check if the seahorse file path argument is present.
  # If not, stop the analysis.
  rlang::check_required(filepath_seahorse)

  # Check if the function contains the right amount of arguments.
  # If not, stop the analysis.
  if (length(list(...)) > 0) {
    cli::cli_alert_info("You provided multiple arguments.")
    cli::cli_alert_danger(glue::glue("Only {length(formals(run_seahtrue)) -1} argument(s) allowed."))
    cli::cli_abort(glue::glue("Only {length(formals(run_seahtrue)) -1} argument(s) allowed."))
  }

  # Check if the provided seahorse file path contains missing values or is an empty string.
  # If not, stop the analysis.
  if (is.na(filepath_seahorse) || filepath_seahorse == "") {
    cli::cli_alert_info("The path you provided either doesn't exist or is empty.")
    cli::cli_alert_danger("Couldn't find path to Excel file.")
    cli::cli_abort("An error occured because an unexpected path is provided. Stopping analyis.")
  }

  # Check if provided seahorse file path has type character.
  # If not, stop the analysis.
  if (!is.character(filepath_seahorse)) {
    cli::cli_alert_info(glue::glue("{formalArgs(run_seahtrue)[1]} must be provided as a character vector."))
    cli::cli_abort("'filepath_seahorse' must be provided as a character vector")
  }

  # Check if file with specific file path exists.
  # If not, stop the analysis.
  path_not_found_boolean <- path_not_found(filepath_seahorse)
  
  # Check if Excel file on file path location contains the required Seahorse sheets.
  # Note: If these seahorse sheets exist we assume a seahorse file has been provided.
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

  # When we have a correct Seahorse Excel file, we can proceed to our data retrieval and preprocessing.
  # Load the Seahorse Excel file, collect data and preprocess it. 
  # Put the preprocessed data in a variable.
 
  # When we have a correct Seahorse Excel file, we can proceed to our data retrieval and preprocessing.
  # Load the Seahorse Excel file, collect data and preprocess it.
  preprocessed_xf_plate <- read_xfplate(filepath_seahorse) %>%
    {
      if (is.null(.)) {
        cli::cli_abort("The dataset is empty.")
      } else if (any(is.na(.))) {
        cli::cli_abort("The dataset contains missing values.")
      } else {
        preprocess_xfplate(.)
      }
    }

  # Validate the preprocessed dataset.
  # We want to extract the failed validation rules to memorize these. 
  failed_validation_rules_tibble <- validate_preprocessed(preprocessed_xf_plate)
  
  # Add the entire validation tibble as a single column to preprocessed_xf_plate
  preprocessed_xf_plate <- preprocessed_xf_plate %>%
    mutate(validation_column = list(failed_validation_rules_tibble))

  return(preprocessed_xf_plate)
}

