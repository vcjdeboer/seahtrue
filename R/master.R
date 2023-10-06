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
#'
#'\dontrun{
#'run_seahtrue(here::here("inst", "extdata", "20191219 SciRep PBMCs donor A.xlsx"))
#' run_seahtrue(here::here("inst", "extdata", "20200110 SciRep PBMCs donor B.xlsx"))
#' run_seahtrue(here::here("inst", "extdata", "20200110 SciRep PBMCs donor C.xlsx"))
#'}

run_seahtrue <- function(filepath_seahorse, ...){
  
  formalArgs(run_seahtrue)
  f <- formals(run_seahtrue)
  
  if (length(list(...)) > 0) {
    cli::cli_alert_info("You provided multiple arguments")
    cli::cli_alert_danger("Only one argument is allowed.")
    cli::cli_abort(glue::glue("Only {length(formals(run_seahtrue)) -1} argument is allowed."))
  }
  
  if (is.na(filepath_seahorse) || filepath_seahorse == ''){
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
  
  filepath_seahorse %T>%
    validate_xf_input() %>%
    read_xfplate() %>%
    preprocess_xfplate() %T>%
    validate_preprocessed()
}

isSingleString <- function (x) 
{
  is.character(x) && length(x) == 1L && !is.na(x)
}

