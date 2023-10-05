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

#'\dontrun{
#' run_seahtrue(here::here("inst", "extdata", "20191219 SciRep PBMCs donor A.xlsx"))
#' run_seahtrue(here::here("inst", "extdata", "20200110 SciRep PBMCs donor B.xlsx"))
#' run_seahtrue(here::here("inst", "extdata", "20200110 SciRep PBMCs donor C.xlsx"))
#'}

run_seahtrue <- function(filepath_seahorse, ...){
  
  out <- tryCatch(
    {
      
    if (length(list(...)) > 0) {
      stop("Only one argument (filepath_seahorse) is allowed.")
    }
      
    filepath_seahorse %T>%
      validate_xf_input() %>%
      read_xfplate() %>%
      preprocess_xfplate() %T>%
      validate_preprocessed()
      }, warning = function(war) {
        cat("WARNING :", conditionMessage(war), "\n")
        logger::log_warn(conditionMessage(war), "\n")
      },
    error = function(err) {
      logger::log_error(conditionMessage(err))
      logger::log_info(glue::glue("Quiting analysis with sheet: {filepath_seahorse}"))
    }
  ) 
}


