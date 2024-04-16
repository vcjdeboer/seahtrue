#' Running the read, preprocess and validate
#'
#' @description
#' This function takes the Seahorse Wave excel file and computes it through
#' read, validate and preprocess
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#'
#' @return A preprocessed seahorse dataset is returned as an output. This is a nested tibble with the following 7 columns: \cr
#'  * plate_id = Barcode plate id of the well plate containing the samples \cr
#'  * filepath_seahorse = Path, and basename to .xlsx input file \cr
#'  * date_run = Date and time when the plate was run \cr
#'  * date_processed = Date and time this output from revive_xfplate() was generated \cr
#'  * assay_info = Meta information from 'Assay Configuration' sheet and 'Calibration' sheet \cr
#'  * injection_info = Dataframe with information from the 'Operation log' sheet \cr
#'  * raw_data = Preprocessed raw dataframe from 'Raw' sheet \cr
#'  * rate_data = Preprocessed rate data from 'Rate' sheet \cr
#'  # validation_output = Output of the data checks. including rules
#'  
#' @export
#' @import logger rlang cli glue readxl 
#' @examples
#' revive_xfplate(
#'   system.file("extdata", 
#'   "20191219_SciRep_PBMCs_donor_A.xlsx", 
#'   package = "seahtrue"))
#'
revive_xfplate <- function(filepath_seahorse) {

  logger::log_info("Start reviving")
  
  # Check if argument is present
  rlang::check_required(filepath_seahorse)

  # Check if file exists.
  if (!file.exists(filepath_seahorse)) {
    cli::cli_abort(
      glue::glue("The following file 
                  does not exist: {basename(filepath_seahorse)}"),
      wrap = TRUE)}
  
  # Check if required sheets are available in input file
  sheets_required <- c("Assay Configuration",
                       "Rate",
                       "Raw",
                       "Calibration",
                       "Operation Log")
  my_excel_sheets <- 
    readxl::excel_sheets(filepath_seahorse) 
  
  my_missing_sheets <- missing_strings(my_excel_sheets, 
                                       sheets_required)
  
  if (!is.null(my_missing_sheets)){
    if (length(my_missing_sheets)>1){
    cli::cli_abort(
      glue::glue("The following sheets 
                  do not exist: {my_missing_sheets}"),
      wrap = TRUE)
      } else {
        cli::cli_abort(
          glue::glue("The following sheet 
                      does not exist: {my_missing_sheets}"),
          wrap = TRUE)
    }
  }
  
  # run the read/preprocess/validate function
  preprocessed_xf_plate <- filepath_seahorse %>%
    read_xfplate() %>%
    preprocess_xfplate() %>% 
    validate_preprocessed()
  
  logger::log_info("Finished reviving")
  
  
  return(preprocessed_xf_plate)
}

