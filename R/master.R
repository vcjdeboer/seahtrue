#' Running the read, validate and preprocess
#'
#' @description
#' This function takes the Seahorse Wave excel file and computes it through
#' read, validate and preprocess
#'
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#'
#' @return xf_plate_pr is returned. This is a nested tibble with the following 7 columns:
#'  * plate_id = Seahorse Plate ID (from assay configuration sheet).
#'  * filepath_seahorse = The original input file and its path on local drive.
#'  * date = Date of the Seahorse run (from assay configuration sheet).
#'  * assay_info = Parameters from Assay Configuration sheet.
#'  * injection_info = Info from Operation log sheet.
#'  * raw_data = Preprocessed raw data from Raw sheet.
#'  * rate_data = Preprocessed rate data from Rate sheet.
#'  One assay (one plate), contains all data in one row.
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


