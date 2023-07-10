#' Running the read, validate and preprocess
#'
#' @description
#' This function takes the Seahorse Wave excel file and computes it through
#' read, validate and preprocess
#'
#'
#' @param filepath_seahorse path to file and filename as input
#'
#' @return xf_plate_pr is returned this is a nested tibble with the following 7 columns:
#'  * plate_id = plate_id (from assay configuration sheet)
#'  * filepath_seahorse = the original input file and its path on local drive
#'  * date = date that seahorse was run (from assay configuration sheet)
#'  * assay_info = parameters from Assay Configuration sheet
#'  * injection_info = info from Operation log sheet
#'  * raw_data = preprocessed raw data from Raw sheet
#'  * rate_data = preprocessed rate data from Rate sheet
#'  One assay (one plate), contains all data in one row
#' @keywords internal
#' @export
#'
#' @examples
#' run_seahtrue(system.file("extdata", "20191219 SciRep PBMCs donor A.xlsx", package = "seahtrue"))
#' run_seahtrue(system.file("extdata", "20200110 SciRep PBMCs donor B.xlsx", package = "seahtrue"))
#' run_seahtrue(system.file("extdata", "20200110 SciRep PBMCs donor C.xlsx", package = "seahtrue"))
#'

run_seahtrue <- function(filepath_seahorse){
  filepath_seahorse %T>%
    validate_xf_input() %>%
    read_xfplate() %>%
    preprocess_xfplate() %T>%
    validate_preprocessed()
}


