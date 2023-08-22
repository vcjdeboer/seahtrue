#' Example Dataset from run_seahtrue Function
#'
#' This dataset is an example output of the run_seahtrue function.
#'
#' @name run_seahtrue_example
#' @title Example of running the run_seahtrue function.
#' @description A final output datset for seahtrue created with run_seahtrue().
#'
#' @format A tibble with 1 row and 7 columns:
#'   \describe{
#'     \item{plate_id}          {Seahorse Plate ID}
#'     \item{filepath_seahorse} {Path to the Seahorse Wave excel file}
#'     \item{date}              {Date of the Seahorse run}
#'     \item{assay_info}        {Assay information (as a list)}
#'     \item{injection_info}    {Injection information (as a list)}
#'     \item{raw_data}          {Preprocessed raw data (as a list)}
#'     \item{rate_data}         {Preprocessed rate data (as a list)}
#'   }
#
#' @usage run_seahtrue(filepath_seahorse)
#' @param dataset A data frame containing Seahorse data.
#' @return A data frame with preprocessed Seahorse information.
#'
#' @examples
#' seahtrue_output_donor_A <- seahtrue::run_seahtrue(system.file("extdata", "20191219 SciRep PBMCs donor A.xlsx", package = "seahtrue"))
#' seahtrue_output_donor_B <- seahtrue::run_seahtrue(system.file("extdata", "20200110 SciRep PBMCs donor B.xlsx", package = "seahtrue"))
#' seahtrue_output_donor_C <- seahtrue::run_seahtrue(system.file("extdata", "20200110 SciRep PBMCs donor C.xlsx", package = "seahtrue"))
#' 
#' Seahtrue Output Dataset for Donor A
#'
#' This dataset contains the output of the Seahtrue analysis for donor A.
#'
#' @name seahtrue_output_donor_A
#' @title Seahtrue output for donor A.
#' @description The dataset represents the preprocessed Seahorse information for donor A.
#' ...
#'
#' @format A tibble with appropriate columns and descriptions.
#'
#' @usage data(seahtrue_output_donor_A)
#' 
#' Seahtrue Output Dataset for Donor B
#'
#' This dataset contains the output of the Seahtrue analysis for donor B.
#'
#' @name seahtrue_output_donor_B
#' @title Seahtrue output for donor B.
#' @description The dataset represents the preprocessed Seahorse information for donor B.
#' ...
#'
#' @format A tibble with appropriate columns and descriptions.
#'
#' @usage data(seahtrue_output_donor_B)
#' 
#' Seahtrue Output Dataset for Donor C
#'
#' This dataset contains the output of the Seahtrue analysis for donor C.
#'
#' @name seahtrue_output_donor_C
#' @title Seahtrue output for donor C.
#' @description The dataset represents the preprocessed Seahorse information for donor C.
#' ...
#'
#' @format A tibble with appropriate columns and descriptions.
#'
#' @usage data(seahtrue_output_donor_C)