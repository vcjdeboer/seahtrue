% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{revive_output_donor_A}
\alias{revive_output_donor_A}
\title{Dataset PBMC donor A}
\format{
A data frame with 1 row and 7 variables.
\describe{
  \item{plate_id}{Barcode plate id of the well plate containing the samples}
  \item{filepath_seahorse}{Path, and basename to .xlsx file}
  \item{date_run}{Date and time when the plate was run}
  \item{date_processed}{Date and time this output from revive_xfplate() was generated}
  \item{assay_info}{Meta information from 'Assay Configuration' sheet and 'Calibration' sheet}
  \item{injection_info}{Dataframe with information from the 'Operation log' sheet}
  \item{raw_data}{Preprocessed raw dataframe from 'Raw' sheet}
  \item{rate_data}{Preprocessed rate data from 'Rate' sheet}
  \item{validation_output}{A list of all validation information, output, and rules that are used}
  
}
}
\source{
Janssen et al. 2021 Sci Rep 11:1162 <https://doi.org/10.1038/s41598-021-81217-4>
}
\usage{
data(revive_output_donor_A)
}
\description{
A dataset containing output for the "20191219_SciRep_PBMCs_donor_A.xlsx" file. This experiment was
part of the study that was published in Janssen et al. (2021) Sci rep 11:1662. This is data from PBMCs
that were isolated from buffy coats as described in the material and methods section of 
Janssen et al. (2021). The .xlsx file is the result of a single XF experiment on one plate.
This .xlsx file was used as input for the revive_xfplate() function. The output of that function 
contains the read, preprocessed, and validated data in the form a a nested tibble.
The file '20191219_SciRep_PBMCs_donor_A.xlsx' can be found in the inst/extdata directory.
}
\keyword{datasets}
