% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/master.R
\name{run_seahtrue}
\alias{run_seahtrue}
\title{Running the read, validate and preprocess}
\usage{
run_seahtrue(filepath_seahorse, ...)
}
\arguments{
\item{filepath_seahorse}{Absolute path to the Seahorse Excel file.}

\item{...}{Dots argument (…) allows the function to take an undefined number of arguments.}
}
\value{
A preprocessed seahorse dataset is returned as an output. This is a nested tibble with the following 7 columns: \cr
 * plate_id = Barcode from the 'Assay Configuration' sheet from the Seahorse Excel file. \cr
 * filepath_seahorse = Path to Seahorse Excel file. \cr
 * date = Date derived from 'Assay Configuration' sheet of the Seahorse Excel file. \cr
 * assay_info = Dataframe with information from the 'Assay Configuration' sheet from the Seahorse Excel file. \cr
 * injection_info = Dataframe with information derived from 'Operation log' sheet. \cr
 * raw_data = Preprocessed raw dataframe from Seahorse 'Raw' sheet. \cr
 * rate_data = Preprocessed rate data from 'Rate' sheet.
}
\description{
This function takes the Seahorse Wave excel file and computes it through
read, validate and preprocess
}
\examples{
run_seahtrue(system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue"))
run_seahtrue(system.file("extdata", "20200110_SciRep_PBMCs_donor_B.xlsx", package = "seahtrue"))
run_seahtrue(system.file("extdata", "20200110_SciRep_PBMCs_donor_C.xlsx", package = "seahtrue"))
\dontrun{
run_seahtrue(here::here("inst", "extdata", "20191219_SciRep_PBMCs_donor_A.xlsx"))
run_seahtrue(here::here("inst", "extdata", "20200110_SciRep_PBMCs_donor_B.xlsx"))
run_seahtrue(here::here("inst", "extdata", "20200110_SciRep_PBMCs_donor_C.xlsx"))
}

}
