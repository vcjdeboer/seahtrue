example_file <- function() {
  system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx",
              package = "seahtrue")
}
example_plate <- function() {
  f <- example_file()
  testthat::skip_if(f == "", "example extdata not installed")
  revive_xfplate(f)
}
