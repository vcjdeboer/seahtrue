# set working_directory  --------------------------------------------------------------

# columnnames 'Raw' --------------------------------------------------------------
# Columnames of 'Raw' Seahorse Excel datasheet.
xf_raw_columns_list <<- list("Measurement",
                               "Tick",
                               "Well",
                               "Group",
                               "TimeStamp",
                               "Well Temperature",
                               "Env. Temperature",
                               "O2 is Valid",
                               "O2 (mmHg)",
                               "O2 Light Emission",
                               "O2 Dark Emission",
                               "O2 Ref Light",
                               "O2 Ref Dark",
                               "O2 Corrected Em.",
                               "pH Is Valid",
                               "pH",
                               "pH Light",
                               "pH Dark",
                               "pH Ref Light",
                               "pH Ref Dark",
                               "pH Corrected Em.")

# Check if columns in df --------------------------------------------------------------
check_column_names <- function(df, df_columns_list){
  for(columname in df_columns_list){
    expect_true(columname %in% names(df))
  }
}

# Test files --------------------------------------------------------------
filepath <- list("20191219 SciRep PBMCs donor A.xlsx",
                 "20200110 SciRep PBMCs donor B.xlsx",
                 "20200110 SciRep PBMCs donor C.xlsx")

# testthat: Test 'Raw' data --------------------------------------------------------------
for (seahorse_file in filepath){
  filepath_seahorse <- here::here("inst", "extdata", seahorse_file)
  test_that("Test 'Raw' data derived from Seahorse XFe96 analyser 'Raw' Excel datasheet.", {
    # check return
    xf_raw <- expect_type(get_xf_raw(filepath_seahorse), "list")
    # perform checks on the returned tibble (list)
    check_column_names(xf_raw, xf_raw_columns_list)
    expect_length(xf_raw, 21)
    expect_true(any(xf_raw$Measurement > 0))
    expect_type(xf_raw$Measurement, "double") # Note: this is actually an integer and will be converted during preprocessing.
    expect_type(xf_raw$Tick, "double") # Note: this is actually an integer and will be converted during preprocessing.
    expect_type(xf_raw$Well, "character")
    expect_type(xf_raw$Group, "character")
    expect_type(xf_raw$TimeStamp, "character") # Note: this is actually a datetime.
    expect_type(xf_raw$`Well Temperature`, "double")
    expect_type(xf_raw$`O2 is Valid`, "character")
    expect_type(xf_raw$`O2 (mmHg)`, "double")
    expect_type(xf_raw$`O2 Light Emission`, "double") # Note: this is actually an integer and will be converted during preprocessing.
    expect_type(xf_raw$`O2 Dark Emission`, "double") # Note: this is actually an integer and will be converted during preprocessing.
    expect_type(xf_raw$`O2 Ref Light`, "double") # Note: this is actually an integer and will be converted during preprocessing.
    expect_type(xf_raw$`O2 Ref Dark`, "double") # Note: this is actually an integer and will be converted during preprocessing.
    expect_type(xf_raw$`O2 Corrected Em.`, "double")
    expect_type(xf_raw$`pH Is Valid`, "character")
    expect_type(xf_raw$pH, "double")
    expect_type(xf_raw$`pH Light`, "double") # Note: this is actually an integer and will be converted during preprocessing.
    expect_type(xf_raw$`pH Dark`, "double") # Note: this is actually an integer and will be converted during preprocessing.
    expect_type(xf_raw$`pH Ref Light`, "double") # Note: this is actually an integer and will be converted during preprocessing.
    expect_type(xf_raw$`pH Ref Dark`, "double") # Note: this is actually an integer and will be converted during preprocessing.
    expect_type(xf_raw$`pH Corrected Em.`, "double")
  })
}

# testthat: Test 'Normalization' data --------------------------------------------------------------
for (seahorse_file in filepath){
  test_that("Test normalization list with 'Normalization values' derived from the Assay configuration sheet.", {
    filepath_seahorse <- here::here("inst", "extdata", seahorse_file)
    xf_norm <- expect_type(get_xf_norm(filepath_seahorse), "list")
    expect_type(xf_norm$well, "character")
    expect_length(xf_norm$well, 96)
    expect_length(xf_norm$well, 96)
    expect_equal(ncol(xf_norm), 2)
    expect_equal(nchar(xf_norm$well[1]), 3)
    if (all(is.na(xf_norm$cell_n)) == TRUE) {
      expect_type(xf_norm$cell_n, "logical")
    } else {
      expect_type(xf_norm$cell_n, "double")
    }
  })
}

# testthat: Test xfplate data --------------------------------------------------------------
# # Check if a data tibble is returned, and check data tibble. (Note: tibble has type list)
# test_that("a data tibble is returned (list format)", {
#   xf <- expect_type(read_xfplate("/cloud/project/data-raw/seahorse_raw_test_data.xlsx"), "list")
# })
