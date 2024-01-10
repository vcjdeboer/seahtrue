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

seahorse_files <- list(testthat::test_path("20191219_SciRep_PBMCs_donor_A.xlsx"),
                       testthat::test_path("20200110_SciRep_PBMCs_donor_B.xlsx"),
                       testthat::test_path("20200110_SciRep_PBMCs_donor_C.xlsx"))


# testthat: Test 'Raw' data --------------------------------------------------------------
for (seahorse_file in seahorse_files){
  filepath_seahorse <- testthat::test_path(seahorse_file)
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
for (seahorse_file in seahorse_files){
  filepath_seahorse <- testthat::test_path(seahorse_file)
  test_that("Test normalization list with 'Normalization values' derived from the Assay configuration sheet.", {
    xf_norm <- expect_type(get_xf_norm(filepath_seahorse), "list")
    expect_type(xf_norm[[1]]$well, "character")
    expect_length(xf_norm[[1]]$well, 96)
    expect_length(xf_norm[[1]]$well, 96)
    expect_equal(ncol(xf_norm[[1]]), 2)
    expect_equal(nchar(xf_norm[[1]]$well[1]), 3)
    if (all(is.na(xf_norm[[1]]$cell_n)) == TRUE) {
      expect_type(xf_norm[[1]]$cell_n, "logical")
    } else {
      expect_type(xf_norm[[1]]$cell_n, "double") #actually integer
    }
  })
}

# testthat: Test unselected 'flagged wells' data Assay Configuration sheet. --------------------------------------------------------------
for (seahorse_file in seahorse_files){
  filepath_seahorse <- testthat::test_path(seahorse_file)
  test_that("Tests for unselected (flagged) wells derived from Assay configuration sheet.", {
    flagged <- get_xf_flagged(filepath_seahorse)
    expect_type(flagged, "list")
  })
}

# testthat: Test the original rate data. --------------------------------------------------------------
for (seahorse_file in seahorse_files){
  filepath_seahorse <- testthat::test_path(seahorse_file)
  test_that("Test original rate data table", {
    xf_rate_list <- get_xf_rate(filepath_seahorse)
    expect_equal(ncol(xf_rate_list[[1]]), 8)
    expect_type(xf_rate_list[[1]], "list")
    expect_type(xf_rate_list[[2]], "logical")
    expect_type(xf_rate_list[[1]]$measurement, "double") # actually integer
    expect_type(xf_rate_list[[1]]$well, "character")
    expect_type(xf_rate_list[[1]]$group, "character")
    expect_type(xf_rate_list[[1]]$time_wave, "double")
    expect_type(xf_rate_list[[1]]$OCR_wave, "double")
    expect_type(xf_rate_list[[1]]$OCR_wave_bc, "double")
    expect_type(xf_rate_list[[1]]$ECAR_wave, "double")
    expect_type(xf_rate_list[[1]]$ECAR_wave_bc, "double")

  })
}

# testthat: Test buffer factor (capacity) info. --------------------------------------------------------------
for (seahorse_file in seahorse_files){
  filepath_seahorse <- testthat::test_path(seahorse_file)
  test_that("Test buffer factor (capacity) info", {
    bufferfactor_info <- get_xf_buffer(filepath_seahorse)
    expect_type(bufferfactor_info, "list")
    expect_type(bufferfactor_info$well, "character")
    expect_type(bufferfactor_info$bufferfactor, "double")
    expect_equal(nchar(bufferfactor_info$well[1]), 3)
    })
  }

# testthat: Test the pH calibration emission data. --------------------------------------------------------------
for (seahorse_file in seahorse_files){
  filepath_seahorse <- testthat::test_path(seahorse_file)
  test_that("Test the pH calibration emission data.", {
  pH_calibration <- get_xf_pHcal(filepath_seahorse)
  expect_type(pH_calibration, "list")
  expect_equal(nchar(pH_calibration$well[1]), 3)
  expect_type(pH_calibration$pH_cal_em, "double") # actually integer
  })
}


# testthat: Get O2 calibration emission. --------------------------------------------------------------

for (seahorse_file in seahorse_files){
  filepath_seahorse <- testthat::test_path(seahorse_file)
  test_that("Test O2 calibration emission.", {
    O2_calibration <- get_xf_O2cal(filepath_seahorse)
    expect_type(O2_calibration, "list")
    expect_type(O2_calibration$well, "character")
    expect_type(O2_calibration$O2_cal_em, "double") # actually integer
    expect_equal(nchar(O2_calibration$well[1]), 3)
  })
}

# testthat: Test injection information. --------------------------------------------------------------
for (seahorse_file in seahorse_files){
  filepath_seahorse <- testthat::test_path(seahorse_file)
  test_that("Test injection information.", {
    measurement_info <- get_xf_inj(filepath_seahorse)
    expect_type(measurement_info$measurement, "integer")
    expect_type(measurement_info$interval, "double") #actually integer
    expect_type(measurement_info$injection, "character")
    expect_length(measurement_info, 3)
  })
}

# testthat: Test assay information. --------------------------------------------------------------
for (seahorse_file in seahorse_files){
  filepath_seahorse <- testthat::test_path(seahorse_file)
  test_that("Test assay information.", {
    tibbler <- get_xf_assayinfo(filepath_seahorse, instrument = "XFe96", norm_available = "TRUE", xls_ocr_backgroundcorrected = "TRUE")
    expect_length(tibbler, 25)
    expect_type(tibbler, "list")
    expect_type(tibbler$V_C, "double")
    expect_type(tibbler$Tau_AC, "double") # actually integer
    expect_type(tibbler$Tau_W, "double") # actually integer
    expect_type(tibbler$Tau_C, "double") # actually integer
    expect_type(tibbler$Tau_P, "double")
    expect_type(tibbler$KSV, "double")
    expect_type(tibbler$KSV_tempCorrection, "logical")
    expect_type(tibbler$KSV_original, "double")
    expect_type(tibbler$gain1, "double")
    expect_type(tibbler$gain2, "double")
    expect_type(tibbler$pH_0, "double")
    expect_type(tibbler$pH_targetEmission, "double") # actually integer
    expect_type(tibbler$O2_targetEmission, "double") # actually integer
    expect_type(tibbler$plate_id, "character")
    expect_type(tibbler$cartridge_barcode, "character")
    expect_type(tibbler$date_run, "character") # actually date-time
    expect_type(tibbler$assay_name, "character")
    expect_type(tibbler$instrument_serial, "character") # actually integer
    expect_type(tibbler$O2_0_mmHg, "double")
    expect_type(tibbler$assay_name, "character")
    expect_type(tibbler$norm_available, "character") #actually logical
    expect_type(tibbler$xls_ocr_backgroundcorrected, "character") #actually logical
  })
}

# testthat: Get plate layout data. --------------------------------------------------------------
for (seahorse_file in seahorse_files){
  filepath_seahorse <- testthat::test_path(seahorse_file)
  test_that("Get plate layout data.", {
    df <- get_platelayout_data(filepath_seahorse, "Assay Configuration", "B84:N92", "cell_n")
    expect_length(df, 2)
    expect_type(df, "list")
    expect_equal(nchar(df$well[1]), 3)
    expect_type(df$well, "character")
  })
}

# testthat: Test get the OCR from the excel file --------------------------------------------------------------
for (seahorse_file in seahorse_files){
  filepath_seahorse <- testthat::test_path(seahorse_file)
  test_that("Test get the OCR from the excel file.", {
    original_rate_df_list <- get_originalRateTable(filepath_seahorse)
    expect_length(original_rate_df_list, 2)
    expect_length(original_rate_df_list[[1]], 8)
    expect_type(original_rate_df_list[[1]]$measurement, "double") # actually integer
    expect_type(original_rate_df_list[[1]]$well, "character")
    expect_type(original_rate_df_list[[1]]$group, "character")
    expect_type(original_rate_df_list[[1]]$time_wave, "double")
    expect_type(original_rate_df_list[[1]]$OCR_wave, "double")
    expect_type(original_rate_df_list[[1]]$OCR_wave_bc, "double")
    expect_type(original_rate_df_list[[1]]$ECAR_wave, "double")
    expect_type(original_rate_df_list[[1]]$ECAR_wave_bc, "double")
  })
}

# testthat: Test read necessary Seahorse plate data from Seahorse Excel file. --------------------------------------------------------------
for (seahorse_file in seahorse_files){
  filepath_seahorse <- testthat::test_path(seahorse_file)
  test_that(" Test read necessary Seahorse plate data from Seahorse Excel file.", {
    xf <- read_xfplate(filepath_seahorse)
    expect_length(xf, 10)
    expect_type(xf, "list")
    check_column_names(xf, list("raw", "rate", "assayinfo", "inj", "pHcal",
                                 "O2cal", "norm", "buffer", "flagged", "filepath_seahorse"))
  })
}
