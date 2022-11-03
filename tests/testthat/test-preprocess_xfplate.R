local({

  # Check if columns in df --------------------------------------------------------------
  check_column_names <- function(df, df_columns_list){
    for(columname in df_columns_list){
      expect_true(columname %in% names(df))
    }
  }

  xf_plate_pr = NULL
  xf_raw_pr = NULL
  OCR_from_excel = NULL

  test_that("type list", {
    xf_plate_pr <<- expect_type(preprocess_xfplate(xf), "list")
    expect_type(xf_plate_pr[4]$assay_info, "list")
    expect_type(xf_plate_pr[5]$injection_info, "list")
    expect_type(xf_plate_pr[6]$raw_data, "list")
    expect_type(xf_plate_pr[7]$rate_data, "list")
    xf_raw_pr <<- expect_type(preprocess_xf_raw(xf$raw, xf$pHcal, xf$inj, xf$assayinfo, xf$buffer, xf$norm, xf$flagged), "list")
    OCR_from_excel <<- expect_type(preprocess_xf_rate(xf$rate, xf$norm, xf$flagged), "list")
  })

  test_that("type character", {
    expect_type(xf_plate_pr[1]$plate_id, "character")
    expect_type(xf_plate_pr$assay_info[[1]][15]$plate_id, "character")
    expect_type(xf_plate_pr[2]$filepath_seahorse, "character")
    expect_type(xf_raw_pr[1]$plate_id, "character")
    expect_type(xf_raw_pr[2]$well, "character")
    expect_type(xf_raw_pr[7]$group, "character")
    expect_type(xf_raw_pr[9]$injection, "character")
    expect_type(xf_plate_pr$assay_info[[1]]$plate_id, "character")
    expect_type(xf_plate_pr$assay_info[[1]][16]$cartridge_barcode, "character")
    expect_type(xf_plate_pr$assay_info[[1]][18]$assay_name, "character")
    expect_type(xf_plate_pr$assay_info[[1]][19]$instrument_serial, "character")
    expect_type(xf_plate_pr$injection_info[[1]][3]$injection, "character")
    expect_type(xf_plate_pr$raw_data[[1]][1]$well, "character")
    expect_type(xf_plate_pr$raw_data[[1]][6]$group, "character")
    expect_type(xf_plate_pr$raw_data[[1]][8]$injection, "character")
    expect_type(xf_plate_pr$rate_data[[1]][2]$well, "character")
    expect_type(xf_plate_pr$rate_data[[1]][3]$group, "character")
  })

  test_that("type double", {
    expect_type(xf_plate_pr[3]$date, "double")
    expect_type(xf_raw_pr[5]$timescale, "double")
    expect_type(xf_raw_pr[6]$minutes, "double")
    expect_type(xf_raw_pr[8]$interval, "double")
    expect_type(xf_raw_pr[10]$O2_em_corr, "double")
    expect_type(xf_raw_pr[11]$pH_em_corr, "double")
    expect_type(xf_raw_pr[12]$O2_mmHg, "double")
    expect_type(xf_raw_pr[13]$pH, "double")
    expect_type(xf_raw_pr[14]$pH_em_corr_corr, "double")
    expect_type(xf_raw_pr[15]$O2_em_corr_bkg, "double")
    expect_type(xf_raw_pr[16]$pH_em_corr_bkg, "double")
    expect_type(xf_raw_pr[17]$O2_mmHg_bkg, "double")
    expect_type(xf_raw_pr[18]$pH_bkgd, "double")
    expect_type(xf_raw_pr[19]$pH_em_corr_corr_bkg, "double")
    expect_type(xf_raw_pr[20]$bufferfactor, "double")
    expect_type(xf_plate_pr$assay_info[[1]][1]$F0, "double")
    expect_type(xf_plate_pr$assay_info[[1]][2]$V_C, "double")
    expect_type(xf_plate_pr$assay_info[[1]][3]$Tau_AC, "double")
    expect_type(xf_plate_pr$assay_info[[1]][4]$Tau_W, "double")
    expect_type(xf_plate_pr$assay_info[[1]][5]$Tau_C, "double")
    expect_type(xf_plate_pr$assay_info[[1]][6]$Tau_P, "double")
    expect_type(xf_plate_pr$assay_info[[1]][7]$KSV, "double")
    expect_type(xf_plate_pr$assay_info[[1]][9]$KSV_original, "double")
    expect_type(xf_plate_pr$assay_info[[1]][10]$gain1, "double")
    expect_type(xf_plate_pr$assay_info[[1]][11]$gain2, "double")
    expect_type(xf_plate_pr$assay_info[[1]][12]$pH_0, "double")
    expect_type(xf_plate_pr$assay_info[[1]][13]$pH_targetEmission, "double")
    expect_type(xf_plate_pr$assay_info[[1]][14]$O2_targetEmission, "double")
    expect_type(xf_plate_pr$assay_info[[1]][20]$O2_0_mmHg, "double")
    expect_type(xf_plate_pr$assay_info[[1]][21]$O2_0_mM, "double")
    expect_type(xf_plate_pr$injection_info[[1]][2]$interval, "double")
    expect_type(xf_plate_pr$raw_data[[1]][4]$timescale, "double")
    expect_type(xf_plate_pr$raw_data[[1]][5]$minutes, "double")
    expect_type(xf_plate_pr$raw_data[[1]][7]$interval, "double")
    expect_type(xf_plate_pr$raw_data[[1]][9]$O2_em_corr, "double")
    expect_type(xf_plate_pr$raw_data[[1]][10]$pH_em_corr, "double")
    expect_type(xf_plate_pr$raw_data[[1]][11]$O2_mmHg, "double")
    expect_type(xf_plate_pr$raw_data[[1]][13]$pH_em_corr_corr, "double")
    expect_type(xf_plate_pr$raw_data[[1]][14]$O2_em_corr_bkg, "double")
    expect_type(xf_plate_pr$raw_data[[1]][15]$pH_em_corr_bkg, "double")
    expect_type(xf_plate_pr$raw_data[[1]][16]$O2_mmHg_bkg, "double")
    expect_type(xf_plate_pr$raw_data[[1]][17]$pH_bkgd, "double")
    expect_type(xf_plate_pr$raw_data[[1]][18]$pH_em_corr_corr_bkg, "double")
    expect_type(xf_plate_pr$raw_data[[1]][19]$bufferfactor, "double")
    expect_type(xf_plate_pr$rate_data[[1]]$measurement, "double")
    expect_type(xf_plate_pr$rate_data[[1]][4]$time_wave, "double")
    expect_type(xf_plate_pr$rate_data[[1]][5]$OCR_wave, "double")
    expect_type(xf_plate_pr$rate_data[[1]][6]$OCR_wave_bc, "double")
    expect_type(xf_plate_pr$rate_data[[1]][7]$ECAR_wave, "double")
    expect_type(xf_plate_pr$rate_data[[1]][8]$ECAR_wave_bc, "double")
  })

  test_that("type integer", {
    expect_type(xf_raw_pr[3]$measurement, "integer")
    expect_type(xf_raw_pr[4]$tick, "integer")
    expect_type(xf_plate_pr$injection_info[[1]][1]$measurement, "integer")
    expect_type(xf_plate_pr$raw_data[[1]][2]$measurement, "integer")
    expect_type(xf_plate_pr$raw_data[[1]][3]$tick, "integer")
    expect_type(xf_plate_pr$rate_data[[1]][1]$measurement, "integer")
  })

  test_that("type logical", {
    expect_type(xf_raw_pr[22]$flagged_well, "logical")
    expect_type(xf_plate_pr$assay_info[[1]][8]$KSV_tempCorrection, "logical")
    expect_type(xf_plate_pr$assay_info[[1]][22]$norm_available, "logical")
    expect_type(xf_plate_pr$raw_data[[1]][20]$cell_n, "logical")
    expect_type(xf_plate_pr$rate_data[[1]][9]$cell_n, "logical")
    expect_type(xf_plate_pr$rate_data[[1]][10]$flagged_well, "logical")
  })

  test_that("check column names", {
    check_column_names(xf_plate_pr, list("plate_id", "filepath_seahorse", "date", "assay_info", "injection_info", "raw_data", "rate_data"))
    check_column_names(xf_raw_pr, list("plate_id" , "well" , "measurement", "tick", "timescale", "minutes", "group", "injection", "O2_em_corr",
                                       "pH_em_corr", "O2_mmHg", "pH", "pH_em_corr_corr", "O2_em_corr_bkg", "pH_em_corr_bkg", "O2_mmHg_bkg",
                                       "pH_bkgd", "pH_em_corr_corr_bkg", "cell_n", "flagged_well"))
  })

  test_that("type date", {
    expect_type(xf_plate_pr$date, "date-time") # datetime must be double?
    expect_type(xf_plate_pr$assay_info[[1]][17]$date_run, "date-time")
  })

test_that("type logical or double", {
    if (all(is.na(xf_plate_pr$raw_data[[1]][20]$cell_n)) == TRUE) {
      expect_type(xf_plate_pr$raw_data[[1]][20]$cell_n, "logical")
    } else {
      expect_type(xf_plate_pr$raw_data[[1]][20]$cell_n, "double") #actually integer
    }

    if (all(is.na(xf_raw_pr[21]$cell_n)) == TRUE) {
      expect_type(xf_raw_pr[21]$cell_n, "logical")
    } else {
      expect_type(xf_raw_pr[21]$cell_n, "double") #actually integer
    }

  })

})
