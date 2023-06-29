# # Test files --------------------------------------------------------------

xf_donor_A <- here::here("data", "xf_donor_A.rda")
xf_donor_B <- here::here("data", "xf_donor_B.rda")
xf_donor_C <- here::here("data", "xf_donor_C.rda")

load(xf_donor_A)
load(xf_donor_B)
load(xf_donor_C)

filepath <- list(xf_donor_A,
                 xf_donor_B,
                 xf_donor_C)

for (rda_file in filepath){

  local({

    xf <- rda_file

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
      # <<- to change the above variables (xf_plate_pr, xf_raw_pr, OCR_from_excel) outside of test_that.
      xf_plate_pr <<- expect_type(preprocess_xfplate(xf), "list")
      expect_type(xf_plate_pr$assay_info, "list")
      expect_type(xf_plate_pr$injection_info, "list")
      expect_type(xf_plate_pr$raw_data, "list")
      expect_type(xf_plate_pr$rate_data, "list")
      xf_raw_pr <<- expect_type(preprocess_xf_raw(xf$raw, xf$pHcal, xf$inj, xf$assayinfo, xf$buffer, xf$norm, xf$flagged), "list")
      OCR_from_excel <<- expect_type(preprocess_xf_rate(xf$rate, xf$norm, xf$flagged), "list")
    })

    test_that("type character", {
      expect_type(xf_plate_pr$plate_id, "character")
      expect_type(xf_plate_pr$assay_info[[1]]$plate_id, "character")
      expect_type(xf_plate_pr$filepath_seahorse, "character")
      expect_type(xf_raw_pr$well, "character")
      expect_type(xf_raw_pr$group, "character")
      expect_type(xf_raw_pr$injection, "character")
      expect_type(xf_plate_pr$assay_info[[1]]$plate_id, "character")
      expect_type(xf_plate_pr$assay_info[[1]]$cartridge_barcode, "character")
      expect_type(xf_plate_pr$assay_info[[1]]$assay_name, "character")
      expect_type(xf_plate_pr$assay_info[[1]]$instrument_serial, "character")
      expect_type(xf_plate_pr$injection_info[[1]]$injection, "character")
      expect_type(xf_plate_pr$raw_data[[1]]$well, "character")
      expect_type(xf_plate_pr$raw_data[[1]]$group, "character")
      expect_type(xf_plate_pr$raw_data[[1]]$injection, "character")
      expect_type(xf_plate_pr$rate_data[[1]]$well, "character")
      expect_type(xf_plate_pr$rate_data[[1]]$group, "character")
    })

    test_that("type double", {
      expect_type(xf_plate_pr$date, "double")
      expect_type(xf_raw_pr$timescale, "double")
      expect_type(xf_raw_pr$minutes, "double")
      expect_type(xf_raw_pr$interval, "double")
      expect_type(xf_raw_pr$O2_em_corr, "double")
      expect_type(xf_raw_pr$pH_em_corr, "double")
      expect_type(xf_raw_pr$O2_mmHg, "double")
      expect_type(xf_raw_pr$pH, "double")
      expect_type(xf_raw_pr$pH_em_corr_corr, "double")
      expect_type(xf_raw_pr$O2_em_corr_bkg, "double")
      expect_type(xf_raw_pr$pH_em_corr_bkg, "double")
      expect_type(xf_raw_pr$O2_mmHg_bkg, "double")
      expect_type(xf_raw_pr$pH_bkgd, "double")
      expect_type(xf_raw_pr$pH_em_corr_corr_bkg, "double")
      expect_type(xf_raw_pr$bufferfactor, "double")
      expect_type(xf_plate_pr$assay_info[[1]]$F0, "double")
      expect_type(xf_plate_pr$assay_info[[1]]$V_C, "double")
      expect_type(xf_plate_pr$assay_info[[1]]$Tau_AC, "double")
      expect_type(xf_plate_pr$assay_info[[1]]$Tau_W, "double")
      expect_type(xf_plate_pr$assay_info[[1]]$Tau_C, "double")
      expect_type(xf_plate_pr$assay_info[[1]]$Tau_P, "double")
      expect_type(xf_plate_pr$assay_info[[1]]$KSV, "double")
      expect_type(xf_plate_pr$assay_info[[1]]$KSV_original, "double")
      expect_type(xf_plate_pr$assay_info[[1]]$gain1, "double")
      expect_type(xf_plate_pr$assay_info[[1]]$gain2, "double")
      expect_type(xf_plate_pr$assay_info[[1]]$pH_0, "double")
      expect_type(xf_plate_pr$assay_info[[1]]$pH_targetEmission, "double")
      expect_type(xf_plate_pr$assay_info[[1]]$O2_targetEmission, "double")
      expect_type(xf_plate_pr$assay_info[[1]]$O2_0_mmHg, "double")
      expect_type(xf_plate_pr$assay_info[[1]]$O2_0_mM, "double")
      expect_type(xf_plate_pr$injection_info[[1]]$interval, "double")
      expect_type(xf_plate_pr$raw_data[[1]]$timescale, "double")
      expect_type(xf_plate_pr$raw_data[[1]]$minutes, "double")
      expect_type(xf_plate_pr$raw_data[[1]]$interval, "double")
      expect_type(xf_plate_pr$raw_data[[1]]$O2_em_corr, "double")
      expect_type(xf_plate_pr$raw_data[[1]]$pH_em_corr, "double")
      expect_type(xf_plate_pr$raw_data[[1]]$O2_mmHg, "double")
      expect_type(xf_plate_pr$raw_data[[1]]$pH_em_corr_corr, "double")
      expect_type(xf_plate_pr$raw_data[[1]]$O2_em_corr_bkg, "double")
      expect_type(xf_plate_pr$raw_data[[1]]$pH_em_corr_bkg, "double")
      expect_type(xf_plate_pr$raw_data[[1]]$O2_mmHg_bkg, "double")
      expect_type(xf_plate_pr$raw_data[[1]]$pH_bkgd, "double")
      expect_type(xf_plate_pr$raw_data[[1]][18]$pH_em_corr_corr_bkg, "double")
      expect_type(xf_plate_pr$raw_data[[1]][19]$bufferfactor, "double")
      expect_type(xf_plate_pr$rate_data[[1]]$measurement, "double")
      expect_type(xf_plate_pr$rate_data[[1]]$time_wave, "double")
      expect_type(xf_plate_pr$rate_data[[1]]$OCR_wave, "double")
      expect_type(xf_plate_pr$rate_data[[1]]$OCR_wave_bc, "double")
      expect_type(xf_plate_pr$rate_data[[1]]$ECAR_wave, "double")
      expect_type(xf_plate_pr$rate_data[[1]]$ECAR_wave_bc, "double")
    })

    test_that("type integer", {
      expect_type(xf_raw_pr$measurement, "integer")
      expect_type(xf_raw_pr$tick, "integer")
      expect_type(xf_plate_pr$injection_info[[1]]$measurement, "integer")
      expect_type(xf_plate_pr$raw_data[[1]]$measurement, "integer")
      expect_type(xf_plate_pr$raw_data[[1]]$tick, "integer")
      expect_type(xf_plate_pr$rate_data[[1]]$measurement, "double") # actually an integer
    })

    test_that("type logical", {
      expect_type(xf_raw_pr$flagged_well, "logical")
      expect_type(xf_plate_pr$assay_info[[1]]$KSV_tempCorrection, "logical")
      expect_type(xf_plate_pr$assay_info[[1]]$norm_available, "logical")
      expect_type(xf_plate_pr$rate_data[[1]]$flagged_well, "logical")
    })

    test_that("check column names", {
      check_column_names(xf_plate_pr, list("plate_id", "filepath_seahorse", "date", "assay_info", "injection_info", "raw_data", "rate_data"))
      check_column_names(xf_raw_pr, list("plate_id" , "well" , "measurement", "tick", "timescale", "minutes", "group", "injection", "O2_em_corr",
                                         "pH_em_corr", "O2_mmHg", "pH", "pH_em_corr_corr", "O2_em_corr_bkg", "pH_em_corr_bkg", "O2_mmHg_bkg",
                                         "pH_bkgd", "pH_em_corr_corr_bkg", "cell_n", "flagged_well"))
    })

    test_that("type date", {
      expect_type(xf_plate_pr$date, "double")
      expect_type(xf_plate_pr$assay_info[[1]]$date_run, "double")
    })

  test_that("type logical or double", {
      if (all(is.na(xf_plate_pr$raw_data[[1]]$cell_n)) == TRUE) {
        expect_type(xf_plate_pr$raw_data[[1]]$cell_n, "logical")
      } else {
        expect_type(xf_plate_pr$raw_data[[1]]$cell_n, "double") #actually integer
      }

      if (all(is.na(xf_raw_pr$cell_n)) == TRUE) {
        expect_type(xf_raw_pr$cell_n, "logical")
      } else {
        expect_type(xf_raw_pr$cell_n, "double") #actually integer
      }

      if (all(is.na(xf_plate_pr$rate_data[[1]]$cell_n)) == TRUE) {
        expect_type(xf_plate_pr$rate_data[[1]]$cell_n, "logical")
      } else {
        expect_type(xf_plate_pr$rate_data[[1]]$cell_n, "double") #actually integer
      }

    })

  })

}
