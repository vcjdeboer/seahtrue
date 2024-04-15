
my_filepath <- 
  testthat::test_path("20191219_SciRep_PBMCs_donor_A.xlsx")

my_req_cols_preprocessed_output <-  
  c("plate_id", "filepath_seahorse", 
    "date_run", "date_processed", 
    "assay_info", "injection_info", 
    "raw_data", "rate_data")#, "my_wrong_col")

my_req_cols_readxfplate_output <- 
  c("raw", "rate", "assayinfo",
    "inj", "pHcal", "O2cal", "norm",
    "flagged", "buffer", "filepath_seahorse") #"my_wrong_col")

my_flagged_required <- 
  tibble(well =  c("A05", "A06", "A07", "A08", 
                   "A12", "C08", "F03", "G12", "H09"),
         flag = c(TRUE, TRUE, TRUE, TRUE, 
                  TRUE, TRUE, TRUE, TRUE, TRUE))

testthat::test_that("test_file_exists_and_notNULL", {

  testthat::expect_true(file.exists(my_filepath))
  
  xf_plate_pr <- my_filepath %>%
    read_xfplate() %>%
    preprocess_xfplate()

  testthat::expect_true(!is.null(xf_plate_pr) )
  testthat::expect_true(exists("xf_plate_pr") )
})

testthat::test_that("missing_columns_read_xfplate_output", {

  xf_plate_read <- my_filepath %>%
    read_xfplate()
  
  my_missing <- 
    missing_strings(xf_plate_read %>%  names(),
                    my_req_cols_readxfplate_output)

  if(is.null(my_missing)){
    no_missing_columns = TRUE
  } else {
    no_missing_columns = FALSE
  }
  
  testthat::expect_length(xf_plate_read %>%  names(), 10)
  testthat::expect_true(no_missing_columns)
  
})

testthat::test_that("missing_columns_preprocessed_output", {

  xf_plate_pr <- my_filepath %>%
    read_xfplate() %>%
    preprocess_xfplate()

  my_missing <- 
    missing_strings(xf_plate_pr %>%
                      names(),
                    my_req_cols_preprocessed_output)

  if(is.null(my_missing)){
    no_missing_columns = TRUE
  } else {
    no_missing_columns = FALSE
  }

  testthat::expect_true(no_missing_columns)
})

testthat::test_that("norm_has_type_logical_attribute", {
  
  xf_norm <- my_filepath %>% 
    get_xf_norm() %>% 
    verify_xf_norm()
  
  my_norm_attribute <- attributes(xf_norm) %>%  purrr::pluck("norm_available")
  
  testthat::expect_type(my_norm_attribute, "logical")
  
})

testthat::test_that("flagged_wells_are_correct", {
  
  xf_flagged <- my_filepath %>% 
    get_xf_flagged()
  
  testthat::expect_mapequal(xf_flagged, my_flagged_required)
  
  
})

testthat::test_that("injections_are_correct", {
  
  xf_inj <- my_filepath %>% 
    get_xf_inj()
  
  testthat::expect_setequal(xf_inj %>% pull(measurement), 1:12)
  testthat::expect_setequal(xf_inj %>% pull(injection), 
                            c(rep("Baseline", 3),
                              rep("FCCP", 3),
                              rep("AM/ROT", 3),
                              rep("Monensin/Hoechst", 3))
                              )
  testthat::expect_setequal(xf_inj %>% pull(interval) %>%  unique(), 1:4)
  
  
  
})

testthat::test_that("rate_background_check", {
  
  xf_flagged <- get_xf_flagged(my_filepath)
  
  xf_rate <- my_filepath %>%  get_xf_rate() %>%  verify_xf_rate(., xf_flagged)
  
  testthat::expect_setequal(
    xf_rate %>%  
      names(), 
    c("measurement", "well", "group", "time_wave",
      "OCR_wave", "OCR_wave_bc", "ECAR_wave", "ECAR_wave_bc" ))
  
  testthat::expect_setequal(
    xf_rate %>% 
      filter(group == "Background") %>% 
      pull(OCR_wave_bc), 
    c(rep(0, 36)))
  
  
})

testthat::test_that("assayinfo_check", {
  
  xf_raw <- get_xf_raw(my_filepath)
  xf_assayinfo <- get_xf_assayinfo(my_filepath, xf_raw)
  
  KSV <- xf_assayinfo %>%  purrr::pluck("KSV")
  F0 <-  xf_assayinfo %>%  purrr::pluck("F0")
  min_to_st <- xf_assayinfo %>%  purrr::pluck("minutes_to_start_measurement_one") 
  testthat::expect_type(KSV, "double")
  testthat::expect_type(F0, "double")
  testthat::expect_type(min_to_st, "double")

  
})


