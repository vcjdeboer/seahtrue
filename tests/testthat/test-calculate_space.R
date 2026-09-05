test_that("calculate_space errors clearly on a non-tibble rate", {
  expect_error(
    calculate_space(rate = 42,
                    param_set_ocr = c(init_ocr = "m3"),
                    param_set_ecar = c(init_ecar = "m3")),
    regexp = "rate"
  )
})

test_that("calculate_space errors when rate lacks required columns", {
  bad <- tibble::tibble(group = "a", measurement = 1L)  # missing OCR_wave_bc/ECAR_wave_bc
  expect_error(
    calculate_space(rate = bad,
                    param_set_ocr = c(init_ocr = "m3"),
                    param_set_ecar = c(init_ecar = "m3")),
    regexp = "is missing required column"
  )
})

test_that("calculate_space errors on unnamed param_set", {
  rate <- tibble::tibble(group = "a", measurement = 1L,
                         OCR_wave_bc = 1, ECAR_wave_bc = 1)
  expect_error(
    calculate_space(rate = rate,
                    param_set_ocr = c("m3"),          # names missing
                    param_set_ecar = c(init_ecar = "m3")),
    regexp = "param_set_ocr"
  )
})

test_that("calculate_space warns when plot-required canonical names are missing", {
  file <- system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx",
                      package = "seahtrue")
  skip_if(file == "", "example data not installed")
  rate <- purrr::pluck(revive_xfplate(file), "rate_data", 1)
  expect_warning(
    calculate_space(
      rate = rate,
      param_set_ocr  = c(init_ocr = "m3", fccp_ocr = "m4", amrot_ocr = "m9", mon_ocr = "m12"),
      param_set_ecar = c(init_ecar = "m3", fccp_ecar = "m4", weird_ecar = "m9", mon_ecar = "m12")
    ),
    regexp = "amrot_ecar|canonical|downstream"
  )
})

test_that("calculate_space returns one row per group with expected columns", {
  rate <- purrr::pluck(example_plate(), "rate_data", 1)
  out <- calculate_space(
    rate = rate,
    param_set_ocr  = c(init_ocr = "m3", fccp_ocr = "m4", amrot_ocr = "m9", mon_ocr = "m12"),
    param_set_ecar = c(init_ecar = "m3", fccp_ecar = "m4", amrot_ecar = "m9", mon_ecar = "m12"),
    conversion_model = "mookerjee"
  )
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), dplyr::n_distinct(rate$group))
  expect_true(all(c("group", "basal_ocr", "fccp_ocr", "amrot_ocr",
                    "basal_ecar", "fccp_ecar", "amrot_ecar",
                    "supply_index", "glyco_index") %in% names(out)))
  expect_type(out$supply_index, "double")
})

test_that("conversion_model presets produce different J values", {
  rate <- purrr::pluck(example_plate(), "rate_data", 1)
  ps_ocr  <- c(init_ocr = "m3", fccp_ocr = "m4", amrot_ocr = "m9", mon_ocr = "m12")
  ps_ecar <- c(init_ecar = "m3", fccp_ecar = "m4", amrot_ecar = "m9", mon_ecar = "m12")
  m <- calculate_space(rate, ps_ocr, ps_ecar, conversion_model = "mookerjee")
  a <- calculate_space(rate, ps_ocr, ps_ecar, conversion_model = "agilent")
  expect_false(isTRUE(all.equal(m$basal_ocr, a$basal_ocr)))
})

test_that("calculate_space accepts custom atp_factors that override the preset", {
  rate <- purrr::pluck(example_plate(), "rate_data", 1)
  ps_ocr  <- c(init_ocr = "m3", fccp_ocr = "m4", amrot_ocr = "m9", mon_ocr = "m12")
  ps_ecar <- c(init_ecar = "m3", fccp_ecar = "m4", amrot_ecar = "m9", mon_ecar = "m12")
  preset <- calculate_space(rate, ps_ocr, ps_ecar, conversion_model = "mookerjee")
  custom <- calculate_space(
    rate, ps_ocr, ps_ecar,
    atp_factors = list(Jglyco_ecar_factor = 1, Jglyco_ocr_factor = 0, Joxphos_ocr_factor = 1)
  )
  # custom factors differ from the mookerjee preset, so derived OCR/ECAR metrics differ
  expect_false(isTRUE(all.equal(preset$basal_ocr, custom$basal_ocr)))
})

test_that("calculate_space rejects malformed atp_factors", {
  rate <- purrr::pluck(example_plate(), "rate_data", 1)
  ps_ocr  <- c(init_ocr = "m3", fccp_ocr = "m4", amrot_ocr = "m9", mon_ocr = "m12")
  ps_ecar <- c(init_ecar = "m3", fccp_ecar = "m4", amrot_ecar = "m9", mon_ecar = "m12")
  expect_error(
    calculate_space(rate, ps_ocr, ps_ecar,
                    atp_factors = list(Jglyco_ecar_factor = 1)),  # missing two required
    regexp = "atp_factors"
  )
})
