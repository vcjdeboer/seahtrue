test_that("revive_xfplate returns a structure containing rate_data", {
  plate <- example_plate()
  rate <- purrr::pluck(plate, "rate_data", 1)
  expect_s3_class(rate, "tbl_df")
  expect_true(all(c("group", "measurement", "OCR_wave_bc", "ECAR_wave_bc") %in% names(rate)))
})
