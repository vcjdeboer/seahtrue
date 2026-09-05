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
