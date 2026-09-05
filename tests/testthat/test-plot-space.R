make_df_space <- function() {
  tibble::tibble(
    group = c("control", "treated"),
    basal_ocr = c(25, 30), fccp_ocr = c(50, 60), amrot_ocr = c(15, 18),
    basal_ecar = c(20, 28), fccp_ecar = c(22, 32), amrot_ecar = c(18, 21),
    max_ecar = c(30, 36)
  )
}
test_that("plot_bioenergetic_space returns a ggplot", {
  expect_s3_class(plot_bioenergetic_space(make_df_space()), "ggplot")
})
test_that("plot_bioenergetic_trajectory returns a ggplot", {
  expect_s3_class(plot_bioenergetic_trajectory(make_df_space()), "ggplot")
})
test_that("plot functions error on missing required columns", {
  bad <- tibble::tibble(group = "a", basal_ocr = 1)
  expect_error(plot_bioenergetic_space(bad), regexp = "required column|Missing")
  expect_error(plot_bioenergetic_trajectory(bad), regexp = "required column|Missing")
})
