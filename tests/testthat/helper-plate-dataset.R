# Synthetic plate datasets for read_plate_dataset() tests. Every value here
# is made up; no real plate data is used.

synthetic_wells <- function() {
  as.vector(t(outer(LETTERS[1:8], sprintf("%02d", 1:12), paste0)))
}

synthetic_background_wells <- c("A01", "A12", "H01", "H12")

# One row per well per tick, in the parser's column order and types.
synthetic_plate_frame <- function(n_measurements = 3L, ticks_per_measurement = 3L,
                                  pH_0 = 7.4, pH_gain1 = 0, pH_gain2 = 0,
                                  background = synthetic_background_wells) {
  wells <- synthetic_wells()
  n_ticks <- n_measurements * ticks_per_measurement
  ticks <- seq_len(n_ticks) - 1L
  grid <- expand.grid(well = wells, tick = ticks, stringsAsFactors = FALSE)
  grid <- grid[order(grid$tick, match(grid$well, wells)), ]
  n <- nrow(grid)
  measurement <- as.integer(grid$tick %/% ticks_per_measurement) + 1L
  injections <- c("Baseline", "FCCP", "AM/rot", "Oligo", "2DG")
  well_index <- match(grid$well, wells)
  data.frame(
    instrument_file_sha256 = strrep("ab", 32),
    date_run = "2019-12-19T16:25:58.068978Z",
    well = grid$well,
    group = ifelse(grid$well %in% background, "Background", "control"),
    flagged_well = FALSE,
    plate_flagged_well = FALSE,
    cell_n = 10000 + well_index,
    normalisation_unit = "Cell number",
    normalisation_scale_factor = 10000,
    bufferfactor = NA_real_,
    measurement = measurement,
    interval = measurement,
    injection = injections[measurement],
    tick = as.integer(grid$tick),
    time_s = 2234 + 20 * grid$tick,
    # about 150 mmHg O2 with F0 = 50000 and Ksv = 0.0219
    O2_em_corr = 11669 + (well_index %% 7) * 10 - grid$tick,
    pH_em_corr = 20000 + (well_index %% 5) * 10,
    O2_cal_em = 30000,
    pH_cal_em = 20000,
    O2_target_emission = 30000,
    pH_target_emission = 20000,
    O2_F0 = 50000,
    O2_ksv = 0.0219,
    O2_0_mmHg = 151.69,
    O2_0_mM = 0.214,
    chamber_volume = 2.28,
    tau_AC = 746,
    tau_W = 296,
    tau_C = 246,
    tau_P = 60.9,
    plate_volume = 0.19,
    pH_0 = pH_0,
    pH_gain1 = pH_gain1,
    pH_gain2 = pH_gain2,
    stringsAsFactors = FALSE
  )
}

write_plate_dataset <- function(df, path = tempfile(fileext = ".parquet"),
                                metadata = c(plate_dataset_schema_version = "1"),
                                arrow_metadata = FALSE) {
  nanoparquet::write_parquet(
    df, path,
    metadata = metadata,
    options = nanoparquet::parquet_options(write_arrow_metadata = arrow_metadata)
  )
  path
}

fixed_time <- as.POSIXct("2026-10-03 12:00:00", tz = "UTC")

read_synthetic <- function(df = synthetic_plate_frame(), ...) {
  suppressMessages(read_plate_dataset(write_plate_dataset(df, ...),
                                      date_processed = fixed_time))
}

expect_refused <- function(expr, pattern) {
  testthat::expect_error(suppressMessages(expr), pattern,
                         class = "seahtrue_plate_dataset_refused")
}
