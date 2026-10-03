# Tests for read_plate_dataset() on synthetic plate datasets only.

test_that("a version-1 plate dataset reads and passes validation", {
  p <- read_synthetic()
  expect_s3_class(p, "tbl_df")
  expect_equal(nrow(p), 1L)
  expect_named(p, c("plate_id", "filepath_seahorse", "date_run",
                    "date_processed", "assay_info", "injection_info",
                    "raw_data", "rate_data", "validation_output"))
  expect_equal(p$plate_id, strrep("ab", 32))
  v <- p$validation_output[[1]]
  expect_true(v$all_96_wells_are_present)
  expect_equal(nrow(v$time_info), 3L)
  raw <- p$raw_data[[1]]
  expect_equal(nrow(raw), 96L * 9L)
  expect_type(raw$group, "character")
})

test_that("in-range values give no validation flag; an out-of-range well is flagged", {
  flags <- read_synthetic()$validation_output[[1]]$failed_ticks_combined$flag
  flag_cols <- c("start_O2", "first_O2", "last_O2",
                 "start_pH", "first_pH", "last_pH")
  expect_false(any(as.matrix(flags[, flag_cols])))

  df <- synthetic_plate_frame()
  df$O2_em_corr[df$well == "C03"] <- 30000 # about 30 mmHg O2
  flags <- read_synthetic(df)$validation_output[[1]]$failed_ticks_combined$flag
  flagged <- flags$well[flags$start_O2 | flags$first_O2 | flags$last_O2]
  expect_equal(flagged, "C03")
})

test_that("the O2 and pH conversions give hand-computed values", {
  df <- synthetic_plate_frame(pH_0 = 7.2, pH_gain1 = 0.001, pH_gain2 = 5)
  df$pH_cal_em[df$well == "B02"] <- 19000
  raw <- read_synthetic(df)$raw_data[[1]]
  row <- raw[raw$well == "B02" & raw$tick == 4L, ]
  src <- df[df$well == "B02" & df$tick == 4L, ]

  o2 <- (1 / 0.0219) * ((50000 / src$O2_em_corr) - 1)
  expect_equal(row$O2_mmHg, o2)

  calibrated <- (20000 / 19000) * src$pH_em_corr
  slope <- 0.0001338185 + (-0.1334797) * 0.001 + (-4.386826e-06) * 5
  expect_equal(row$pH_em_corr_corr, calibrated)
  expect_equal(row$pH, 7.2 + slope * (calibrated - 20000))
})

test_that("row order in the file does not change the result", {
  df <- synthetic_plate_frame()
  set.seed(1)
  shuffled <- df[sample(nrow(df)), ]
  without_file_name <- function(p) p[, names(p) != "filepath_seahorse"]
  expect_identical(without_file_name(read_synthetic(shuffled)),
                   without_file_name(read_synthetic(df)))
})

test_that("two reads with the same date_processed are identical", {
  path <- write_plate_dataset(synthetic_plate_frame())
  a <- suppressMessages(read_plate_dataset(path, date_processed = fixed_time))
  b <- suppressMessages(read_plate_dataset(path, date_processed = fixed_time))
  expect_identical(a, b)
})

test_that("date_run round-trips to the same UTC instant", {
  p <- read_synthetic()
  expect_equal(attr(p$date_run, "tzone"), "UTC")
  expect_equal(format(p$date_run, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"),
               "2019-12-19T16:25:58.068978Z")
  df <- synthetic_plate_frame()
  df$date_run <- "2019-12-19T16:25:58Z"
  expect_equal(read_synthetic(df)$date_run,
               as.POSIXct("2019-12-19 16:25:58", tz = "UTC"))
})

test_that("assay_info carries the schema version and the parser schema sha256", {
  info <- read_synthetic()$assay_info[[1]]
  expect_equal(info$plate_dataset_schema_version, "1")
  expect_match(info$plate_dataset_schema_sha256, "^[0-9a-f]{64}$")
  expect_equal(info$O2_ksv, 0.0219)
  expect_false("instrument" %in% names(info))
})

test_that("an unknown or missing schema version is refused, naming the versions", {
  path <- write_plate_dataset(synthetic_plate_frame(),
                              metadata = c(plate_dataset_schema_version = "2"))
  expect_refused(read_plate_dataset(path), "Unknown plate dataset schema version \"2\"; known versions: 1")
  path <- write_plate_dataset(synthetic_plate_frame(), metadata = c(other = "x"))
  expect_refused(read_plate_dataset(path), "no plate_dataset_schema_version; known versions: 1")
})

test_that("a refusal quotes file values truncated and escaped", {
  path <- write_plate_dataset(
    synthetic_plate_frame(),
    metadata = c(plate_dataset_schema_version = paste0("{system('x')}\001", strrep("z", 100)))
  )
  err <- tryCatch(suppressMessages(read_plate_dataset(path)), error = identity)
  expect_s3_class(err, "seahtrue_plate_dataset_refused")
  msg <- conditionMessage(err)
  expect_match(msg, "{system('x')}\\001", fixed = TRUE)
  expect_match(msg, "...", fixed = TRUE)
  expect_false(grepl(strrep("z", 100), msg, fixed = TRUE))
})

test_that("extra, missing, reordered and mistyped columns are refused", {
  df <- synthetic_plate_frame()
  extra <- df
  extra$OCR_wave <- 1
  expect_refused(read_plate_dataset(write_plate_dataset(extra)), "outside schema version 1")
  expect_refused(read_plate_dataset(write_plate_dataset(df[, names(df) != "pH_0"])),
                 "lacks columns: pH_0")
  expect_refused(read_plate_dataset(write_plate_dataset(df[, rev(names(df))])),
                 "not in schema order")
  mistyped <- df
  mistyped$tick <- as.double(mistyped$tick)
  expect_refused(read_plate_dataset(write_plate_dataset(mistyped)),
                 "Column tick is not of type INT32")
})

test_that("rule-breaking plate datasets are refused", {
  df <- synthetic_plate_frame()

  bad <- df
  bad$instrument_file_sha256 <- "not-a-sha"
  expect_refused(read_synthetic(bad), "not a lowercase sha256")

  bad <- df
  bad$date_run <- "19-12-2019 16:25"
  expect_refused(read_synthetic(bad), "date_run is not ISO 8601 UTC")

  bad <- df
  bad$pH_0[1] <- 7.3
  expect_refused(read_synthetic(bad), "pH_0 must have one value per plate")

  bad <- df
  bad$time_s[bad$tick == 2L & bad$well == "D04"] <- 1
  expect_refused(read_synthetic(bad), "time_s must have one value per tick")

  bad <- df
  bad$injection[bad$tick == 0L & bad$well == "D04"] <- "Other"
  expect_refused(read_synthetic(bad), "injection must have one value per measurement")

  bad <- df
  bad$group[bad$well == "D04" & bad$tick == 1L] <- "other"
  expect_refused(read_synthetic(bad), "group must have one value per well")

  bad <- df
  bad$tick[bad$well == "D04" & bad$tick == 1L] <- 0L
  expect_refused(read_synthetic(bad), "same tick twice")

  bad <- df[!(df$well == "D04" & df$tick == 8L), ]
  expect_refused(read_synthetic(bad), "Not every well has every tick")

  bad <- df
  bad$well[bad$well == "D04"] <- "D4"
  expect_refused(read_synthetic(bad), "Well names must be A01 to H12")

  bad <- df[df$well != "D04", ]
  expect_refused(read_synthetic(bad), "needs 96 wells; found 95")

  bad <- df
  bad$O2_F0[] <- NA_real_
  expect_refused(read_synthetic(bad), "O2_F0 has missing values")

  bad <- df
  bad$group[bad$well == "D04"] <- strrep("g", 300)
  expect_refused(read_synthetic(bad), "over-long value")
})

test_that("size caps are enforced", {
  path <- write_plate_dataset(synthetic_plate_frame())
  expect_refused(read_plate_dataset(path, max_file_bytes = 10), "larger than")
  expect_refused(read_plate_dataset(tempfile()), "does not exist")
})

# The footer values below come from the file itself; a crafted file can claim
# any of them, so each is replaced through a mocked footer reader.
with_footer <- function(path, change) {
  real <- nanoparquet::read_parquet_metadata(path)
  testthat::local_mocked_bindings(
    read_parquet_metadata = function(file, ...) change(real),
    .package = "nanoparquet",
    .env = parent.frame()
  )
}

test_that("a footer row count over the cap is refused before reading", {
  path <- write_plate_dataset(synthetic_plate_frame())
  with_footer(path, function(m) {
    m$file_meta_data$num_rows <- PLATE_DATASET_MAX_ROWS + 1
    m
  })
  expect_refused(read_plate_dataset(path), "more rows than allowed")
})

test_that("rows read differing from the footer row count are refused", {
  path <- write_plate_dataset(synthetic_plate_frame())
  with_footer(path, function(m) {
    m$file_meta_data$num_rows <- m$file_meta_data$num_rows - 1
    m
  })
  expect_refused(read_plate_dataset(path), "differ from the file's own row count")
})

test_that("an uncompressed size over the cap is refused before reading", {
  path <- write_plate_dataset(synthetic_plate_frame())
  with_footer(path, function(m) {
    m$column_chunks$total_uncompressed_size[1] <- PLATE_DATASET_MAX_UNCOMPRESSED + 1
    m
  })
  expect_refused(read_plate_dataset(path), "too large once uncompressed")
})

test_that("non-finite values are refused, in the file and after conversion", {
  df <- synthetic_plate_frame()

  bad <- df
  bad$O2_em_corr[1] <- Inf
  expect_refused(read_synthetic(bad), "O2_em_corr has a non-finite value")

  bad <- df
  bad$pH_cal_em[bad$well == "B02"] <- -Inf
  expect_refused(read_synthetic(bad), "pH_cal_em has a non-finite value")

  bad <- df
  bad$cell_n[bad$well == "B02"] <- Inf # nullable, but never infinite
  expect_refused(read_synthetic(bad), "cell_n has a non-finite value")

  bad <- df
  bad$O2_em_corr[bad$well == "C03" & bad$tick == 4L] <- 0
  expect_refused(read_synthetic(bad), "non-finite O2_mmHg value")

  bad <- df
  bad$O2_ksv[] <- 0
  expect_refused(read_synthetic(bad), "non-finite O2_mmHg value")

  bad <- df
  bad$pH_cal_em[bad$well == "B02"] <- 0
  expect_refused(read_synthetic(bad), "non-finite pH_em_corr_corr value")
})

test_that("file metadata is read as data only: nothing is executed or restored", {
  canary <- tempfile("canary")
  payload_code <- sprintf("writeLines('pwned', '%s')", canary)
  payload <- rawToChar(serialize(
    structure(list(), class = "hostile", onload = parse(text = payload_code)),
    NULL, ascii = TRUE
  ))
  df <- synthetic_plate_frame()
  df$group <- factor(df$group)
  path <- write_plate_dataset(
    df,
    metadata = c(plate_dataset_schema_version = "1", r = payload,
                 eval = payload_code),
    arrow_metadata = TRUE
  )
  p <- suppressMessages(read_plate_dataset(path, date_processed = fixed_time))
  expect_false(file.exists(canary))
  raw <- p$raw_data[[1]]
  expect_type(raw$group, "character")
  expect_null(attributes(raw$group))
  expect_null(attr(p, "hostile"))
})

test_that("a plate with no background well gets missing background columns", {
  raw <- read_synthetic(synthetic_plate_frame(background = character()))$raw_data[[1]]
  expect_true(all(is.na(raw$O2_em_corr_bkg)))
  expect_true(all(is.na(raw$pH_bkgd)))
})

test_that("background columns come from every Background well, flagged or not", {
  df <- synthetic_plate_frame()
  df$flagged_well[df$well == "A01"] <- TRUE
  df$plate_flagged_well[df$well == "H12"] <- TRUE
  raw <- read_synthetic(df)$raw_data[[1]]
  t0 <- raw[raw$tick == 0L, ]
  used <- t0$well %in% synthetic_background_wells
  expect_equal(unique(t0$O2_em_corr_bkg), mean(t0$O2_em_corr[used]))
})

test_that("both manual flags are carried as data, never exclusions, and may disagree", {
  df <- synthetic_plate_frame()
  df$flagged_well[df$well %in% c("A05", "C08")] <- TRUE
  df$plate_flagged_well[df$well %in% c("A05", "E06", "F01")] <- TRUE
  p <- read_synthetic(df)
  raw <- p$raw_data[[1]]
  expect_equal(nrow(raw), 96L * 9L)
  expect_true(p$validation_output[[1]]$all_96_wells_are_present)
  expect_equal(sort(unique(raw$well[raw$flagged_well])), c("A05", "C08"))
  expect_equal(sort(unique(raw$well[raw$plate_flagged_well])), c("A05", "E06", "F01"))
  flags <- p$validation_output[[1]]$failed_ticks_combined$flag
  expect_true(all(c("A05", "C08", "E06", "F01") %in% flags$well))
})

test_that("a plate dataset without plate_flagged_well is refused", {
  df <- synthetic_plate_frame()
  expect_refused(
    read_plate_dataset(write_plate_dataset(df[, names(df) != "plate_flagged_well"])),
    "lacks columns: plate_flagged_well"
  )
})

test_that("the output carries no Agilent or Wave name and no rates", {
  p <- read_synthetic()
  denylist <- c("OCR_wave", "ECAR_wave", "OCR_wave_bc", "ECAR_wave_bc",
                "O2 (mmHg)", "O2 Corrected Em.", "pH Corrected Em.", "Well",
                "Measurement", "Group", "Tick", "TimeStamp", "OCR", "ECAR",
                "PER", "Well Temperature", "Env. Temperature")
  all_names <- c(names(p), unlist(lapply(
    p[, c("filepath_seahorse", "assay_info", "injection_info",
          "raw_data", "rate_data")],
    function(x) names(x[[1]])
  )))
  expect_length(intersect(all_names, denylist), 0L)
  expect_false(any(grepl("_wave$", all_names)))
  expect_equal(nrow(p$rate_data[[1]]), 0L)
  expect_named(p$rate_data[[1]], c("plate_id", "well", "measurement", "group"))
  fp <- p$filepath_seahorse[[1]]
  expect_true(is.na(fp$directory_path))
  expect_true(is.na(fp$full_path))
  expect_false(grepl("/", fp$base_name, fixed = TRUE))
})
