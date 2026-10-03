test_that("a rate table carries the plate, the method id and the unit on every row", {
    a <- fixture_method_a()
    rt <- .rate_table(FIXTURE_PLATE_ID, a, fixture_rates())
    expect_s3_class(rt, "seahtrue_rate_table")
    expect_identical(names(rt), RATE_TABLE_COLUMNS)
    expect_true(all(rt$plate_id == FIXTURE_PLATE_ID))
    expect_true(all(rt$method_id == "fixture-a@1"))
    expect_identical(rt$unit[rt$rate_kind == "ocr"][1], "pmol/min")
    expect_identical(rt$unit[rt$rate_kind == "ecar"][1], "mpH/min")
    expect_type(rt$measurement, "integer")
    expect_identical(attr(rt, "method_digest"), FIXTURE_A_SHA256)
})

test_that("a rate table refuses what is not that method's rates for that plate", {
    a <- fixture_method_a()
    expect_error(.rate_table("ABC", a, fixture_rates()), "plate id")
    expect_error(.rate_table(FIXTURE_PLATE_ID, a, fixture_rates("per")),
                 "rate kind is not among")
    r <- fixture_rates()
    r$rate_kind[1] <- "o2-series"
    expect_error(.rate_table(FIXTURE_PLATE_ID, a, r), "rate kind is not among")
    r <- fixture_rates()
    r$rate_kind[1] <- "fitted-values"
    expect_error(.rate_table(FIXTURE_PLATE_ID, a, r), "rate kind is not among")
    r <- fixture_rates()
    r$well[1] <- "I01"
    expect_error(.rate_table(FIXTURE_PLATE_ID, a, r), "A01..H12")
    r <- fixture_rates()
    r$measurement[1] <- 0L
    expect_error(.rate_table(FIXTURE_PLATE_ID, a, r), "positive whole number")
    r <- fixture_rates()
    r <- rbind(r, r[1, ])
    expect_error(.rate_table(FIXTURE_PLATE_ID, a, r), "appears twice")
    r <- fixture_rates()
    r$value[1] <- Inf
    expect_error(.rate_table(FIXTURE_PLATE_ID, a, r), "finite number or NA")
    r <- fixture_rates()
    r$value[1] <- NA_real_
    expect_s3_class(.rate_table(FIXTURE_PLATE_ID, a, r), "seahtrue_rate_table")
    r <- fixture_rates()
    r$extra <- 1
    expect_error(.rate_table(FIXTURE_PLATE_ID, a, r), "exactly the columns")
})

test_that("a rate table checked against another method or plate is refused", {
    a <- fixture_method_a()
    rt <- .rate_table(FIXTURE_PLATE_ID, a, fixture_rates())
    expect_error(.check_rate_table(rt, strrep("cd", 32), a), "another plate")
    other <- rate_method("fixture-a", 2, "tick-rates", "fixture_fun",
                         c("ocr", "ecar"))
    expect_error(.check_rate_table(rt, FIXTURE_PLATE_ID, other),
                 "method digest|another method id")
    forged <- rt
    attr(forged, "method_digest") <- NULL
    expect_error(.check_rate_table(forged, FIXTURE_PLATE_ID, a), "method digest")
    plain <- tibble::as_tibble(unclass(rt))
    expect_error(.check_rate_table(plain, FIXTURE_PLATE_ID, a), "wrong class")
})

test_that("a fitted-values table only for a method that outputs fitted values", {
    a <- fixture_method_a()
    b <- fixture_method_b()
    v <- data.frame(term = c("intercept", "slope"), value = c(1, 2),
                    unit = c("pmol/min", ""), stringsAsFactors = FALSE)
    fv <- .fitted_values_table(FIXTURE_PLATE_ID, b, v)
    expect_s3_class(fv, "seahtrue_fitted_values_table")
    expect_identical(names(fv), FITTED_VALUES_COLUMNS)
    expect_true(all(fv$method_id == "fixture-b@2"))
    expect_error(.fitted_values_table(FIXTURE_PLATE_ID, a, v),
                 "does not output fitted values")
    v2 <- v
    v2$term[2] <- "intercept"
    expect_error(.fitted_values_table(FIXTURE_PLATE_ID, b, v2), "each once")
})
