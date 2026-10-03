# compute_rates() is tested through the internal .compute_rates() with a
# fixture registry and resolver; the exported function has neither.

producer_fun <- function(plate, method, inputs, plate_inputs) {
    list(rates = .rate_table(plate$plate_id, method, fixture_rates()),
         fitted_values = NULL)
}

consumer_fun <- function(plate, method, inputs, plate_inputs) {
    r <- fixture_rates(c("glycolytic-atp-production-rate",
                         "oxidative-atp-production-rate"))
    v <- data.frame(term = "cells", value = plate_inputs[["cell-count"]],
                    unit = "", stringsAsFactors = FALSE)
    list(rates = .rate_table(plate$plate_id, method, r),
         fitted_values = .fitted_values_table(plate$plate_id, method, v))
}

resolver <- function(funs) function(name) funs[[name]]

run <- function(id, inputs = list(), plate_inputs = list(),
                plate = fixture_plate(), fun = NULL) {
    reg <- fixture_registry()
    funs <- list(fixture_fun = function(plate, method, inputs, plate_inputs) {
        if (method$name == "fixture-a") {
            producer_fun(plate, method, inputs, plate_inputs)
        } else {
            consumer_fun(plate, method, inputs, plate_inputs)
        }
    })
    if (!is.null(fun)) funs$fixture_fun <- fun
    .compute_rates(plate, id, inputs, plate_inputs, registry = reg,
                   resolve_fun = resolver(funs))
}

test_that("the exported compute_rates has no registry or resolver argument", {
    expect_identical(names(formals(compute_rates)),
                     c("plate", "method_id", "inputs", "plate_inputs"))
})

test_that("compute_rates returns the method's rate table and fitted values", {
    a <- run("fixture-a@1")
    expect_s3_class(a$rates, "seahtrue_rate_table")
    expect_null(a$fitted_values)
    b <- run("fixture-b@2", inputs = list(ocr = a$rates, ecar = a$rates),
             plate_inputs = list(`cell-count` = 5000))
    expect_true(all(b$rates$method_id == "fixture-b@2"))
    expect_s3_class(b$fitted_values, "seahtrue_fitted_values_table")
})

test_that("compute_rates refuses a plate that is not one plate dataset plate", {
    expect_error(run("fixture-a@1", plate = fixture_plate("barcode123")),
                 "lowercase sha256")
    two <- rbind(fixture_plate(), fixture_plate())
    expect_error(run("fixture-a@1", plate = two), "one-row")
    expect_error(run("fixture-a@1", plate = list(plate_id = FIXTURE_PLATE_ID)),
                 "one-row")
})

test_that("compute_rates refuses missing, extra or foreign inputs", {
    a <- run("fixture-a@1")
    pin <- list(`cell-count` = 5000)
    expect_error(run("fixture-b@2", inputs = list(ocr = a$rates),
                     plate_inputs = pin), "needs exactly the inputs")
    expect_error(run("fixture-b@2",
                     inputs = list(ocr = a$rates, ecar = a$rates, per = a$rates),
                     plate_inputs = pin), "needs exactly the inputs")
    expect_error(run("fixture-a@1", inputs = list(ocr = a$rates)),
                 "needs exactly the inputs: none")
    other_plate <- run("fixture-a@1", plate = fixture_plate(strrep("cd", 32)))
    expect_error(run("fixture-b@2", inputs = list(ocr = other_plate$rates,
                                                  ecar = a$rates),
                     plate_inputs = pin), "another plate")
    hand_built <- tibble::as_tibble(unclass(a$rates))
    expect_error(run("fixture-b@2", inputs = list(ocr = hand_built,
                                                  ecar = a$rates),
                     plate_inputs = pin), "wrong class")
    forged <- a$rates
    attr(forged, "method_digest") <- strrep("0", 64)
    expect_error(run("fixture-b@2", inputs = list(ocr = forged, ecar = a$rates),
                     plate_inputs = pin), "method digest")
})

test_that("compute_rates refuses a missing or extra declared plate input", {
    a <- run("fixture-a@1")
    ins <- list(ocr = a$rates, ecar = a$rates)
    expect_error(run("fixture-b@2", inputs = ins),
                 "declared plate inputs: cell-count")
    expect_error(run("fixture-b@2", inputs = ins,
                     plate_inputs = list(`cell-count` = 1, extra = 2)),
                 "declared plate inputs")
})

test_that("compute_rates refuses a result that is not the method's", {
    a_method <- fixture_method_a()
    other <- rate_method("fixture-a", 2, "tick-rates", "fixture_fun",
                         c("ocr", "ecar"))
    expect_error(run("fixture-a@1", fun = function(plate, method, ...) {
        list(rates = .rate_table(plate$plate_id, other, fixture_rates()),
             fitted_values = NULL)
    }), "not a rate table of fixture-a@1")
    expect_error(run("fixture-a@1", fun = function(plate, method, ...) {
        list(rates = .rate_table(strrep("cd", 32), method, fixture_rates()),
             fitted_values = NULL)
    }), "another plate")
    expect_error(run("fixture-a@1", fun = function(plate, method, ...) {
        .rate_table(plate$plate_id, method, fixture_rates())
    }), "must return")
    expect_error(run("fixture-a@1", fun = function(plate, method, ...) {
        list(rates = .rate_table(plate$plate_id, method, fixture_rates()),
             fitted_values = data.frame())
    }), "outputs no fitted values")
    a <- run("fixture-a@1")
    expect_error(run("fixture-b@2", inputs = list(ocr = a$rates, ecar = a$rates),
                     plate_inputs = list(`cell-count` = 1),
                     fun = function(plate, method, ...) {
                         r <- fixture_rates(c("glycolytic-atp-production-rate"))
                         list(rates = .rate_table(plate$plate_id, method, r),
                              fitted_values = NULL)
                     }), "fitted-values table")
})

test_that("O2 series methods are refused until phzb defines the container", {
    o2 <- rate_method("o2", 1, "o2-correction", "fixture_fun", "o2-series")
    m2 <- rate_method("m2", 1, "tick-rates", "fixture_fun", "ocr",
                      input_methods = list(`o2-series` = "o2@1"))
    reg <- .check_method_registry(list(o2, m2), exports = "fixture_fun")
    f <- function(...) stop("must not be called")
    expect_error(.compute_rates(fixture_plate(), "o2@1", list(), list(), reg,
                                function(name) f), "phzb")
    expect_error(.compute_rates(fixture_plate(), "m2@1", list(), list(), reg,
                                function(name) f), "phzb")
})

test_that("the resolver returns only exported seahtrue functions", {
    expect_identical(.resolve_method_function("calculate_space"),
                     getExportedValue("seahtrue", "calculate_space"))
    expect_error(.resolve_method_function(".compute_rates"), "not an exported")
    expect_error(.resolve_method_function("system"), "not an exported")
    expect_error(.resolve_method_function("revive_xfplate"), "not an exported")
})

test_that("compute_rates on the shipped (empty) registry refuses every id", {
    expect_error(compute_rates(fixture_plate(), "tick-rates-wave-matching@1"),
                 "no rate calculation method is registered")
})
