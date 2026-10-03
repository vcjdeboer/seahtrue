# The rate table and the fitted-values table.
#
# Work item st-seahtrue-rate-calculation-method-value-object-method-5j2b.
# The constructors are internal: method functions (inside seahtrue) build
# them; everyone else gets them from compute_rates(), which checks them.

PLATE_ID_PATTERN <- "^[0-9a-f]{64}$"
WELL_PATTERN <- "^[A-H](0[1-9]|1[0-2])$"
RATE_TABLE_COLUMNS <- c("plate_id", "method_id", "well", "measurement",
                        "rate_kind", "value", "unit")
FITTED_VALUES_COLUMNS <- c("plate_id", "method_id", "term", "value", "unit")

# Builds the rate table of one method for one plate. `rates` has columns
# well, measurement, rate_kind and value; plate_id, method_id and unit are
# added. Carries the method digest as attribute "method_digest".
.rate_table <- function(plate_id, method, rates) {
    .check_string(plate_id, "plate id", PLATE_ID_PATTERN, 64L)
    .check_rate_method(method)
    if (!is.data.frame(rates) ||
        !setequal(names(rates), c("well", "measurement", "rate_kind", "value")) ||
        ncol(rates) != 4L) {
        stop("rates must be a data frame with exactly the columns well, ",
             "measurement, rate_kind, value", call. = FALSE)
    }
    measurement <- rates$measurement
    if (is.double(measurement) && all(is.finite(measurement)) &&
        all(measurement == round(measurement))) {
        measurement <- as.integer(measurement)
    }
    unit <- unname(RATE_KIND_UNITS[as.character(rates$rate_kind)])
    x <- tibble::tibble(
        plate_id = rep(plate_id, nrow(rates)),
        method_id = rep(method_id(method), nrow(rates)),
        well = rates$well,
        measurement = measurement,
        rate_kind = rates$rate_kind,
        value = rates$value,
        unit = unit
    )
    class(x) <- c("seahtrue_rate_table", class(x))
    attr(x, "method_digest") <- method_digest(method)
    .check_rate_table(x, plate_id, method)
    x
}

# Builds the fitted-values table of one method for one plate. `values` has
# columns term, value and unit.
.fitted_values_table <- function(plate_id, method, values) {
    .check_string(plate_id, "plate id", PLATE_ID_PATTERN, 64L)
    .check_rate_method(method)
    if (!is.data.frame(values) ||
        !setequal(names(values), c("term", "value", "unit")) ||
        ncol(values) != 3L) {
        stop("fitted values must be a data frame with exactly the columns ",
             "term, value, unit", call. = FALSE)
    }
    x <- tibble::tibble(
        plate_id = rep(plate_id, nrow(values)),
        method_id = rep(method_id(method), nrow(values)),
        term = values$term,
        value = values$value,
        unit = values$unit
    )
    class(x) <- c("seahtrue_fitted_values_table", class(x))
    attr(x, "method_digest") <- method_digest(method)
    .check_fitted_values_table(x, plate_id, method)
    x
}

# A rate table of exactly `method` for exactly `plate_id`: one plate, one
# method id on every row, only that method's rate kinds, one row per well,
# measurement and rate kind.
.check_rate_table <- function(x, plate_id, method) {
    id <- method_id(method)
    fail <- function(...) {
        stop("not a rate table of ", id, ": ", ..., call. = FALSE)
    }
    if (!inherits(x, "seahtrue_rate_table") || !is.data.frame(x)) {
        fail("wrong class")
    }
    if (!identical(names(x), RATE_TABLE_COLUMNS)) {
        fail("its columns must be ", paste(RATE_TABLE_COLUMNS, collapse = ", "))
    }
    if (!identical(attr(x, "method_digest"), method_digest(method))) {
        fail("its method digest is not the registered method's")
    }
    if (!is.character(x$plate_id) || !all(x$plate_id %in% plate_id)) {
        fail("a row belongs to another plate")
    }
    if (!is.character(x$method_id) || !all(x$method_id %in% id)) {
        fail("a row carries another method id")
    }
    if (!is.character(x$well) || anyNA(x$well) || !all(grepl(WELL_PATTERN, x$well))) {
        fail("a well is not A01..H12")
    }
    if (!is.integer(x$measurement) || anyNA(x$measurement) ||
        any(x$measurement < 1L)) {
        fail("a measurement is not a positive whole number")
    }
    kinds <- intersect(method$output_kinds, RATE_KINDS)
    if (!is.character(x$rate_kind) || anyNA(x$rate_kind) ||
        !all(x$rate_kind %in% kinds)) {
        fail("a rate kind is not among the method's rate kinds (",
             paste(kinds, collapse = ", "), ")")
    }
    if (!is.double(x$value) || any(is.nan(x$value) | is.infinite(x$value))) {
        fail("a value is not a finite number or NA")
    }
    if (!is.character(x$unit) ||
        !identical(unname(x$unit), unname(RATE_KIND_UNITS[x$rate_kind]))) {
        fail("a unit is not its rate kind's unit")
    }
    if (anyDuplicated(x[, c("well", "measurement", "rate_kind")])) {
        fail("a well, measurement and rate kind appears twice")
    }
    invisible(TRUE)
}

.check_fitted_values_table <- function(x, plate_id, method) {
    id <- method_id(method)
    fail <- function(...) {
        stop("not a fitted-values table of ", id, ": ", ..., call. = FALSE)
    }
    if (!("fitted-values" %in% method$output_kinds)) {
        fail("the method does not output fitted values")
    }
    if (!inherits(x, "seahtrue_fitted_values_table") || !is.data.frame(x)) {
        fail("wrong class")
    }
    if (!identical(names(x), FITTED_VALUES_COLUMNS)) {
        fail("its columns must be ", paste(FITTED_VALUES_COLUMNS, collapse = ", "))
    }
    if (!identical(attr(x, "method_digest"), method_digest(method))) {
        fail("its method digest is not the registered method's")
    }
    if (!is.character(x$plate_id) || !all(x$plate_id %in% plate_id)) {
        fail("a row belongs to another plate")
    }
    if (!is.character(x$method_id) || !all(x$method_id %in% id)) {
        fail("a row carries another method id")
    }
    if (!is.character(x$term) || anyNA(x$term) || anyDuplicated(x$term)) {
        fail("terms must be strings, each once")
    }
    for (t in x$term) .check_free_text(t, "fitted-values term", 128L)
    if (!is.double(x$value) || any(is.nan(x$value) | is.infinite(x$value))) {
        fail("a value is not a finite number or NA")
    }
    if (!is.character(x$unit) || anyNA(x$unit)) fail("a unit is missing")
    for (u in x$unit) .check_free_text(u, "fitted-values unit", 64L)
    invisible(TRUE)
}
