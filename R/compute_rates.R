# compute_rates(): the one entry point that computes rates with a registered
# rate calculation method.
#
# Work item st-seahtrue-rate-calculation-method-value-object-method-5j2b.

#' Compute rates with a registered rate calculation method
#'
#' Looks the method id up in the method registry (exact match), checks the
#' plate, the input methods' rate tables and the declared plate inputs,
#' calls the method function, and checks what it returns. Every rate
#' returned carries the method id.
#'
#' A method function \code{fun} is called as
#' \code{fun(plate, method, inputs, plate_inputs)} and returns
#' \code{list(rates = <rate table>, fitted_values = <fitted-values table or
#' NULL>)}.
#'
#' An O2 series input or output is refused for now: its container is
#' defined by the O2 AUC method's work item
#' (st-seahtrue-o2-auc-method-phzb).
#'
#' @param plate One plate: a one-row data frame whose \code{plate_id} is the
#'   lowercase sha256 of its instrument file, as the plate dataset reader
#'   returns it.
#' @param method_id The method id, \code{"name@version"}.
#' @param inputs A named list: for each input method of the method, keyed
#'   as the method declares it, that input method's rate table for the same
#'   plate (as \code{compute_rates()} returned it).
#' @param plate_inputs A named list holding exactly the method's declared
#'   plate inputs (for example \code{cell-count}).
#'
#' @return \code{list(rates, fitted_values)}: the method's rate table for
#'   this plate, and its fitted-values table (\code{NULL} unless the method
#'   outputs fitted values).
#' @export
compute_rates <- function(plate, method_id, inputs = list(),
                          plate_inputs = list()) {
    .compute_rates(plate, method_id, inputs, plate_inputs,
                   registry = method_registry(),
                   resolve_fun = .resolve_method_function)
}

.O2_SERIES_NOT_DEFINED <- paste(
    "O2 series inputs and outputs are not defined yet",
    "(ticket st-seahtrue-o2-auc-method-phzb)"
)

.compute_rates <- function(plate, id, inputs, plate_inputs,
                           registry, resolve_fun) {
    method <- .lookup_rate_method(id, registry)
    if ("o2-series" %in% method$output_kinds ||
        "o2-series" %in% names(method$input_methods)) {
        stop(id, ": ", .O2_SERIES_NOT_DEFINED, call. = FALSE)
    }
    plate_id <- .plate_id_of(plate)

    if (!is.list(inputs) || inherits(inputs, "data.frame")) {
        stop("inputs must be a named list of rate tables", call. = FALSE)
    }
    want <- names(method$input_methods)
    have <- names(inputs)
    if (length(inputs) > 0L && (is.null(have) || any(have == "") ||
                                anyDuplicated(have))) {
        stop("inputs must be named, once each", call. = FALSE)
    }
    if (!setequal(have, want) || length(inputs) != length(want)) {
        stop(id, " needs exactly the inputs: ",
             if (length(want)) paste(want, collapse = ", ") else "none",
             call. = FALSE)
    }
    for (k in want) {
        src <- .lookup_rate_method(method$input_methods[[k]], registry)
        .check_rate_table(inputs[[k]], plate_id, src)
        if (!(k %in% inputs[[k]]$rate_kind)) {
            stop("input ", k, " from ", method_id(src), " has no ", k,
                 " rates", call. = FALSE)
        }
    }

    if (!is.list(plate_inputs) || inherits(plate_inputs, "data.frame")) {
        stop("plate inputs must be a named list", call. = FALSE)
    }
    want_p <- method$plate_inputs
    have_p <- names(plate_inputs)
    if (length(plate_inputs) > 0L && (is.null(have_p) || any(have_p == "") ||
                                      anyDuplicated(have_p))) {
        stop("plate inputs must be named, once each", call. = FALSE)
    }
    if (!setequal(have_p, want_p) || length(plate_inputs) != length(want_p)) {
        stop(id, " needs exactly the declared plate inputs: ",
             if (length(want_p)) paste(want_p, collapse = ", ") else "none",
             call. = FALSE)
    }

    fun <- resolve_fun(method$fun)
    result <- fun(plate, method, inputs, plate_inputs)

    if (!is.list(result) || inherits(result, "data.frame") ||
        !identical(sort(names(result)), c("fitted_values", "rates"))) {
        stop(id, ": the method function must return ",
             "list(rates, fitted_values)", call. = FALSE)
    }
    .check_rate_table(result$rates, plate_id, method)
    if ("fitted-values" %in% method$output_kinds) {
        .check_fitted_values_table(result$fitted_values, plate_id, method)
    } else if (!is.null(result$fitted_values)) {
        stop(id, ": the method outputs no fitted values", call. = FALSE)
    }
    list(rates = result$rates, fitted_values = result$fitted_values)
}

.plate_id_of <- function(plate) {
    if (!is.data.frame(plate) || nrow(plate) != 1L ||
        !("plate_id" %in% names(plate))) {
        stop("plate must be one plate: a one-row data frame with a plate_id",
             call. = FALSE)
    }
    plate_id <- plate$plate_id[[1]]
    if (!is.character(plate_id) || length(plate_id) != 1L || is.na(plate_id) ||
        !grepl(PLATE_ID_PATTERN, plate_id)) {
        stop("plate_id must be the lowercase sha256 of the plate's ",
             "instrument file (a plate dataset)", call. = FALSE)
    }
    plate_id
}

# A method function is only ever an exported seahtrue function.
.resolve_method_function <- function(name) {
    if (!is.character(name) || length(name) != 1L ||
        !(name %in% getNamespaceExports("seahtrue")) ||
        name %in% XLSX_READER_FUNCTIONS) {
        stop("method function is not an exported seahtrue function",
             call. = FALSE)
    }
    getExportedValue("seahtrue", name)
}
