# Rate calculation method: the value object, its method id and method digest.
#
# Work item st-seahtrue-rate-calculation-method-value-object-method-5j2b
# (swamp_seahtrue factory). Design: swamp_seahtrue
# packages/st-engine/rate-methods.md, section 4.

# The method families (GLOSSARY: method family).
RATE_METHOD_FAMILIES <- c("tick-rates", "o2-correction", "proton-efflux",
                          "atp-production")

# Rate kinds and the unit every rate of that kind carries.
RATE_KIND_UNITS <- c(
    "ocr" = "pmol/min",
    "ecar" = "mpH/min",
    "per" = "pmol H+/min",
    "mito-per" = "pmol H+/min",
    "glyco-per" = "pmol H+/min",
    "co2-corrected-ecar" = "mpH/min",
    "glycolytic-atp-production-rate" = "pmol ATP/min/\u00b5g",
    "oxidative-atp-production-rate" = "pmol ATP/min/\u00b5g"
)
RATE_KINDS <- names(RATE_KIND_UNITS)

# Output kinds: a rate kind, an O2 series, or fitted values.
OUTPUT_KINDS <- c(RATE_KINDS, "o2-series", "fitted-values")

# What an input method may be keyed by: a rate kind, or an O2 series (C2).
INPUT_KEYS <- c(RATE_KINDS, "o2-series")

# Plate dataset schema v1 names no instrument model: XFe96 only until it does.
INSTRUMENT_MODELS <- c("XFe96")

# The xlsx reader reads Wave's Rate sheet; it is never a method function.
XLSX_READER_FUNCTIONS <- c("revive_xfplate", "glue_xfplates")

METHOD_ID_PATTERN <- "^[a-z0-9-]+@[0-9]+$"
METHOD_ID_MAX_CHARS <- 80L
METHOD_NAME_PATTERN <- "^[a-z0-9-]+$"
METHOD_NAME_MAX_CHARS <- 64L
TOKEN_PATTERN <- "^[a-z0-9-]+$"
PARAMETER_NAME_PATTERN <- "^[A-Za-z][A-Za-z0-9_.]*$"
PARAMETER_VALUE_PATTERN <- "^-?[0-9]+(\\.[0-9]+)?([eE][-+]?[0-9]+)?$"
FUNCTION_NAME_PATTERN <- "^[A-Za-z][A-Za-z0-9_.]*$"

#' A method parameter of a rate calculation method
#'
#' A named constant with a value and unit inside a rate calculation method.
#' The value is a decimal string exactly as written in its source (for
#' example \code{"0.7574012"}), so the method digest never depends on how a
#' floating-point number is printed. A method function converts it with
#' \code{as.numeric()}.
#'
#' @param name The parameter's name: a letter, then letters, digits, \code{_}
#'   or \code{.}.
#' @param value The value as a decimal string (optionally with an exponent).
#' @param unit The unit, a string of at most 64 characters (\code{""} when
#'   the value has none).
#'
#' @return A list of class \code{seahtrue_method_parameter}.
#' @export
#' @examples
#' method_parameter("co2_contribution_factor", "0.61", "")
method_parameter <- function(name, value, unit) {
    .check_string(name, "parameter name", PARAMETER_NAME_PATTERN, 64L)
    .check_string(value, "parameter value", PARAMETER_VALUE_PATTERN, 64L)
    .check_free_text(unit, "parameter unit", 64L)
    structure(list(name = name, value = value, unit = unit),
              class = "seahtrue_method_parameter")
}

#' A rate calculation method
#'
#' Builds and checks the value of a rate calculation method: a named,
#' versioned way of computing rates from the plate dataset, any input methods
#' and any declared plate inputs. Methods are registered in seahtrue's method
#' registry (\code{\link{method_registry}}); a method value is never changed
#' once its method id has been released.
#'
#' @param name The method's name, \code{^[a-z0-9-]+$}, at most 64 characters.
#' @param version A positive whole number.
#' @param family The method family: \code{"tick-rates"},
#'   \code{"o2-correction"}, \code{"proton-efflux"} or
#'   \code{"atp-production"}.
#' @param fun The name of the exported seahtrue function that computes the
#'   method (the method function). The registry checks that it is exported;
#'   the xlsx reader is never a method function.
#' @param output_kinds What the method outputs: rate kinds (\code{"ocr"},
#'   \code{"ecar"}, \code{"per"}, \code{"mito-per"}, \code{"glyco-per"},
#'   \code{"co2-corrected-ecar"}, \code{"glycolytic-atp-production-rate"},
#'   \code{"oxidative-atp-production-rate"}), \code{"o2-series"} (an O2
#'   correction only) or \code{"fitted-values"}.
#' @param parameters A list of \code{\link{method_parameter}} values, unique
#'   by name.
#' @param corrections The ordered steps inside the method (for example
#'   \code{"background-subtraction"}), each \code{^[a-z0-9-]+$}.
#' @param input_methods A named list: for each rate kind or \code{"o2-series"}
#'   the method consumes, the method id producing it. An \code{"o2-series"}
#'   input is allowed only on a tick-rates method.
#' @param plate_inputs Declared plate inputs the method needs (for example
#'   \code{"cell-count"}), each \code{^[a-z0-9-]+$}.
#' @param instrument_models The instrument models the method is valid for;
#'   only \code{"XFe96"} while the plate dataset names no instrument model.
#'
#' @return A list of class \code{seahtrue_rate_method}.
#' @seealso \code{\link{method_id}}, \code{\link{method_digest}},
#'   \code{\link{method_registry}}, \code{\link{compute_rates}}
#' @export
rate_method <- function(name, version, family, fun, output_kinds,
                        parameters = list(), corrections = character(),
                        input_methods = list(), plate_inputs = character(),
                        instrument_models = "XFe96") {
    if (is.numeric(version) && length(version) == 1L && !is.na(version) &&
        is.finite(version) && version == round(version) &&
        abs(version) <= .Machine$integer.max) {
        version <- as.integer(version)
    }
    m <- structure(
        list(
            name = name,
            version = version,
            family = family,
            fun = fun,
            output_kinds = output_kinds,
            parameters = parameters,
            corrections = corrections,
            input_methods = input_methods,
            plate_inputs = plate_inputs,
            instrument_models = instrument_models
        ),
        class = "seahtrue_rate_method"
    )
    .check_rate_method(m)
    m
}

#' The method id of a rate calculation method
#'
#' @param method A rate calculation method (\code{\link{rate_method}}).
#' @return \code{"name@version"}, matching \code{^[a-z0-9-]+@[0-9]+$}.
#' @export
method_id <- function(method) {
    .check_rate_method(method)
    paste0(method$name, "@", method$version)
}

#' The canonical form of a rate calculation method
#'
#' The method value as RFC 8785 (JSON Canonicalization Scheme) JSON: an
#' object with exactly the keys \code{corrections}, \code{family},
#' \code{fun}, \code{input_methods}, \code{instrument_models}, \code{name},
#' \code{output_kinds}, \code{parameters}, \code{plate_inputs} and
#' \code{version}, in that (sorted) order, without whitespace. The sets
#' \code{output_kinds}, \code{plate_inputs} and \code{instrument_models} are
#' sorted, and \code{parameters} (objects with keys \code{name}, \code{unit},
#' \code{value}) is sorted by name; \code{corrections} keeps its order. The
#' seahtrue tag is not part of it. For example:
#'
#' \preformatted{{"corrections":[],"family":"tick-rates","fun":"f","input_methods":{},"instrument_models":["XFe96"],"name":"a","output_kinds":["ecar","ocr"],"parameters":[],"plate_inputs":[],"version":1}}
#'
#' @inheritParams method_id
#' @return A single string (UTF-8).
#' @export
method_canonical_json <- function(method) {
    .check_rate_method(method)
    params <- method$parameters
    if (length(params) > 0L) {
        pnames <- vapply(params, function(p) p$name, character(1))
        params <- params[order(pnames, method = "radix")]
    }
    param_json <- vapply(params, function(p) {
        .json_object(c(name = .json_string(p$name),
                       unit = .json_string(p$unit),
                       value = .json_string(p$value)))
    }, character(1))
    inputs <- method$input_methods
    input_json <- vapply(inputs, .json_string, character(1))
    names(input_json) <- names(inputs)
    .json_object(c(
        corrections = .json_array(vapply(method$corrections, .json_string,
                                         character(1))),
        family = .json_string(method$family),
        fun = .json_string(method$fun),
        input_methods = .json_object(input_json),
        instrument_models = .json_string_set(method$instrument_models),
        name = .json_string(method$name),
        output_kinds = .json_string_set(method$output_kinds),
        parameters = .json_array(param_json),
        plate_inputs = .json_string_set(method$plate_inputs),
        version = as.character(method$version)
    ))
}

#' The method digest of a rate calculation method
#'
#' @inheritParams method_id
#' @return The lowercase hexadecimal sha256 of
#'   \code{\link{method_canonical_json}(method)}, encoded as UTF-8.
#' @export
method_digest <- function(method) {
    .sha256_utf8(method_canonical_json(method))
}

#' @export
print.seahtrue_rate_method <- function(x, ...) {
    cat("<rate calculation method ", method_id(x), ">\n", sep = "")
    cat("  family:       ", x$family, "\n", sep = "")
    cat("  function:     ", x$fun, "\n", sep = "")
    cat("  output kinds: ", paste(x$output_kinds, collapse = ", "), "\n",
        sep = "")
    if (length(x$input_methods) > 0L) {
        cat("  inputs:       ",
            paste(names(x$input_methods), unlist(x$input_methods),
                  sep = " <- ", collapse = ", "), "\n", sep = "")
    }
    invisible(x)
}

# ---- checks ---------------------------------------------------------------

.check_rate_method <- function(m) {
    if (!inherits(m, "seahtrue_rate_method") || !is.list(m)) {
        stop("not a rate calculation method (use rate_method())", call. = FALSE)
    }
    expected <- c("name", "version", "family", "fun", "output_kinds",
                  "parameters", "corrections", "input_methods",
                  "plate_inputs", "instrument_models")
    if (!identical(names(m), expected)) {
        stop("a rate calculation method has exactly the fields ",
             paste(expected, collapse = ", "), call. = FALSE)
    }
    .check_string(m$name, "method name", METHOD_NAME_PATTERN,
                  METHOD_NAME_MAX_CHARS)
    if (!is.integer(m$version) || length(m$version) != 1L ||
        is.na(m$version) || m$version < 1L) {
        stop("method version must be one positive whole number", call. = FALSE)
    }
    if (nchar(paste0(m$name, "@", m$version)) > METHOD_ID_MAX_CHARS) {
        stop("method id is longer than ", METHOD_ID_MAX_CHARS, " characters",
             call. = FALSE)
    }
    .check_choice(m$family, "method family", RATE_METHOD_FAMILIES)
    .check_string(m$fun, "method function name", FUNCTION_NAME_PATTERN, 64L)
    if (m$fun %in% XLSX_READER_FUNCTIONS) {
        stop("the xlsx reader (", m$fun, ") is never a method function: ",
             "the Rate sheet is never a method", call. = FALSE)
    }
    .check_set(m$output_kinds, "output kinds", OUTPUT_KINDS, min = 1L)
    if ("o2-series" %in% m$output_kinds && m$family != "o2-correction") {
        stop("only an O2 correction method outputs an O2 series",
             call. = FALSE)
    }
    if (!is.list(m$parameters) ||
        !all(vapply(m$parameters, inherits, logical(1),
                    "seahtrue_method_parameter"))) {
        stop("parameters must be a list of method_parameter() values",
             call. = FALSE)
    }
    for (p in m$parameters) {
        method_parameter(p$name, p$value, p$unit)
    }
    pnames <- vapply(m$parameters, function(p) p$name, character(1))
    if (anyDuplicated(pnames)) {
        stop("parameter names must be unique", call. = FALSE)
    }
    if (!is.character(m$corrections) || anyNA(m$corrections)) {
        stop("corrections must be a character vector", call. = FALSE)
    }
    for (s in m$corrections) .check_string(s, "correction", TOKEN_PATTERN, 64L)
    if (anyDuplicated(m$corrections)) {
        stop("a correction appears only once in a method", call. = FALSE)
    }
    .check_input_methods(m$input_methods, m$family)
    if (!is.character(m$plate_inputs) || anyNA(m$plate_inputs)) {
        stop("plate inputs must be a character vector", call. = FALSE)
    }
    for (s in m$plate_inputs) .check_string(s, "plate input", TOKEN_PATTERN, 64L)
    if (anyDuplicated(m$plate_inputs)) {
        stop("plate inputs must be unique", call. = FALSE)
    }
    .check_set(m$instrument_models, "instrument models", INSTRUMENT_MODELS,
               min = 1L)
    invisible(TRUE)
}

.check_input_methods <- function(x, family) {
    if (!is.list(x)) {
        stop("input methods must be a named list", call. = FALSE)
    }
    if (length(x) == 0L) return(invisible(TRUE))
    keys <- names(x)
    if (is.null(keys) || anyNA(keys) || any(keys == "") || anyDuplicated(keys)) {
        stop("input methods must be named, once each, by the rate kind or ",
             "O2 series they provide", call. = FALSE)
    }
    bad <- setdiff(keys, INPUT_KEYS)
    if (length(bad) > 0L) {
        stop("an input method is keyed by a rate kind or o2-series, not: ",
             paste(bad, collapse = ", "), call. = FALSE)
    }
    if ("o2-series" %in% keys && family != "tick-rates") {
        stop("an O2 series is consumed only by a tick-rates method ",
             "(as its O2 input)", call. = FALSE)
    }
    for (k in keys) {
        id <- x[[k]]
        if (!.is_method_id(id)) {
            stop("input method for ", k, " is not a method id (name@version)",
                 call. = FALSE)
        }
    }
    invisible(TRUE)
}

.is_method_id <- function(id) {
    is.character(id) && length(id) == 1L && !is.na(id) &&
        nchar(id) <= METHOD_ID_MAX_CHARS && grepl(METHOD_ID_PATTERN, id)
}

.check_string <- function(x, what, pattern, max_chars) {
    if (!is.character(x) || length(x) != 1L || is.na(x)) {
        stop(what, " must be one string", call. = FALSE)
    }
    if (nchar(x, type = "chars", allowNA = TRUE) > max_chars ||
        !grepl(pattern, x)) {
        stop(what, " does not match ", pattern, " (at most ", max_chars,
             " characters)", call. = FALSE)
    }
    invisible(TRUE)
}

.check_free_text <- function(x, what, max_chars) {
    if (!is.character(x) || length(x) != 1L || is.na(x)) {
        stop(what, " must be one string", call. = FALSE)
    }
    if (!validUTF8(x)) stop(what, " is not valid UTF-8", call. = FALSE)
    x <- enc2utf8(x)
    if (nchar(x, type = "chars") > max_chars) {
        stop(what, " is longer than ", max_chars, " characters", call. = FALSE)
    }
    if (any(utf8ToInt(x) < 32L | utf8ToInt(x) == 127L)) {
        stop(what, " contains a control character", call. = FALSE)
    }
    invisible(TRUE)
}

.check_choice <- function(x, what, choices) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !(x %in% choices)) {
        stop(what, " must be one of ", paste(choices, collapse = ", "),
             call. = FALSE)
    }
    invisible(TRUE)
}

.check_set <- function(x, what, choices, min = 0L) {
    if (!is.character(x) || anyNA(x) || length(x) < min) {
        stop(what, " must be a character vector with at least ", min,
             " element(s)", call. = FALSE)
    }
    if (anyDuplicated(x)) stop(what, " must not repeat a value", call. = FALSE)
    bad <- setdiff(x, choices)
    if (length(bad) > 0L) {
        stop(what, " must be among ", paste(choices, collapse = ", "),
             call. = FALSE)
    }
    invisible(TRUE)
}

# ---- RFC 8785 (JCS) serialisation, over strings, integers, arrays, objects --

# A JSON string as ECMAScript's JSON.stringify writes it (RFC 8785 3.2.2.2):
# '"' and '\' escaped, U+0008/09/0A/0C/0D as \b \t \n \f \r, other code
# points below U+0020 as \u00xx (lowercase hex), everything else as is.
.json_string <- function(s) {
    s <- enc2utf8(s)
    cps <- utf8ToInt(s)
    out <- vapply(cps, function(cp) {
        if (cp == 0x22L) return("\\\"")
        if (cp == 0x5CL) return("\\\\")
        if (cp == 0x08L) return("\\b")
        if (cp == 0x09L) return("\\t")
        if (cp == 0x0AL) return("\\n")
        if (cp == 0x0CL) return("\\f")
        if (cp == 0x0DL) return("\\r")
        if (cp < 0x20L) return(sprintf("\\u%04x", cp))
        intToUtf8(cp)
    }, character(1))
    enc2utf8(paste0("\"", paste(out, collapse = ""), "\""))
}

.json_array <- function(values) {
    paste0("[", paste(values, collapse = ","), "]")
}

# Members sorted by key; keys here are ASCII, so radix (code unit) order is
# RFC 8785's UTF-16 order.
.json_object <- function(members) {
    if (length(members) == 0L) return("{}")
    keys <- names(members)
    o <- order(keys, method = "radix")
    paste0("{", paste0(vapply(keys[o], .json_string, character(1)), ":",
                       members[o], collapse = ","), "}")
}

.json_string_set <- function(x) {
    x <- sort(x, method = "radix")
    .json_array(vapply(x, .json_string, character(1)))
}

.sha256_utf8 <- function(s) {
    digest::digest(enc2utf8(s), algo = "sha256", serialize = FALSE)
}
