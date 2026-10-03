# The method registry: seahtrue's append-only list of rate calculation
# methods, the only source of method ids.
#
# Work item st-seahtrue-rate-calculation-method-value-object-method-5j2b.
#
# How a method is registered (each child work item of the rate-methods
# design): add R/rate_methods_<family>.R with an internal function returning
# a list of rate_method() values, add one call of it below, and regenerate
# inst/extdata/method_registry.json with .write_method_registry_json().
# A released method id never changes its value: a change is a new version,
# and the old entry stays.
#
# Release conditions:
# - inst/extdata/method_registry_released.tsv records every method id a
#   seahtrue version has carried, with its method digest, and the version it
#   was recorded at. tests/testthat/test-method_registry.R fails when the
#   package version is newer than that record while the registry differs
#   from it, so a version bump must refresh the record.
# - No seahtrue tag may carry a non-empty registry until the recompute test
#   (each released method recomputed on a public plate against the previous
#   tag) exists; the test "the registry stays empty" holds this until the
#   first method's work item replaces it.

.method_registry_entries <- function() {
    c(
        list()
        # one line per family, e.g. .rate_methods_tick_rates(),
    )
}

#' The method registry
#'
#' Every registered rate calculation method, keyed by method id. The registry
#' is append-only: once a seahtrue version carries a method id, that id keeps
#' naming the same method value (the same method digest). The registry is
#' checked on every call:
#' \itemize{
#'   \item every method id is unique;
#'   \item every method function is an exported seahtrue function, never the
#'     xlsx reader;
#'   \item every input method is registered and outputs the rate kind or O2
#'     series it is consumed as;
#'   \item the input graph has no cycle.
#' }
#'
#' @return A named list of \code{\link{rate_method}} values, names being
#'   method ids, sorted by method id. Empty until the first method is
#'   registered.
#' @export
method_registry <- function() {
    .check_method_registry(.method_registry_entries())
}

#' Look up a rate calculation method by its method id
#'
#' Exact match only: the id is compared with the registered method ids, never
#' evaluated or partially matched.
#'
#' @param id A method id, \code{"name@version"}.
#' @return The \code{\link{rate_method}} registered under \code{id}; an error
#'   if there is none. The error quotes \code{id} only when it is a
#'   well-formed method id.
#' @export
lookup_rate_method <- function(id) {
    .lookup_rate_method(id, method_registry())
}

.lookup_rate_method <- function(id, registry) {
    if (!is.character(id) || length(id) != 1L || is.na(id)) {
        stop("a method id must be one string", call. = FALSE)
    }
    if (!.is_method_id(id)) {
        stop("not a method id: it must match ", METHOD_ID_PATTERN,
             " and be at most ", METHOD_ID_MAX_CHARS, " characters",
             call. = FALSE)
    }
    i <- match(id, names(registry))
    if (is.na(i)) {
        stop("no rate calculation method is registered as '", id, "'",
             call. = FALSE)
    }
    registry[[i]]
}

# Checks a list of methods as a registry (R1-R5) and returns it keyed and
# sorted by method id. `exports` is seahtrue's exported names; tests pass
# their own.
.check_method_registry <- function(methods,
                                   exports = getNamespaceExports("seahtrue")) {
    if (!is.list(methods)) stop("the registry is a list of methods", call. = FALSE)
    if (length(methods) == 0L) {
        return(structure(list(), names = character()))
    }
    for (m in methods) .check_rate_method(m)
    ids <- vapply(methods, method_id, character(1))
    dup <- unique(ids[duplicated(ids)])
    if (length(dup) > 0L) {
        stop("method id registered more than once: ",
             paste(dup, collapse = ", "), call. = FALSE)
    }
    names(methods) <- ids
    for (id in ids) {
        m <- methods[[id]]
        if (!(m$fun %in% exports)) {
            stop(id, ": method function ", m$fun,
                 " is not an exported seahtrue function", call. = FALSE)
        }
        for (k in names(m$input_methods)) {
            src <- m$input_methods[[k]]
            if (!(src %in% ids)) {
                stop(id, ": input method ", src, " (for ", k,
                     ") is not registered", call. = FALSE)
            }
            if (!(k %in% methods[[src]]$output_kinds)) {
                stop(id, ": input method ", src, " does not output ", k,
                     call. = FALSE)
            }
        }
    }
    .check_no_input_cycle(methods)
    methods[order(ids, method = "radix")]
}

.check_no_input_cycle <- function(methods) {
    state <- stats::setNames(integer(length(methods)), names(methods))
    visit <- function(id, path) {
        if (state[[id]] == 2L) return(invisible())
        if (state[[id]] == 1L) {
            stop("the input graph has a cycle: ",
                 paste(c(path, id), collapse = " -> "), call. = FALSE)
        }
        state[[id]] <<- 1L
        for (src in unlist(methods[[id]]$input_methods, use.names = FALSE)) {
            visit(src, c(path, id))
        }
        state[[id]] <<- 2L
        invisible()
    }
    for (id in names(methods)) visit(id, character())
    invisible(TRUE)
}

# ---- the registry's JSON copy and the released registry --------------------

# The registry as canonical JSON text (one array of {digest, id, method},
# sorted by id) plus a final newline; inst/extdata/method_registry.json is
# this text, byte for byte.
.method_registry_json <- function(registry = method_registry()) {
    entries <- vapply(names(registry), function(id) {
        m <- registry[[id]]
        .json_object(c(digest = .json_string(method_digest(m)),
                       id = .json_string(id),
                       method = method_canonical_json(m)))
    }, character(1))
    paste0(.json_array(unname(entries)), "\n")
}

.write_method_registry_json <- function(path = file.path("inst", "extdata",
                                                         "method_registry.json")) {
    con <- file(path, open = "wb")
    on.exit(close(con))
    writeBin(charToRaw(enc2utf8(.method_registry_json())), con)
    invisible(path)
}

# The registry as "id<TAB>digest" lines, the body of
# inst/extdata/method_registry_released.tsv.
.method_registry_digests <- function(registry = method_registry()) {
    stats::setNames(vapply(registry, method_digest, character(1)),
                    names(registry))
}

# Reads the released registry: its first line is
# "# seahtrue_version: <version>", then one "id<TAB>digest" line per method.
.read_released_registry <- function(path) {
    lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
    if (length(lines) < 1L || !grepl("^# seahtrue_version: [0-9.]+$", lines[1])) {
        stop("released registry must start with '# seahtrue_version: <version>'",
             call. = FALSE)
    }
    version <- sub("^# seahtrue_version: ", "", lines[1])
    body <- lines[-1]
    body <- body[nzchar(body)]
    parts <- strsplit(body, "\t", fixed = TRUE)
    ok <- vapply(parts, function(p) {
        length(p) == 2L && .is_method_id(p[1]) && grepl("^[0-9a-f]{64}$", p[2])
    }, logical(1))
    if (!all(ok)) stop("malformed line in the released registry", call. = FALSE)
    digests <- vapply(parts, `[`, character(1), 2L)
    names(digests) <- vapply(parts, `[`, character(1), 1L)
    if (anyDuplicated(names(digests))) {
        stop("a method id appears twice in the released registry", call. = FALSE)
    }
    list(version = version, digests = digests)
}

# Append-only (R6): every released method id is still registered with the
# same method digest. Returns the problems found (character(0) when none).
.registry_append_only_problems <- function(released, current) {
    problems <- character()
    for (id in names(released)) {
        if (!(id %in% names(current))) {
            problems <- c(problems, paste0(id, " was released and is gone"))
        } else if (!identical(unname(current[[id]]), unname(released[[id]]))) {
            problems <- c(problems,
                          paste0(id, " was released and its method digest changed"))
        }
    }
    problems
}
