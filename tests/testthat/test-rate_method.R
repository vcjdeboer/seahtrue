test_that("the canonical form and method digest match independent RFC 8785 vectors", {
    a <- fixture_method_a()
    b <- fixture_method_b()
    expect_identical(method_canonical_json(a), FIXTURE_A_JSON)
    expect_identical(method_digest(a), FIXTURE_A_SHA256)
    expect_identical(enc2utf8(method_canonical_json(b)), enc2utf8(FIXTURE_B_JSON))
    expect_identical(method_digest(b), FIXTURE_B_SHA256)
})

test_that("method_id is name@version", {
    expect_identical(method_id(fixture_method_a()), "fixture-a@1")
    expect_identical(method_id(fixture_method_b()), "fixture-b@2")
})

test_that("JSON strings are escaped as RFC 8785 says", {
    expect_identical(.json_string("a\"b\\c"), "\"a\\\"b\\\\c\"")
    expect_identical(.json_string("x\b\t\n\f\ry"), "\"x\\b\\t\\n\\f\\ry\"")
    expect_identical(.json_string("\u0001\u001f"), "\"\\u0001\\u001f\"")
    expect_identical(.json_string(""), "\"\"")
    expect_identical(.json_string("\u00b5\u20ac"), enc2utf8("\"\u00b5\u20ac\""))
})

test_that("the digest ignores the order of set-like fields only", {
    a <- fixture_method_a()
    a2 <- rate_method(
        name = "fixture-a", version = 1, family = "tick-rates",
        fun = "fixture_fun", output_kinds = c("ecar", "ocr"),
        parameters = list(method_parameter("Alpha", "0.7574012", ""),
                          method_parameter("tau_w", "747", "s")),
        corrections = c("background-subtraction", "tick-selection")
    )
    expect_identical(method_digest(a2), method_digest(a))

    reordered <- a
    reordered$corrections <- rev(a$corrections)
    expect_false(identical(method_digest(reordered), method_digest(a)))

    b <- fixture_method_b()
    b2 <- b
    b2$input_methods$ocr <- "fixture-a@2"
    expect_false(identical(method_digest(b2), method_digest(b)))

    p <- a
    p$parameters[[1]] <- method_parameter("tau_w", "747.0", "s")
    expect_false(identical(method_digest(p), method_digest(a)))
})

test_that("rate_method refuses every malformed field", {
    ok <- list(name = "m", version = 1, family = "tick-rates", fun = "f",
               output_kinds = "ocr")
    mk <- function(...) {
        args <- utils::modifyList(ok, list(...))
        do.call(rate_method, args)
    }
    expect_s3_class(mk(), "seahtrue_rate_method")
    expect_error(mk(name = "Bad_Name"), "method name")
    expect_error(mk(name = strrep("a", 65)), "method name")
    expect_error(mk(version = 0), "positive whole number")
    expect_error(mk(version = 1.5), "positive whole number")
    expect_error(mk(version = "1"), "positive whole number")
    expect_error(mk(family = "normalisation"), "method family")
    expect_error(mk(fun = "x; y"), "method function name")
    expect_error(mk(fun = "revive_xfplate"), "Rate sheet is never a method")
    expect_error(mk(fun = "glue_xfplates"), "Rate sheet is never a method")
    expect_error(mk(output_kinds = character()), "output kinds")
    expect_error(mk(output_kinds = "OCR"), "output kinds")
    expect_error(mk(output_kinds = c("ocr", "ocr")), "repeat")
    expect_error(mk(output_kinds = "o2-series"), "only an O2 correction")
    expect_s3_class(mk(family = "o2-correction", output_kinds = "o2-series"),
                    "seahtrue_rate_method")
    expect_error(mk(parameters = list(list(name = "a", value = "1", unit = ""))),
                 "method_parameter")
    expect_error(mk(parameters = list(method_parameter("a", "1", ""),
                                      method_parameter("a", "2", ""))),
                 "unique")
    expect_error(mk(corrections = "Background"), "correction")
    expect_error(mk(corrections = c("x", "x")), "only once")
    expect_error(mk(input_methods = list("a@1")), "named")
    expect_error(mk(input_methods = list(`fitted-values` = "a@1")),
                 "rate kind or o2-series")
    expect_error(mk(input_methods = list(ocr = "a")), "not a method id")
    expect_error(mk(family = "proton-efflux",
                    input_methods = list(`o2-series` = "a@1")),
                 "only by a tick-rates method")
    expect_s3_class(mk(input_methods = list(`o2-series` = "a@1")),
                    "seahtrue_rate_method")
    expect_error(mk(plate_inputs = "Cell count"), "plate input")
    expect_error(mk(plate_inputs = c("cell-count", "cell-count")), "unique")
    expect_error(mk(instrument_models = "XFp"), "instrument models")
    expect_error(mk(instrument_models = character()), "instrument models")
    expect_error(mk(version = 10^16), "positive whole number")
    expect_s3_class(mk(name = strrep("a", 64), version = .Machine$integer.max),
                    "seahtrue_rate_method")
})

test_that("method_parameter refuses a non-decimal value and bad text", {
    expect_s3_class(method_parameter("a", "-1.5e+3", "s"),
                    "seahtrue_method_parameter")
    expect_error(method_parameter("a", 1.5, "s"), "one string")
    expect_error(method_parameter("a", "1,5", "s"), "parameter value")
    expect_error(method_parameter("a", "Inf", "s"), "parameter value")
    expect_error(method_parameter("_a", "1", "s"), "parameter name")
    expect_error(method_parameter("a", "1", "s\n"), "control character")
    expect_error(method_parameter("a", "1", strrep("s", 65)), "longer than")
})

test_that("a hand-altered method value is refused", {
    a <- fixture_method_a()
    a$extra <- 1
    expect_error(method_id(a), "exactly the fields")
    expect_error(method_id(list(name = "x")), "not a rate calculation method")
})
