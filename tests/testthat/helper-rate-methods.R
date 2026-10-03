# Fixtures for the rate calculation method tests (work item
# st-seahtrue-rate-calculation-method-value-object-method-5j2b).
#
# The expected canonical JSON strings and sha256 digests below were computed
# independently with Python 3: json.dumps(value, sort_keys=True,
# separators=(",", ":"), ensure_ascii=False) and hashlib.sha256 over its UTF-8
# bytes. For this value domain (ASCII keys, strings and integers only) that
# output equals RFC 8785.

fixture_method_a <- function() {
    rate_method(
        name = "fixture-a", version = 1, family = "tick-rates",
        fun = "fixture_fun", output_kinds = c("ocr", "ecar"),
        parameters = list(method_parameter("tau_w", "747", "s"),
                          method_parameter("Alpha", "0.7574012", "")),
        corrections = c("background-subtraction", "tick-selection")
    )
}

fixture_method_b <- function() {
    rate_method(
        name = "fixture-b", version = 2, family = "atp-production",
        fun = "fixture_fun",
        output_kinds = c("oxidative-atp-production-rate",
                         "glycolytic-atp-production-rate", "fitted-values"),
        parameters = list(method_parameter("q", "1e-3", "a\"b\\c"),
                          method_parameter("divisor", "20", "\u00b5g")),
        input_methods = list(ocr = "fixture-a@1", ecar = "fixture-a@1"),
        plate_inputs = "cell-count"
    )
}

FIXTURE_A_JSON <- paste0(
    '{"corrections":["background-subtraction","tick-selection"],',
    '"family":"tick-rates","fun":"fixture_fun","input_methods":{},',
    '"instrument_models":["XFe96"],"name":"fixture-a",',
    '"output_kinds":["ecar","ocr"],"parameters":[{"name":"Alpha","unit":"",',
    '"value":"0.7574012"},{"name":"tau_w","unit":"s","value":"747"}],',
    '"plate_inputs":[],"version":1}'
)
FIXTURE_A_SHA256 <-
    "78644b5a2d30021fecccd6bf86de86f12a7842b5bfad52e89aebc33dbaa567a5"

FIXTURE_B_JSON <- paste0(
    '{"corrections":[],"family":"atp-production","fun":"fixture_fun",',
    '"input_methods":{"ecar":"fixture-a@1","ocr":"fixture-a@1"},',
    '"instrument_models":["XFe96"],"name":"fixture-b",',
    '"output_kinds":["fitted-values","glycolytic-atp-production-rate",',
    '"oxidative-atp-production-rate"],"parameters":[{"name":"divisor",',
    '"unit":"\u00b5g","value":"20"},{"name":"q","unit":"a\\"b\\\\c",',
    '"value":"1e-3"}],"plate_inputs":["cell-count"],"version":2}'
)
FIXTURE_B_SHA256 <-
    "1f07abb43e16e76f6c4c2aa1f0b2d06b5567a7cdeb14427611794c0428d06384"

FIXTURE_PLATE_ID <- strrep("ab", 32)

fixture_plate <- function(plate_id = FIXTURE_PLATE_ID) {
    tibble::tibble(plate_id = plate_id)
}

fixture_rates <- function(kinds = c("ocr", "ecar")) {
    g <- expand.grid(well = c("A01", "B02"), measurement = 1:2,
                     rate_kind = kinds, stringsAsFactors = FALSE)
    g$value <- seq_len(nrow(g)) * 1.5
    g
}

# A registry of the two fixture methods, checked with "fixture_fun" counted
# as exported.
fixture_registry <- function(methods = list(fixture_method_a(),
                                            fixture_method_b())) {
    .check_method_registry(methods, exports = "fixture_fun")
}
