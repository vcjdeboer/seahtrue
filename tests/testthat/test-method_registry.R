test_that("the shipped registry is empty and checks", {
    reg <- method_registry()
    expect_type(reg, "list")
    expect_length(reg, 0L)
})

# Release condition: no seahtrue tag carries a non-empty registry until the
# recompute test (each released method recomputed on a public plate against
# the previous tag) exists. The first method's work item
# (st-seahtrue-compute-ocr-ecar-o2-ph-ticks-wxjc) replaces this test with it.
test_that("the registry stays empty until the recompute test exists", {
    expect_length(.method_registry_entries(), 0L)
})

test_that("inst/extdata/method_registry.json equals the registry, byte for byte (R7)", {
    path <- system.file("extdata", "method_registry.json", package = "seahtrue")
    expect_true(nzchar(path))
    shipped <- readBin(path, "raw", file.info(path)$size)
    expect_identical(shipped, charToRaw(enc2utf8(.method_registry_json())))
})

test_that("the registry JSON of a non-empty registry is canonical", {
    reg <- fixture_registry()
    json <- .method_registry_json(reg)
    expect_identical(
        json,
        enc2utf8(paste0(
            "[{\"digest\":\"", FIXTURE_A_SHA256, "\",\"id\":\"fixture-a@1\",",
            "\"method\":", FIXTURE_A_JSON, "},",
            "{\"digest\":\"", FIXTURE_B_SHA256, "\",\"id\":\"fixture-b@2\",",
            "\"method\":", FIXTURE_B_JSON, "}]\n"
        ))
    )
})

test_that("the registry is keyed and sorted by method id", {
    reg <- fixture_registry(list(fixture_method_b(), fixture_method_a()))
    expect_identical(names(reg), c("fixture-a@1", "fixture-b@2"))
})

test_that("R1: a method id registered twice is refused", {
    expect_error(fixture_registry(list(fixture_method_a(), fixture_method_a())),
                 "more than once")
})

test_that("R2: input methods must be registered and output what they feed", {
    expect_error(fixture_registry(list(fixture_method_b())), "not registered")
    a_ocr_only <- rate_method("fixture-a", 1, "tick-rates", "fixture_fun",
                              output_kinds = "ocr")
    expect_error(fixture_registry(list(a_ocr_only, fixture_method_b())),
                 "does not output ecar")
})

test_that("R3: a cycle in the input graph is refused", {
    x <- rate_method("x", 1, "proton-efflux", "fixture_fun", "per",
                     input_methods = list(ocr = "y@1"))
    y <- rate_method("y", 1, "tick-rates", "fixture_fun", "ocr",
                     input_methods = list(per = "x@1"))
    expect_error(.check_method_registry(list(x, y), exports = "fixture_fun"),
                 "cycle")
    self <- rate_method("s", 1, "tick-rates", "fixture_fun", "ocr",
                        input_methods = list(ocr = "s@1"))
    expect_error(.check_method_registry(list(self), exports = "fixture_fun"),
                 "cycle")
})

test_that("R4: an O2 series input only feeds a tick-rates method", {
    o2 <- rate_method("o2", 1, "o2-correction", "fixture_fun", "o2-series")
    m2 <- rate_method("m2", 1, "tick-rates", "fixture_fun", "ocr",
                      input_methods = list(`o2-series` = "o2@1"))
    reg <- .check_method_registry(list(o2, m2), exports = "fixture_fun")
    expect_named(reg, c("m2@1", "o2@1"))
    expect_error(rate_method("p", 1, "proton-efflux", "fixture_fun", "per",
                             input_methods = list(`o2-series` = "o2@1")),
                 "only by a tick-rates method")
})

test_that("R5: a method function must be exported and never the xlsx reader", {
    expect_error(.check_method_registry(list(fixture_method_a()),
                                        exports = character()),
                 "not an exported seahtrue function")
    expect_error(.check_method_registry(
        list(rate_method("m", 1, "tick-rates", "print", "ocr"))),
        "not an exported seahtrue function")
    expect_error(.check_method_registry(
        list(rate_method("m", 1, "tick-rates", ".method_registry_json", "ocr"))),
        "method function name")
    bad <- fixture_method_a()
    bad$fun <- "revive_xfplate"
    expect_error(.check_method_registry(list(bad),
                                        exports = c("revive_xfplate")),
                 "Rate sheet is never a method")
})

test_that("lookup_rate_method matches exactly and quotes only well-formed ids", {
    reg <- fixture_registry()
    expect_identical(method_id(.lookup_rate_method("fixture-a@1", reg)),
                     "fixture-a@1")
    expect_error(.lookup_rate_method("fixture-a@", reg), "not a method id")
    expect_error(.lookup_rate_method("fixture-a", reg), "not a method id")
    expect_error(.lookup_rate_method("fixture-a@11", reg),
                 "no rate calculation method is registered as 'fixture-a@11'")
    err <- tryCatch(.lookup_rate_method("system('x')", reg),
                    error = function(e) conditionMessage(e))
    expect_false(grepl("system", err, fixed = TRUE))
    expect_error(.lookup_rate_method(c("a@1", "b@1"), reg), "one string")
    expect_error(.lookup_rate_method(NA_character_, reg), "one string")
    expect_error(lookup_rate_method("anything@1"), "no rate calculation method")
})

# ---- R6: append-only against the released registry ---------------------

released_path <- function() {
    system.file("extdata", "method_registry_released.tsv", package = "seahtrue")
}

test_that("R6: every released method id is still registered, unchanged", {
    rel <- .read_released_registry(released_path())
    current <- .method_registry_digests()
    expect_identical(.registry_append_only_problems(rel$digests, current),
                     character())
})

test_that("R6: a version newer than the released record needs a refreshed record", {
    rel <- .read_released_registry(released_path())
    current <- .method_registry_digests()
    if (utils::packageVersion("seahtrue") > package_version(rel$version)) {
        expect_identical(sort(names(current)), sort(names(rel$digests)))
        expect_identical(.registry_append_only_problems(current, rel$digests),
                         character())
    } else {
        expect_true(utils::packageVersion("seahtrue") ==
                        package_version(rel$version))
    }
})

test_that("R6: a changed digest or a removed id under a released id fails", {
    reg <- fixture_registry()
    released <- .method_registry_digests(reg)
    expect_identical(.registry_append_only_problems(released, released),
                     character())

    changed <- fixture_method_a()
    changed$parameters[[1]] <- method_parameter("tau_w", "748", "s")
    reg2 <- fixture_registry(list(changed, fixture_method_b()))
    expect_match(.registry_append_only_problems(released,
                                                .method_registry_digests(reg2)),
                 "fixture-a@1 was released and its method digest changed")

    reg3 <- fixture_registry(list(fixture_method_a()))
    expect_match(.registry_append_only_problems(released,
                                                .method_registry_digests(reg3)),
                 "fixture-b@2 was released and is gone")

    newer <- rate_method("fixture-a", 2, "tick-rates", "fixture_fun", "ocr")
    reg4 <- fixture_registry(list(fixture_method_a(), fixture_method_b(), newer))
    expect_identical(.registry_append_only_problems(
        released, .method_registry_digests(reg4)), character())
})

test_that("the released registry file format is checked", {
    tmp <- tempfile(fileext = ".tsv")
    writeLines(c("# seahtrue_version: 1.7.2",
                 paste0("fixture-a@1\t", FIXTURE_A_SHA256)), tmp)
    rel <- .read_released_registry(tmp)
    expect_identical(rel$version, "1.7.2")
    expect_identical(rel$digests, c(`fixture-a@1` = FIXTURE_A_SHA256))
    writeLines(c("fixture-a@1\tabc"), tmp)
    expect_error(.read_released_registry(tmp), "must start with")
    writeLines(c("# seahtrue_version: 1.7.2", "fixture-a@1\tabc"), tmp)
    expect_error(.read_released_registry(tmp), "malformed")
})
