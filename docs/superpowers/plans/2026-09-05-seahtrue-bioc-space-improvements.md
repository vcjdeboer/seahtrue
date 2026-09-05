# seahtrue Bioconductor-Readiness & Space-API Hardening — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Bring the bioenergetic-space/trajectory feature and the surrounding `seahtrue` R code up to a Bioconductor-shippable standard — validated inputs, a documented public API, clean `BiocCheck`, and real tests — so working scientists can install it from Bioconductor and run it on their own data.

**Architecture:** Three sequential phases. **Phase 1** = zero-behavior-change hygiene that clears `BiocCheck` NOTEs/WARNINGs and establishes a check baseline. **Phase 2** = hardening the space API (input validation, a documented column-name contract, a dedicated source file, naming cleanup). **Phase 3** = a real `testthat` suite for the exported functions. A short **Phase 4** covers release-prep hand-off (version reconciliation for the Bioconductor sync). Two items are **maintainer decision gates** and must be answered before their tasks run.

**Tech Stack:** R (≥4.4), Bioconductor toolchain (`BiocCheck`, `devtools`, `testthat` ≥3.0, `roxygen2`), tidyverse-family imports (dplyr, tidyr, purrr, ggplot2, ggrepel, cli, rlang).

**Spec:** This document. Design rationale lives in the **Background** and **Decision Gates** sections below; it was produced collaboratively from a full code audit of the `devel` branch (see Background). The plan argues from that audit — executors should read Background before Task 1.

## Global Constraints

- **Branch:** all work happens on `worktree-bioc-space-improvements`, based on `devel` (has the space feature). Do **not** target `main` or Bioconductor `upstream` directly.
- **Bioconductor size limit:** the source package must stay well under ~5 MB. **Do NOT add large data files** (`.xlsx`/`.rda`) to the package. Use only the already-shipped `inst/extdata/20191219_SciRep_PBMCs_donor_A.xlsx` for examples and tests.
- **No top-level executable code in `R/`.** Every `R/*.R` file is sourced at install time; only function/`globalVariables()`/`roxygen` definitions belong there.
- **No hard-coded local paths** anywhere in `R/`, tests, or bundled vignettes/qmd. Use `system.file("extdata", ...)`.
- **User-facing messages** use `cli::cli_*` (or `message()`), never `cat()`/`print()`.
- **NSE columns** must not trip "no visible binding": use the `.data` pronoun or register names via `utils::globalVariables()`.
- **API stability:** seahtrue is not yet on Bioconductor *with these functions*, so renaming exported args/columns is allowed **now** — but any rename of the `supply_index` column or exported arg names also affects **the book of seahtrue** (loads seahtrue via webR) and must be noted in `NEWS.md`.
- **Commit discipline:** conventional-commit messages; one commit per completed task step group as indicated. End commit messages with the attribution footer configured for this session.
- **Every task ends green:** `devtools::test()` passes and (from Phase 1 onward) `R CMD check` introduces no new WARNING/ERROR.

---

## Background (read before Task 1)

The audit of `devel` found:

- **The space feature is real and mostly complete.** `calculate_space()`, `plot_bioenergetic_space()`, `plot_bioenergetic_trajectory()` all live in `R/plot_xfplate.R` (lines ~812–1151), have complete roxygen with runnable examples, and the "SFI" (`supply_index`) metric is already shipped — it is **not** missing. The shipped `calculate_space()` is a superset of the `new_trajectory.qmd` prototype; the only prototype capability dropped is passing custom `atp_factors` (see Decision Gate B).
- **The core usability trap (highest-value fix):** `calculate_space()` renames its output columns to whatever the user passes in `param_set_ocr`/`param_set_ecar` (`dplyr::rename(!!!param_set_*)`). Its own downstream metrics reference *canonical* names (`init_ocr`, `fccp_ocr`, `amrot_ocr`, `om_ocr`, `mon_ocr`, `init_ecar`, `mon_ecar`, `om_ecar`), and both plot functions hard-require `basal_ocr, fccp_ocr, amrot_ocr, basal_ecar, fccp_ecar, amrot_ecar`. If a scientist names an injection differently, `calculate_space()` silently returns `NA` metrics and the plots later error with a bare `stop()`. This contract is undocumented.
- **No input validation** on `param_set_ocr`/`param_set_ecar` or on the `rate` tibble.
- **Bioconductor NOTEs:** pervasive "no visible binding for global variable" (bare-column NSE with no `utils::globalVariables()` anywhere except the `.data` pronoun in the two plot functions); `cat("\n")` at `R/master.R:122`; deprecated ggplot2 `size=` for lines; 105 lines >80 chars; several very long functions.
- **Build hygiene:** `new_trajectory.qmd` / `new_trajectory_test.qmd` in the repo root are **not** in `.Rbuildignore` (they carry a hard-coded personal data path and would be bundled). `R/workbench.R` is gitignored local scratch — it is **not** tracked and must never be `git add`ed into `R/`.
- **Tests:** only `tests/testthat/test-revive_xfplate.R` exists; **all 9 exported functions have zero direct tests**, including the entire space/trajectory API.

Exact exported signatures (from `R/plot_xfplate.R`, verified):

```r
calculate_space(rate, param_set_ocr, param_set_ecar,
                conversion_model = "mookerjee",
                ug_protein_scaling_factor = 20,
                OCR_var = "J_oxphos", ECAR_var = "J_glyco")   # -> tibble, one row per group

plot_bioenergetic_space(df, ecar_title=..., ocr_title=..., legend_title="Group",
                        palette=NULL, title=NULL)             # -> ggplot; requires
   # group, basal_ocr, fccp_ocr, basal_ecar, amrot_ecar

plot_bioenergetic_trajectory(df, palette=NULL, title="Metabolic J-space",
                        label_map=c("Baseline"="Basal","FCCP"="FCCP","AM/Rot"="AM/Rot")) # -> ggplot; requires
   # group, basal_ocr, fccp_ocr, amrot_ocr, basal_ecar, fccp_ecar, amrot_ecar
```

---

## Decision Gates (maintainer must answer before the flagged tasks)

**Gate A — `amrot_ecar` vs `max_ecar` (blocks Task 8).**
`plot_bioenergetic_space()` draws the metabolic-space rectangle **width** from `amrot_ecar` (`plot_xfplate.R:1003,1008`), but `calculate_space()` defines `max_ecar` (used by `supply_index`/`glyco_index_max`) from `mon_ecar` (or `om_ecar`) (`:898–904`). These are different injections. **Question:** is the plotted width intentionally the antimycin/rotenone ECAR, or should it use `max_ecar` like the metrics? Answer decides whether Task 8 is a doc-only clarification or a code fix.

**Gate B — custom `atp_factors` (blocks Task 11, optional).**
The prototype let users pass their own ATP conversion factors; the shipped code hard-codes two presets (`mookerjee`/`agilent`). **Question:** do target users need to supply custom factors? If yes, Task 11 adds an escape hatch; if no, Task 11 is dropped and the presets are simply documented.

**Recommended defaults if unanswered:** Gate A → treat as a **bug**, make the plot width consistent with `max_ecar`, and document (safest for correctness). Gate B → **drop** the escape hatch (YAGNI) and document the presets. Do not implement these defaults without an explicit "use the defaults" from the maintainer.

---

# Phase 1 — Hygiene & check baseline (no behavior change)

### Task 1: Exclude stray files from the build

**Files:**
- Modify: `.Rbuildignore`

**Interfaces:**
- Consumes: nothing.
- Produces: a build tarball free of the root `.qmd` prototypes.

- [ ] **Step 1: Add ignore rules**

Append to `.Rbuildignore`:

```
^new_trajectory\.qmd$
^new_trajectory_test\.qmd$
^docs/superpowers$
^\.claude$
```

- [ ] **Step 2: Verify they are excluded**

Run: `R CMD build . && tar -tzf seahtrue_*.tar.gz | grep -E 'new_trajectory|superpowers' || echo "OK: excluded"`
Expected: prints `OK: excluded` (grep finds nothing in the tarball). Delete the built tarball afterward.

- [ ] **Step 3: Confirm workbench.R is not tracked**

Run: `git ls-files R/workbench.R` → Expected: **empty output** (untracked/gitignored). If it prints a path, STOP — it must not be committed; remove from index with `git rm --cached R/workbench.R` and add `R/workbench.R` to `.gitignore`.

- [ ] **Step 4: Commit**

```bash
git add .Rbuildignore
git commit -m "build: exclude prototype qmd and tooling dirs from package build"
```

### Task 2: Replace `cat()` with cli in master.R

**Files:**
- Modify: `R/master.R:122`

**Interfaces:**
- Consumes: nothing. Produces: no API change.

- [ ] **Step 1: Inspect context**

Run: `sed -n '110,130p' R/master.R` to see the surrounding block (it already uses `cli::cli_inform` nearby).

- [ ] **Step 2: Replace the call**

Change the `cat("\n")` at line 122 to a cli newline consistent with the surrounding style, e.g.:

```r
cli::cli_inform("")
```

(If the `cat("\n")` is purely cosmetic spacing between messages, `cli::cli_inform("")` reproduces it. If it terminates a `cat`-built line, fold that whole line into the adjacent `cli::cli_inform()` instead.)

- [ ] **Step 3: Load and smoke-test**

Run: `Rscript -e 'devtools::load_all("."); cat("loaded OK\n")'`
Expected: loads without error.

- [ ] **Step 4: Commit**

```bash
git add R/master.R
git commit -m "style: use cli instead of cat for user-facing output in master.R"
```

### Task 3: Fix deprecated ggplot2 `size=` → `linewidth`

**Files:**
- Modify: `R/plot_xfplate.R` (lines `1025`, `1030`, `1031`, `1136`, `1141`, `1142`, and any other `size =` inside `element_line()` / `geom_hline()` / `geom_vline()` / line-drawing geoms across the sketch plots)

**Interfaces:**
- Consumes: nothing. Produces: no API change; removes ggplot2 ≥3.4 deprecation warnings.

- [ ] **Step 1: Find every offending `size =`**

Run: `grep -nE 'element_line\([^)]*size =|geom_(h|v)line\([^)]*size =|geom_segment\([^)]*size =|geom_line\([^)]*size =' R/plot_xfplate.R`
Record each line. **Do not** change `size =` on `geom_point`/`geom_text`/`geom_label` (there `size` is still correct — it's point/text size, not line width).

- [ ] **Step 2: Replace `size` with `linewidth` on line geoms/elements only**

For each line-drawing occurrence, rename the argument, e.g.:

```r
ggplot2::element_line(color = "grey90", linewidth = 0.3)
ggplot2::geom_hline(yintercept = seq(0, y_max, by = 10), color = "grey90", linewidth = 0.3)
ggplot2::geom_vline(xintercept = seq(0, x_max, by = 10), color = "grey90", linewidth = 0.3)
```

- [ ] **Step 3: Verify the examples render without deprecation warnings**

Run:
```bash
Rscript -e 'devtools::load_all("."); df <- tibble::tibble(group=c("a","b"), basal_ocr=c(25,30), fccp_ocr=c(50,60), amrot_ocr=c(15,18), basal_ecar=c(20,28), fccp_ecar=c(22,32), amrot_ecar=c(18,21)); print(class(plot_bioenergetic_space(df))); print(class(plot_bioenergetic_trajectory(df)))' 2>&1 | grep -i 'deprecat' && echo "STILL DEPRECATED" || echo "OK no deprecation"
```
Expected: `OK no deprecation`, and both print `"gg" "ggplot"`.

- [ ] **Step 4: Commit**

```bash
git add R/plot_xfplate.R
git commit -m "fix: use linewidth instead of deprecated ggplot2 size for line geoms"
```

### Task 4: Clear "no visible binding" NOTEs via `utils::globalVariables()`

**Files:**
- Create: `R/globals.R`

**Interfaces:**
- Consumes: nothing. Produces: a single registration of NSE column names so `R CMD check` stops reporting undefined globals.

- [ ] **Step 1: Harvest the exact list from a check run**

Run:
```bash
R CMD build . && R CMD check --no-manual seahtrue_*.tar.gz 2>&1 | \
  grep -A200 'no visible binding for global variable' | \
  grep -oE "'[^']+'" | tr -d "'" | sort -u
```
Collect every reported name (e.g. `well`, `group`, `measurement`, `OCR_wave_bc`, `ECAR_wave_bc`, `J_glyco`, `J_oxphos`, `init_ocr`, `fccp_ocr`, `amrot_ocr`, `om_ocr`, `mon_ocr`, `non_mito_ocr`, `basal_ocr`, `max_ocr`, `basal_ecar`, `max_ecar`, `bioenergetic_scope`, …). This produces the authoritative set — do not hand-guess it.

- [ ] **Step 2: Write the registration file**

Create `R/globals.R` with the harvested names (illustrative — replace with the full harvested set):

```r
# Column names used in non-standard evaluation (dplyr/tidyr) across the package.
# Registered here so R CMD check does not report "no visible binding".
utils::globalVariables(c(
  "well", "group", "measurement",
  "OCR_wave", "OCR_wave_bc", "ECAR_wave", "ECAR_wave_bc", "PER_wave",
  "J_glyco", "J_oxphos", "my_OCR", "my_ECAR", "OCR", "ECAR",
  "init_ocr", "fccp_ocr", "amrot_ocr", "om_ocr", "mon_ocr", "non_mito_ocr",
  "basal_ocr", "max_ocr", "spare_ocr", "atp_linked", "proton_leak",
  "init_ecar", "fccp_ecar", "amrot_ecar", "om_ecar", "mon_ecar",
  "basal_ecar", "max_ecar", "spare_ecar", "bioenergetic_scope"
  # ... append every remaining name reported in Step 1 ...
))
```

- [ ] **Step 3: Re-check that the NOTE is gone**

Run: `R CMD build . && R CMD check --no-manual seahtrue_*.tar.gz 2>&1 | grep -c 'no visible binding'`
Expected: `0`. Delete built artifacts afterward.

- [ ] **Step 4: Commit**

```bash
git add R/globals.R
git commit -m "fix: register NSE column names to clear no-visible-binding NOTEs"
```

### Task 5: Establish the BiocCheck baseline (checkpoint)

**Files:** none (produces a recorded baseline).

**Interfaces:** Consumes Tasks 1–4. Produces the authoritative issue list that guides Phase 2/3 priorities.

- [ ] **Step 1: Run BiocCheck**

Run:
```bash
Rscript -e 'BiocCheck::BiocCheck(".")' 2>&1 | tee /tmp/bioccheck-baseline.txt
```

- [ ] **Step 2: Record the summary**

Copy the final ERROR/WARNING/NOTE counts and the line-length / function-length notes into `NEWS.md` under a new `# seahtrue (development)` heading as a working checklist (these get removed before release). This is the reference for what remains.

- [ ] **Step 3: Commit**

```bash
git add NEWS.md
git commit -m "docs: record BiocCheck baseline after Phase 1 hygiene"
```

---

# Phase 2 — Harden the space API

### Task 6: Add input validation to `calculate_space()`

**Files:**
- Modify: `R/plot_xfplate.R` (top of `calculate_space`, after the existing `conversion_model` check at ~`:820`)
- Test: `tests/testthat/test-calculate_space.R` (create)

**Interfaces:**
- Consumes: the verified `calculate_space()` signature (Background).
- Produces: `calculate_space()` that raises clear `cli::cli_abort` errors for missing/invalid `param_set_*` and malformed `rate`. Return value/columns unchanged for valid input.

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-calculate_space.R`:

```r
test_that("calculate_space errors clearly on a non-tibble rate", {
  expect_error(
    calculate_space(rate = 42,
                    param_set_ocr = c(init_ocr = "m3"),
                    param_set_ecar = c(init_ecar = "m3")),
    regexp = "rate"
  )
})

test_that("calculate_space errors when rate lacks required columns", {
  bad <- tibble::tibble(group = "a", measurement = 1L)  # missing OCR_wave_bc/ECAR_wave_bc
  expect_error(
    calculate_space(rate = bad,
                    param_set_ocr = c(init_ocr = "m3"),
                    param_set_ecar = c(init_ecar = "m3")),
    regexp = "OCR_wave_bc|ECAR_wave_bc|column"
  )
})

test_that("calculate_space errors on unnamed param_set", {
  rate <- tibble::tibble(group = "a", measurement = 1L,
                         OCR_wave_bc = 1, ECAR_wave_bc = 1)
  expect_error(
    calculate_space(rate = rate,
                    param_set_ocr = c("m3"),          # names missing
                    param_set_ecar = c(init_ecar = "m3")),
    regexp = "param_set_ocr"
  )
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `Rscript -e 'devtools::test(filter = "calculate_space")'`
Expected: FAIL (current code gives cryptic dplyr/rlang errors, not these messages).

- [ ] **Step 3: Implement validation**

Insert after the `conversion_model` check in `calculate_space`:

```r
  # --- input validation ---
  if (!is.data.frame(rate)) {
    cli::cli_abort("{.arg rate} must be a data frame/tibble, not {.cls {class(rate)}}.")
  }
  required_rate_cols <- c("group", "measurement", "OCR_wave_bc", "ECAR_wave_bc")
  missing_rate <- setdiff(required_rate_cols, names(rate))
  if (length(missing_rate) > 0) {
    cli::cli_abort(c(
      "{.arg rate} is missing required column{?s}: {.val {missing_rate}}.",
      "i" = "Pass the tibble from {.code purrr::pluck(revive_xfplate(file), \"rate_data\", 1)}."
    ))
  }
  for (arg in c("param_set_ocr", "param_set_ecar")) {
    val <- get(arg)
    if (is.null(names(val)) || any(names(val) == "" ) || is.null(val)) {
      cli::cli_abort(c(
        "{.arg {arg}} must be a *named* character vector.",
        "i" = "e.g. {.code c(init_ocr = \"m3\", fccp_ocr = \"m4\", amrot_ocr = \"m9\")}"
      ))
    }
  }
```

- [ ] **Step 4: Run to verify pass**

Run: `Rscript -e 'devtools::test(filter = "calculate_space")'`
Expected: the three validation tests PASS.

- [ ] **Step 5: Commit**

```bash
git add R/plot_xfplate.R tests/testthat/test-calculate_space.R
git commit -m "feat: validate calculate_space inputs with clear cli errors"
```

### Task 7: Document & guard the canonical column-name contract

**Files:**
- Modify: `R/plot_xfplate.R` (roxygen of `calculate_space` + a warning inside it)
- Test: `tests/testthat/test-calculate_space.R` (append)

**Interfaces:**
- Consumes: Task 6 validation. Produces: `calculate_space()` warns (not silently NAs) when the canonical names needed by downstream metrics/plots are absent, and documents the required vocabulary.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-calculate_space.R`:

```r
test_that("calculate_space warns when plot-required canonical names are missing", {
  file <- system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx",
                      package = "seahtrue")
  skip_if(file == "", "example data not installed")
  rate <- purrr::pluck(revive_xfplate(file), "rate_data", 1)
  # deliberately use a non-canonical ECAR name ("max_ecar" instead of "amrot_ecar")
  expect_warning(
    calculate_space(
      rate = rate,
      param_set_ocr  = c(init_ocr = "m3", fccp_ocr = "m4", amrot_ocr = "m9", mon_ocr = "m12"),
      param_set_ecar = c(init_ecar = "m3", fccp_ecar = "m4", weird_ecar = "m9", mon_ecar = "m12")
    ),
    regexp = "amrot_ecar|canonical|downstream"
  )
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `Rscript -e 'devtools::test(filter = "calculate_space")'`
Expected: FAIL (no warning currently emitted).

- [ ] **Step 3: Implement the guard**

After both `param_set_*` are applied (i.e., after the `df_ocr`/`df_ecar` renames, before `return(df_space)`), add:

```r
  # Names required by the plotting functions downstream.
  plot_required <- c("basal_ocr", "fccp_ocr", "amrot_ocr",
                     "basal_ecar", "fccp_ecar", "amrot_ecar")
  produced <- names(df_space)
  missing_for_plots <- setdiff(plot_required, produced)
  if (length(missing_for_plots) > 0) {
    cli::cli_warn(c(
      "Output lacks column{?s} {.val {missing_for_plots}} needed by \\
       {.fn plot_bioenergetic_space}/{.fn plot_bioenergetic_trajectory}.",
      "i" = "Use the canonical injection names in {.arg param_set_ocr}/{.arg param_set_ecar}: \\
             {.val {c('init','fccp','amrot','om','mon')}} suffixed with {.val _ocr}/{.val _ecar}."
    ))
  }
```

- [ ] **Step 4: Document the contract in roxygen**

Add a `@section Canonical injection names:` block to `calculate_space`'s roxygen listing the vocabulary (`init_/fccp_/amrot_/om_/mon_` × `_ocr`/`_ecar`), which names are pass-through vs derived (`basal_*`, `max_*`, `spare_*`, `supply_index` are derived), and that the plot functions require `basal_ocr, fccp_ocr, amrot_ocr, basal_ecar, fccp_ecar, amrot_ecar`. Then run `Rscript -e 'devtools::document()'`.

- [ ] **Step 5: Run tests + check docs built**

Run: `Rscript -e 'devtools::test(filter = "calculate_space")'` (PASS) and confirm `man/calculate_space.Rd` updated.

- [ ] **Step 6: Commit**

```bash
git add R/plot_xfplate.R man/calculate_space.Rd tests/testthat/test-calculate_space.R
git commit -m "feat: warn on and document calculate_space canonical name contract"
```

### Task 8: Resolve `amrot_ecar` vs `max_ecar` (DECISION GATE A)

**Files:** `R/plot_xfplate.R` (+ `man/…` if roxygen changes) — **or docs-only**, depending on Gate A.

**Interfaces:** Consumes Gate A answer. Produces either a corrected plot width or a documented rationale. **Do not start until Gate A is answered.**

- [ ] **Step 1: Apply the maintainer's decision**
  - **If "bug — use `max_ecar`":** in `plot_bioenergetic_space()`, add `max_ecar` to `required_cols`, and change the rectangle width (`x_max` at `:1003`, `xmax = .data$amrot_ecar` at `:1008`, and the `geom_segment` xend at `:1012`) to use `.data$max_ecar`. Add a test asserting the rectangle's `xmax` aesthetic maps to `max_ecar`.
  - **If "intentional — keep `amrot_ecar`":** add a `@details` note to both plot functions explaining that the space width uses the antimycin/rotenone ECAR by design and how it differs from the `max_ecar` used in `supply_index`/`glyco_index_max`. No code change.

- [ ] **Step 2: Verify** — `Rscript -e 'devtools::test()'` PASS and, if docs changed, `devtools::document()` run.

- [ ] **Step 3: Commit** with a message stating which branch of Gate A was taken.

### Task 9: Extract the space feature into its own file

**Files:**
- Create: `R/space_xfplate.R`
- Modify: `R/plot_xfplate.R` (remove the moved block)

**Interfaces:**
- Consumes: Tasks 6–8. Produces: `calculate_space`, `plot_bioenergetic_space`, `plot_bioenergetic_trajectory` (with full roxygen + `@importFrom` tags) relocated verbatim; QC/sketch plots stay in `plot_xfplate.R`.

- [ ] **Step 1: Move the block**

Cut the `# new functions ---` section (from `#' Calculate Bioenergetic Indices…` through the end of `plot_bioenergetic_trajectory`) out of `R/plot_xfplate.R` and paste it, unchanged, into a new `R/space_xfplate.R` (keep all roxygen and `@importFrom` lines with their functions).

- [ ] **Step 2: Rebuild namespace & load**

Run: `Rscript -e 'devtools::document(); devtools::load_all(".")'`
Expected: no errors; `NAMESPACE` still exports the three functions.

- [ ] **Step 3: Full test run**

Run: `Rscript -e 'devtools::test()'`
Expected: all PASS (pure move — behavior unchanged).

- [ ] **Step 4: Commit**

```bash
git add R/space_xfplate.R R/plot_xfplate.R NAMESPACE
git commit -m "refactor: extract bioenergetic space/trajectory API into space_xfplate.R"
```

### Task 10: Naming cleanup

**Files:**
- Modify: `R/space_xfplate.R`, `NEWS.md`
- Test: `tests/testthat/test-calculate_space.R`

**Interfaces:** Consumes Task 9. Produces consistent naming. **Note the book-of-seahtrue impact of renaming `supply_index`.**

- [ ] **Step 1: Decide the `supply_index` name**

The comment says "**Supply** Flexibility Index" but the feature is "**Space** Flexibility Index". Pick ONE and make code + comment + docs agree. **Recommendation:** keep the exported column name `supply_index` (renaming breaks the book), and fix the *comment/roxygen* to consistently say "Supply Flexibility Index". If the maintainer prefers `space_index`/`sfi`, rename the column, add it to `NEWS.md` as a breaking change, and update the book's chapters that reference it.

- [ ] **Step 2: snake_case the two args**

Rename `OCR_var` → `ocr_var`, `ECAR_var` → `ecar_var` in the signature and body (these are function-local, not columns; low blast radius). Update roxygen `@param`. Since defaults are unchanged, positional/most named calls keep working; note in `NEWS.md` that the argument names changed.

- [ ] **Step 3: Verify**

Run: `Rscript -e 'devtools::document(); devtools::test()'` → PASS.

- [ ] **Step 4: Commit**

```bash
git add R/space_xfplate.R man NEWS.md tests/testthat/test-calculate_space.R
git commit -m "refactor: consistent naming for space API (args snake_case, SFI wording)"
```

### Task 11: Custom `atp_factors` escape hatch (DECISION GATE B — optional)

**Files:** `R/space_xfplate.R`, test, `man/…`. **Do not start unless Gate B = "yes".**

**Interfaces:** Consumes Gate B. Produces an optional `atp_factors = NULL` arg that, when supplied, overrides the preset list.

- [ ] **Step 1: Write failing test** — `calculate_space(..., atp_factors = list(Jglyco_ecar_factor=1, Jglyco_ocr_factor=0, Joxphos_ocr_factor=1))` yields `J_oxphos == OCR_wave_bc / ug_protein_scaling_factor` for a known row.
- [ ] **Step 2: Run — fails** (arg doesn't exist).
- [ ] **Step 3: Implement** — add `atp_factors = NULL` to the signature; if non-NULL, validate it has the three named numeric elements (`cli::cli_abort` otherwise) and use it instead of the preset; else keep preset selection by `conversion_model`.
- [ ] **Step 4: Run — passes.** Update roxygen. `devtools::document()`.
- [ ] **Step 5: Commit** `feat: allow custom atp_factors in calculate_space`.

---

# Phase 3 — Tests for the exported API

### Task 12: Test fixture + `revive_xfplate()` direct test

**Files:**
- Create: `tests/testthat/helper-seahtrue.R`
- Create: `tests/testthat/test-revive_xfplate-api.R`

**Interfaces:**
- Produces: a shared `example_plate()` helper returning the parsed shipped file, and a direct test of the exported `revive_xfplate()`.

- [ ] **Step 1: Write the helper**

Create `tests/testthat/helper-seahtrue.R`:

```r
example_file <- function() {
  system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx",
              package = "seahtrue")
}
example_plate <- function() {
  f <- example_file()
  testthat::skip_if(f == "", "example extdata not installed")
  revive_xfplate(f)
}
```

- [ ] **Step 2: Write the failing test**

Create `tests/testthat/test-revive_xfplate-api.R`:

```r
test_that("revive_xfplate returns a nested structure with rate_data", {
  plate <- example_plate()
  expect_true("rate_data" %in% names(plate) || is.list(plate))
  rate <- purrr::pluck(plate, "rate_data", 1)
  expect_s3_class(rate, "tbl_df")
  expect_true(all(c("group", "measurement", "OCR_wave_bc", "ECAR_wave_bc") %in% names(rate)))
})
```

- [ ] **Step 3: Run — verify pass** (`revive_xfplate` exists; test documents its contract): `Rscript -e 'devtools::test(filter = "revive_xfplate-api")'`. If the structure differs, adjust the assertions to the *actual* returned shape (read `?revive_xfplate`), keeping a real assertion on `rate_data`.

- [ ] **Step 4: Commit** `test: add helper + direct revive_xfplate API test`.

### Task 13: `calculate_space()` value tests

**Files:**
- Modify: `tests/testthat/test-calculate_space.R`

**Interfaces:** Consumes Task 12 helper. Produces value/shape assertions on real data.

- [ ] **Step 1: Write the failing test**

Append:

```r
test_that("calculate_space returns one row per group with expected columns", {
  rate <- purrr::pluck(example_plate(), "rate_data", 1)
  out <- calculate_space(
    rate = rate,
    param_set_ocr  = c(init_ocr = "m3", fccp_ocr = "m4", amrot_ocr = "m9", mon_ocr = "m12"),
    param_set_ecar = c(init_ecar = "m3", fccp_ecar = "m4", amrot_ecar = "m9", mon_ecar = "m12"),
    conversion_model = "mookerjee"
  )
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), dplyr::n_distinct(rate$group))
  expect_true(all(c("group", "basal_ocr", "fccp_ocr", "amrot_ocr",
                    "basal_ecar", "fccp_ecar", "amrot_ecar",
                    "supply_index", "glyco_index") %in% names(out)))
  expect_type(out$supply_index, "double")
})

test_that("conversion_model presets produce different J values", {
  rate <- purrr::pluck(example_plate(), "rate_data", 1)
  ps_ocr  <- c(init_ocr = "m3", fccp_ocr = "m4", amrot_ocr = "m9", mon_ocr = "m12")
  ps_ecar <- c(init_ecar = "m3", fccp_ecar = "m4", amrot_ecar = "m9", mon_ecar = "m12")
  m <- calculate_space(rate, ps_ocr, ps_ecar, conversion_model = "mookerjee")
  a <- calculate_space(rate, ps_ocr, ps_ecar, conversion_model = "agilent")
  expect_false(isTRUE(all.equal(m$basal_ocr, a$basal_ocr)))
})
```

- [ ] **Step 2: Run — verify pass** (`devtools::test(filter="calculate_space")`). Adjust the exact column list only if a name legitimately differs from Background; keep real assertions.

- [ ] **Step 3: Commit** `test: value/shape tests for calculate_space on example data`.

### Task 14: Plot function tests

**Files:**
- Create: `tests/testthat/test-plot-space.R`

**Interfaces:** Produces tests that the two plot functions return `ggplot` objects and error on missing columns.

- [ ] **Step 1: Write the failing test**

```r
make_df_space <- function() {
  tibble::tibble(
    group = c("control", "treated"),
    basal_ocr = c(25, 30), fccp_ocr = c(50, 60), amrot_ocr = c(15, 18),
    basal_ecar = c(20, 28), fccp_ecar = c(22, 32), amrot_ecar = c(18, 21),
    max_ecar = c(30, 36)
  )
}

test_that("plot_bioenergetic_space returns a ggplot", {
  p <- plot_bioenergetic_space(make_df_space())
  expect_s3_class(p, "ggplot")
})

test_that("plot_bioenergetic_trajectory returns a ggplot", {
  p <- plot_bioenergetic_trajectory(make_df_space())
  expect_s3_class(p, "ggplot")
})

test_that("plot functions error on missing required columns", {
  bad <- tibble::tibble(group = "a", basal_ocr = 1)
  expect_error(plot_bioenergetic_space(bad), regexp = "required column")
  expect_error(plot_bioenergetic_trajectory(bad), regexp = "required column")
})
```

- [ ] **Step 2: Run — verify pass** (`devtools::test(filter = "plot-space")`).

- [ ] **Step 3: Commit** `test: ggplot return + missing-column errors for space plots`.

### Task 15: Full check + coverage sweep (checkpoint)

**Files:** possibly small fixes surfaced by the run.

**Interfaces:** Consumes all prior tasks. Produces a clean-as-possible `R CMD check` + `BiocCheck`.

- [ ] **Step 1: Run the full suite**

Run: `Rscript -e 'devtools::test()'` → all PASS.

- [ ] **Step 2: Run R CMD check**

Run: `R CMD build . && R CMD check --no-manual seahtrue_*.tar.gz 2>&1 | tail -30`
Expected: `Status: OK` or only pre-existing NOTEs; **no new WARNING/ERROR**. Fix anything newly introduced.

- [ ] **Step 3: Run BiocCheck and diff against baseline**

Run: `Rscript -e 'BiocCheck::BiocCheck(".")' 2>&1 | tail -30` and compare to `/tmp/bioccheck-baseline.txt` from Task 5. Confirm the NSE NOTE and `cat`/`size=` issues are gone.

- [ ] **Step 4: Commit** any fixes; then `docs: update NEWS.md with resolved BiocCheck items`.

---

# Phase 4 — Release-prep hand-off (do NOT push to Bioconductor here)

### Task 16: Version reconciliation & NEWS

**Files:**
- Modify: `DESCRIPTION`, `NEWS.md`

**Interfaces:** Consumes Phases 1–3. Produces a version consistent with the Bioconductor devel scheme, ready for a maintainer-run sync.

- [ ] **Step 1: Set the version**

Local `devel` is `1.1.0.9001`; Bioconductor devel is `1.7.0`. Bioconductor requires an *increase* and devel carries an **odd** minor. Set `DESCRIPTION` `Version:` to **`1.7.1`** (odd minor, above current). Confirm with the maintainer before finalizing.

- [ ] **Step 2: Write real NEWS entries**

Replace the working checklist under `# seahtrue (development)` with user-facing entries: input validation, documented canonical-name contract, Gate A/B outcomes, naming changes, new tests.

- [ ] **Step 3: Final check** — `R CMD build . && R CMD check --no-manual seahtrue_*.tar.gz` → clean.

- [ ] **Step 4: Commit** `chore: bump version to 1.7.1 and finalize NEWS for release`.

- [ ] **Step 5: STOP — hand back to maintainer.** Do not push to `upstream` (Bioconductor) or merge to `main`. The maintainer performs the Bioconductor sync (reconcile histories with `upstream/devel`, push), re-triggers the `webr.yml` build for the book, and watches the Bioconductor build report. This plan deliberately ends at a clean, versioned branch.

---

## Self-Review

- **Spec coverage:** every audit finding maps to a task — build hygiene (T1), cat→cli (T2), size→linewidth (T3), NSE NOTEs (T4), BiocCheck baseline (T5), input validation (T6), name contract (T7), amrot/max_ecar (T8, gated), file split (T9), naming (T10), custom factors (T11, gated), tests for revive/calculate/plots (T12–T14), full check (T15), version/release prep (T16). No finding left unassigned.
- **Placeholder scan:** the only intentionally deferred content is behind explicit **Decision Gates A/B** with recommended defaults, and the `globalVariables()` list (T4) which is *harvested from a real check run* by a defined procedure, not guessed. No "TODO/handle edge cases" placeholders.
- **Type/name consistency:** exported signatures and required-column lists are copied verbatim from the source (Background); test code uses those exact names; `example_plate()`/`example_file()` helpers (T12) are reused by T13.
- **Decision gates are honored:** T8 and T11 are marked "do not start until answered."
