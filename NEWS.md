# seahtrue (development version)

## BiocCheck working checklist (remove before release)

- Baseline after Phase 1 hygiene (2026-09-05): ERROR 3, WARNING 1, NOTE 6.
- Remaining items to address in Phase 2/3:
  - ERROR: `Remotes: bioc::BiocStyle` in DESCRIPTION must be removed before release (Bioc requires deps from CRAN/Bioconductor only).
  - ERROR: package `Version:` (1.1.0.9001) is not a valid Bioconductor version number (needs x.y.z form).
  - ERROR: package directory name must match the `Package:` field — not applicable to the real checkout (this run was from a worktree directory named `bioc-space-improvements`, not `seahtrue`); re-verify from a `seahtrue`-named checkout before release.
  - WARNING: no Bioconductor dependencies detected in DESCRIPTION.
  - NOTE: `suppressWarnings()` used 3 times (R/read_xfplate.R lines 139, 140, 383) — avoid if possible.
  - NOTE: 18 functions exceed the recommended 50-line length; longest include `get_xf_assayinfo()` (R/read_xfplate.R, 258 lines) and `validate_O2_pH_levels()` (R/assertions_read.R, 105 lines).
  - NOTE: R version dependency should be updated from 4.2.0 to 4.4.0.
  - NOTE: 120 lines (4%) exceed 80 characters; 418 lines (13%) are not indented in multiples of 4 spaces — consider running `styler`.
  - NOTE: cannot verify Bioc-devel mailing list subscription (requires admin credentials) — confirm manually before release.

# seahtrue 0.99.2

* bioconductor - refactor the code that gives the no visible binding for tibble calls, and validator ruls
* bioconductor - include importsFrom in validate_preprocess function for (head, tail, quantile functions)
* bioconductor - remove family tag in master functions in roxygen comments
* biocondcutor - updated the vignette layout for the sessionInfo call

# seahtrue 0.99.1

* bioconductor - lower package size by removing example files.
* bioconductor - removed citation because there is no DOI

# seahtrue 0.99.0

* bioconductor

# seahtrue 0.3.0

* Plotting functions are now written and included. These are the `sketch_plate()`, `sketch_rate()`, `sketch_assimilate_raw()` and `sketch_assimilate_rate()` functions
* The vignettes and pkgdown gh pages are updated with examples on how to use the plotting functions

# seahtrue 0.2.0

* Master function name changed from `run_seahorse()` to `revive_xfplate()`
* Fixed an issue in `calc_background()` where the summarize/reframe was not grouped, which resulted in 48 times inflation of rows
* Complete rework of the assertions and tests that are performed on the data
* Initialized the pkgdown style vignettes and seahtrue package info docs
* deployed the docs to github pages
* updated the data files provided in the package

# seahtrue 0.1.0

* Initial first complete release
