# seahtrue 1.7.1

* Added input validation with clear error messages for `calculate_space()`.
* Documented the canonical injection-name contract; `calculate_space()` now warns when the plot-required columns can't be produced.
* `calculate_space()` arguments `OCR_var`/`ECAR_var` renamed to `ocr_var`/`ecar_var` for naming consistency (no change in behavior or defaults).
* `calculate_space()` gains an optional `atp_factors` argument to supply custom ATP conversion factors.
* Space/trajectory functions moved to their own source file.
* Added test coverage for `revive_xfplate()`, `calculate_space()`, and the space plots.
* Package hygiene: converted source files to ASCII-only, cleared R CMD check NOTEs, and switched internal messaging to `cli`.
* Regenerated the `revive_output_donor_A` example dataset with current code, removing a stale dependency on the `validate`/`settings` packages that broke `R CMD check` in environments without them.
* Switched the vignette output from `BiocStyle::html_document` to `rmarkdown::html_document` and dropped `BiocStyle` from Suggests, so the webR build (which cannot resolve Bioconductor-only packages) builds without it.
* Rewrote the internal O2/pH tick-range quality check as a single vectorized whole-plate pass instead of a per-well loop; identical output, but fast enough to run under webR/wasm in the interactive book.

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
