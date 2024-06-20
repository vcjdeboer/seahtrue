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
