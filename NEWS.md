# epl 0.2.1

* Attempt to fix failing R-CMD-check

# epl 0.2.0

* Create `replace_outliers()` to detect and replace local outliers with `NA` or the local median value.
* Create `find_peaks()` to detect peak values within a given timespan, e.g. for finding 30-second V̇O~2~peak.
* Create `theme_epl()` and other `{ggplot2}` helper functions for pretty plotting.
* Create `data.R` with documentation of included example files retrievable with `example_epl()`.
* Remove `time_column` argument for `read_parvo()` since as far as I can tell the time column is always *"TIME"*.
* Website documentation available [here](https://jemarnold.github.io/epl/), including function reference index and vignettes.
* Write custom `Reference/Index` package documentation available [here](https://jemarnold.github.io/epl/reference/index.html).
* Create *"Reading and Cleaning Data"* vignette on common usage of functions. Available [here](https://jemarnold.github.io/epl/articles/reading-and-cleaning-data.html).
* Re-write simplified README pointing to vignette for more details.
* Update `test_that` for new functions.

# epl 0.1.0 internal release

* Initial internal release of development version.
* `read_parvo()` for reading data exported from *"Parvo TrueOne"* metabolic cart.
* `read_tymewear()` for reading data exported from *"Tymewear"* device.
    * currently only method available for *"live"* file export.
* `example_epl()` for retrieving example data files to test read and process functions.
* Undocumented internal validation & helper functions in `helpers.R`.
* Basic README to demonstrate usage for the current three exported functions.
* Unit tests for exported functions.
