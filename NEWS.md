# epl 0.2.0

* Create `replace_outliers()` to detect and replace local outliers with `NA` or the local median value.
* Create `theme_epl()` and other `{ggplot2}` helper functions for pretty plotting.
* Updated README for new functions
* Updated `test_that` for new functions.

# epl 0.1.0

* Initial public commit of development version.
* `read_parvo()` for reading data exported from *"Parvo TrueOne"* metabolic cart.
* `read_tymewear()` for reading data exported from *"Tymewear"* device.
    * currently only method available for *"live"* file export.
* `example_epl()` for retrieving example data files to test read and process functions.
* Undocumented internal validation & helper functions in `helpers.R`.
* Basic README to demonstrate usage for the current three exported functions.
* Unit tests for exported functions.
