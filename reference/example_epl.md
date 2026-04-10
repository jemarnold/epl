# Get path to EPL example files

Get path to EPL example files

## Usage

``` r
example_epl(file = NULL)
```

## Arguments

- file:

  Name of file as character string. If `NULL`, returns a vector of all
  available file names.

## Value

File paths for selected example files stored in this package.

## Examples

``` r
## lists all files
example_epl()
#> [1] "parvo_binned.CSV"  "parvo_bxb.CSV"     "parvo_excel.xlsx" 
#> [4] "parvo_ramp.CSV"    "tymewear_live.csv" "tymewear_post.csv"

## partial matching will error if matches multiple
## example_epl("tymewear")
#> Error in `example_epl()`:
#> ! Multiple files match "tymewear":
#> ℹ Matching files: "tymewear_live.csv" and "tymewear_post.csv"

example_epl("tymewear_live")
#> [1] "/home/runner/work/_temp/Library/epl/extdata/tymewear_live.csv"
```
