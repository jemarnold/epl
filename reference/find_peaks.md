# Find peak values from rows within a given timespan

Detects the peak values of `y` column within a `span` of `x` column
(e.g. timespan), returns the mean of all columns within those rows.

## Usage

``` r
find_peaks(data, x, y, span = 30, between = NULL)
```

## Arguments

- data:

  A data frame with at least numeric `x` and `y` columns.

- x:

  A character string for the column name of `x` (e.g. *"time"*. Must
  match column name exactly.

- y:

  A character string for the column name of `y` (e.g. *"VO2"*. Must
  match column name exactly.

- span:

  A numeric value defining timespan in which to calculate rolling local
  means, from which peak values will be taken. `span = 0` will take the
  single highest sample.

- between:

  An optional time range in the form `c(min, max)` in units of `x`,
  within which to look for peak values (inclusive).

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with one row of mean values from all columns across the peak-detected
rows. `Samples` column shows the number of samples within the timespan.

## Details

The function uses a rolling window approach:

1.  For each non-NA value of `y`, calculates the mean of `y` within a
    window from time `t` to `t + span`.

2.  Identifies the window with the maximum mean value of `y`.

3.  Extracts all rows and columns within that peak window.

4.  Returns the mean of all numeric columns (and first value of
    non-numeric columns) within the peak window.

## Examples

``` r
parvo_data <- read_parvo(example_epl("parvo_ramp"))$data

## Find peak 30-second VO2
find_peaks(parvo_data, x = "TIME", y = "VO2", span = 30)
#> # A tibble: 1 × 25
#>   samples  TIME    HR VO2kg   VO2  VCO2   RER    RR    Vt    VE VEVO2 VEVCO2
#>     <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
#> 1       1 2670.    47  50.5  3.69  4.17  1.13  36.7  3.23  97.4  32.1   28.4
#> # ℹ 13 more variables: FEO2 <dbl>, FECO2 <dbl>, FATmin <dbl>, CHOmin <dbl>,
#> #   FatOx <dbl>, CarbOx <dbl>, O2kJ <dbl>, O2kcal <dbl>, O2work <dbl>,
#> #   O2energy <dbl>, O2power <dbl>, O2pulse <dbl>, METS <dbl>

## search within specific time range
find_peaks(parvo_data, x = "TIME", y = "VO2", span = 60, between = c(1500, 2000))
#> # A tibble: 1 × 25
#>   samples  TIME    HR VO2kg   VO2  VCO2   RER    RR    Vt    VE VEVO2 VEVCO2
#>     <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
#> 1       2 1755.   222  49.3  3.61  4.09  1.13  44.4  2.92  106.  35.9   31.6
#> # ℹ 13 more variables: FEO2 <dbl>, FECO2 <dbl>, FATmin <dbl>, CHOmin <dbl>,
#> #   FatOx <dbl>, CarbOx <dbl>, O2kJ <dbl>, O2kcal <dbl>, O2work <dbl>,
#> #   O2energy <dbl>, O2power <dbl>, O2pulse <dbl>, METS <dbl>
```
