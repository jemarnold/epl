# Replace Local Outliers

Detects local outliers in vector data with a Hampel filter using median
absolute deviation (MAD), and replaces with `NA` or the local median
value.

## Usage

``` r
replace_outliers(x, width, method = c("median", "NA"), t0 = 3)
```

## Arguments

- x:

  A numeric vector.

- width:

  An integer defining the sample window in which to detect local
  outliers. Where `window = -width < idx < width`.

- method:

  A character string indicating how to handle replacement (see *Details*
  for more on each method):

  `"median"`

  :   Replaces outliers with the median within a locally centred window
      defined by `width` (the default).

  `"NA"`

  :   Replaces outliers with `NA`.

- t0:

  An integer for the local outlier threshold. Default `t0 = 3`
  (Pearson's rule; analogous to 3σ rule).

## Value

A numeric vector of filtered data.

## Details

The default `method = "median"` will replace outliers with the local
median value, as in `pracma::hampel()`. Otherwise, `method = "NA"` will
replace outliers with `NA`.

This function will pass through any missing `NA` values in the input
vector `x`. `NA` values in `x` are excluded from processing and restored
in the returned vector, but not replaced with the local median value.

A high `t0` threshold makes the outlier filter more forgiving, a low one
will declare more points to be outliers. `t0 = 3` (the default)
corresponds to Pearson's 3 sigma edit rule, `t0 = 0` to Tukey's median
filter.

## See also

`pracma::hampel()`

## Examples

``` r
tyme_data <- read_tymewear(example_epl("tymewear_live"))$data
vt_filtered <- replace_outliers(tyme_data$vt, width = 7, method = "median")

if (FALSE) { # \dontrun{
ggplot(tyme_data) +
    aes(x = time, y = vt) +
    ylab("Tidal Volume (L)") +
    scale_x_continuous(
        name = "Time (mm:ss)",
        breaks = breaks_timespan(),
        labels = format_hmmss
    ) +
    scale_colour_epl() +
    geom_line(aes(colour = "BR")) +
    geom_point(
        data = slice(tyme_data, which(vt_filtered != vt)),
        aes(y = vt, colour = "outliers")
    )
} # }
```
