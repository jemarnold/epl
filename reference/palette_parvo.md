# Custom Parvo colour palette

Custom Parvo colour palette

## Usage

``` r
palette_parvo(...)
```

## Arguments

- ...:

  A character string specifying the named colour(s) to return.

## Value

Named character vector of hex colours.

## See also

[`theme_epl()`](https://jemarnold.github.io/epl/reference/theme_epl.md)
[`scale_colour_epl()`](https://jemarnold.github.io/epl/reference/scale_colour_epl.md)
[`palette_epl()`](https://jemarnold.github.io/epl/reference/palette_epl.md)

## Examples

``` r
if (FALSE) { # \dontrun{
scales::show_col(palette_parvo())
scales::show_col(palette_parvo("VO2", "VCO2", "RER", "VEVO2"))
} # }
```
