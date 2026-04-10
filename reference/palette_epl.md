# Custom EPL colour palette

Custom EPL colour palette

## Usage

``` r
palette_epl(n = NULL)
```

## Arguments

- n:

  A character or numeric vector specifying either the name or the number
  in order of colours to return.

## Value

Named or unnamed character vector of hex colours.

## See also

[`theme_epl()`](https://jemarnold.github.io/epl/reference/theme_epl.md)
[`scale_colour_epl()`](https://jemarnold.github.io/epl/reference/scale_colour_epl.md)
[`palette_parvo()`](https://jemarnold.github.io/epl/reference/palette_parvo.md)

## Examples

``` r
if (FALSE) { # \dontrun{
scales::show_col(palette_epl())
scales::show_col(palette_epl(2))
scales::show_col(palette_epl(c("red", "orange")))
} # }
```
