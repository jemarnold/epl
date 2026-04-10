# Scales for custom EPL palette

Scales for custom EPL palette

## Usage

``` r
scale_colour_epl(..., aesthetics = "colour")

scale_color_epl(..., aesthetics = "colour")

scale_fill_epl(..., aesthetics = "fill")
```

## Arguments

- ...:

  Arguments passed to
  [`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html).

- aesthetics:

  A character vector with aesthetic(s) passed to
  [`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html).
  Default is `"colour"`.

## Value

A ggplot2 scale object.

## See also

[`theme_epl()`](https://jemarnold.github.io/epl/reference/theme_epl.md)
[`palette_epl()`](https://jemarnold.github.io/epl/reference/palette_epl.md)
