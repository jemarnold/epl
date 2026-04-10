# Custom EPL ggplot2 theme

A
[ggplot2](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
theme for display.

## Usage

``` r
theme_epl(
  base_size = 14,
  base_family = "sans",
  border = c("partial", "full"),
  ink = "black",
  paper = "white",
  accent = "#0080ff",
  ...
)
```

## Arguments

- base_size:

  Base font size, given in pts.

- base_family:

  Base font family.

- border:

  Define either a *partial* or *full* border around plots.

- ink:

  Colour for text and lines. Default is *"black"*.

- paper:

  Background colour. Default is *"white"*.

- accent:

  Accent colour for highlights. Default is *"#0080ff"*.

- ...:

  Additional arguments to add to
  [theme()](https://ggplot2.tidyverse.org/reference/theme.html).

## Value

A [ggplot2](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Details

- `axis.title = element_text(face = "bold")` by default. Modify to
  *"plain"*.

- `panel.grid.major` & `panel.grid.major` set to blank. Modify to
  `= element_line()` for visible grid lines.

- `legend.position = "top"` by default. Modify `"none"` to remove legend
  entirely.

- `border = "partial"` uses `panel.border = element_blank()` and
  `axis.line = element_line()`.

- `border = "full"` uses `panel.border = element_rect(colour = "black",`
  `linewidth = 1)` and `axis.line = element_line()`.

- `base_family = "sans"` by default. `"Merriweather Sans"` is a nice
  alternative font which can be installed from
  <https://fonts.google.com/specimen/Merriweather+Sans>.

## See also

[`palette_epl()`](https://jemarnold.github.io/epl/reference/palette_epl.md)
[`scale_colour_epl()`](https://jemarnold.github.io/epl/reference/scale_colour_epl.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## set theme for the current script
theme_set(theme_epl())

## plot example data
read_tymewear(example_epl("tymewear_live"))$data |>
    ggplot() +
    aes(x = time) +
    scale_colour_epl(name = NULL) +
    ylab("Ventilation Measures") +
    scale_x_continuous(
        name = "Time (mm:ss)",
        breaks = breaks_timespan(),
        labels = format_hmmss
    ) +
    geom_line(aes(y = br, colour = "BR")) +
    geom_line(aes(y = ve, colour = "VE"))
} # }
```
