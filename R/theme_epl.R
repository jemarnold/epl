#' Custom EPL ggplot2 theme
#'
#' A [ggplot2][ggplot2::ggplot2-package] theme for display.
#'
#' @param base_size Base font size, given in pts.
#' @param base_family Base font family.
#' @param border Define either a *partial* or *full* border around plots.
#' @param ink Colour for text and lines. Default is *"black"*.
#' @param paper Background colour. Default is *"white"*.
#' @param accent Accent colour for highlights. Default is *"#0080ff"*.
#' @param ... Additional arguments to add to [theme()][ggplot2::theme()].
#'
#' @details
#' - `axis.title = element_text(face = "bold")` by default. Modify to *"plain"*.
#'
#' - `panel.grid.major` & `panel.grid.major` set to blank. Modify to
#'   `= element_line()` for visible grid lines.
#'
#' - `legend.position = "top"` by default. Modify `"none"` to remove legend
#'   entirely.
#'
#' - `border = "partial"` uses `panel.border = element_blank()` and
#'   `axis.line = element_line()`.
#'
#' - `border = "full"` uses `panel.border = element_rect(colour = "black",`
#'   `linewidth = 1)` and `axis.line = element_line()`.
#'
#' - `base_family = "sans"` by default. `"Merriweather Sans"` is a nice
#'   alternative font which can be installed from
#'   <https://fonts.google.com/specimen/Merriweather+Sans>.
#'
#' @return A [ggplot2][ggplot2::ggplot()] object.
#'
#' @seealso [palette_epl()] [scale_colour_epl()]
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' ## set theme for the current script
#' theme_set(theme_epl())
#'
#' ## plot example data
#' read_tymewear(example_epl("tymewear_live"))$data |>
#'     ggplot() +
#'     aes(x = time) +
#'     scale_colour_epl(name = NULL) +
#'     ylab("Ventilation Measures") +
#'     scale_x_continuous(
#'         name = "Time (mm:ss)",
#'         breaks = breaks_timespan(),
#'         labels = format_hmmss
#'     ) +
#'     geom_line(aes(y = br, colour = "BR")) +
#'     geom_line(aes(y = ve, colour = "VE"))
#' }
#'
#' @importFrom ggplot2 theme_bw theme element_rect element_line element_text
#'  element_blank rel margin_auto margin_part unit
#'
#' @export
theme_epl <- function(
        base_size = 14,
        base_family = "sans",
        border = c("partial", "full"),
        ink = "black",
        paper = "white",
        accent = "#0080ff",
        ...
) {
    half_line = base_size/2
    border = match.arg(border)

    if (border == "partial") {
        panel.border <- element_blank()
        axis.line <- element_line()
    } else {
        panel.border <- element_rect(colour = "black", linewidth = 1)
        axis.line <- element_blank()
    }

    theme_bw(
        base_size = base_size,
        base_family = base_family,
        ink = ink,
        paper = paper,
        accent = accent
    ) +
        theme(
            plot.title = element_text(size = rel(1.2), lineheight = 1.1),
            plot.subtitle = element_text(lineheight = 1.1),
            plot.caption = element_text(colour = "grey50"),
            panel.border = panel.border,
            axis.line = axis.line,
            axis.title = element_text(face = "bold"),
            strip.background = element_rect(fill = "grey95"),
            strip.text = element_text(margin = margin_auto(t = half_line/2)),
            plot.margin = margin_part(r = base_size, unit = "pt"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "top",
            legend.justification = "right",
            legend.direction = "horizontal",
            legend.margin = margin_auto(t = 1),
            legend.box.spacing = unit(half_line/2, "pt")
        ) +
        theme(...)
}




#' Custom EPL colour palette
#'
#' @param n A character or numeric vector specifying either the name or the
#'   number in order of colours to return.
#'
#' @return Named or unnamed character vector of hex colours.
#'
#' @seealso [theme_epl()] [scale_colour_epl()] [palette_parvo()]
#'
#' @examples
#' \dontrun{
#' scales::show_col(palette_epl())
#' scales::show_col(palette_epl(2))
#' scales::show_col(palette_epl(c("red", "orange")))
#' }
#'
#' @export
palette_epl <- function(n = NULL) {
    colours_epl <- c(
        `light blue`  = "#0080ff",      ## "VL"
        `dark red`    = "#ba2630",      ## "FCR"
        `light green` = "#5b8c52",      ## "BB" "#7dbf70"
        `pink`        = "#ff80ff",      ## "VM"
        `orange`      = "#ff7f00",      ## "SCM"
        `dark blue`   = "#00468Bff",    ## "TA"
        `light red`   = "#db5555",      ## "ECR"
        `green`       = "#42B540FF",    ## "DL"
        `purple`      = "#9f79ee",      ## "RF"
        `brown`       = "#8b4726",      ## "PS"
        `blue`        = "#0000ff",      ## "HHb"
        `red`         = "#ED0000FF")    ## "O2Hb"

    if (is.null(n)) {
        return(unname(colours_epl))
    }
    if (is.character(n)) {
        return(colours_epl[n])
    }
    if (is.numeric(n) && n <= length(colours_epl)) {
        return(unname(colours_epl[seq_len(n)]))
    } else if (is.numeric(n)) {
        ## interpolate if more colours needed, but this probably won't look good!
        return(grDevices::colorRampPalette(colours_epl)(n))
    }
}



#' Custom Parvo colour palette
#'
#' @param ... A character string specifying the named colour(s) to return.
#'
#' @return Named character vector of hex colours.
#'
#' @seealso [theme_epl()] [scale_colour_epl()] [palette_epl()]
#'
#' @examples
#' \dontrun{
#' scales::show_col(palette_parvo())
#' scales::show_col(palette_parvo("VO2", "VCO2", "RER", "VEVO2"))
#' }
#'
#' @export
palette_parvo <- function(...) {
    cols <- c(...)
    colours_parvo <- c(
        "Power"        = "#e6ce3f",
        "Cadence"      = "#7dbf70",
        "Heart Rate"   = "#ba2630",
        "HR"           = "#ba2630",
        "VO2/HR"       = "red",
        "O2pulse"      = "red",
        "Speed"        = "#4a7db0",
        "VO2"          = "#98b6d3",
        "VCO2"         = "navajowhite3",
        "VE"           = "deepskyblue",
        "RF"           = "indianred3",
        "RR"           = "indianred3",
        "BR"           = "indianred3",
        "TV"           = "#7dbf70",
        "VT"           = "#7dbf70",
        "RER"          = "mediumpurple2",
        "VE/VO2"       = "springgreen2",
        "VEVO2"        = "springgreen2",
        "VE/VCO2"      = "tomato",
        "VEVCO2"       = "tomato",
        "FEO2"         = "sienna1",
        "FECO2"        = "sienna4",
        "FatOx"        = "sienna1",
        "CarbOx"       = "sienna4",
        "RPE"          = "darkblue",
        "BLa"          = "red",
        "Left"         = "#ff80ff",
        "Right"        = "#0080ff"
    )
    col_length <- length(colours_parvo)
    if (is.null(cols)) {
        return(colours_parvo)
    }
    if (all(is.numeric(cols)) &&
        (any(cols > col_length) || length(cols) > col_length)
    ) {
        cli::cli_abort(
            "Exceeded {.val {col_length}} colours available."
        )
    }
    colours_parvo[cols]
}




#' Scales for custom EPL palette
#'
#' @param ... Arguments passed to `ggplot2::discrete_scale()`.
#' @param aesthetics A character vector with aesthetic(s) passed to
#'   `ggplot2::discrete_scale()`. Default is `"colour"`.
#'
#' @return A ggplot2 scale object.
#'
#' @seealso [palette_epl()] [theme_epl()]
#' @rdname scale_colour_epl
#' @export
scale_colour_epl <- function(..., aesthetics = "colour") {
    ggplot2::discrete_scale(
        aesthetics = aesthetics,
        palette = palette_epl,
        na.value = "grey10",
        ...
    )
}

#' @rdname scale_colour_epl
#' @export
scale_color_epl <- function(..., aesthetics = "color") {
    ggplot2::discrete_scale(
        aesthetics = aesthetics,
        palette = palette_epl,
        na.value = "grey10",
        ...
    )
}

#' @rdname scale_colour_epl
#' @export
scale_fill_epl <- function(..., aesthetics = "fill") {
    ggplot2::discrete_scale(
        aesthetics = aesthetics,
        palette = palette_epl,
        na.value = "grey10",
        ...
    )
}




#' Breaks for Timespan Data
#'
#' Pretty timespan breaks for plotting in units of 5, 15, 30, 60 sec, etc.
#' Modified from [scales::breaks_timespan()].
#'
#' @param unit The time unit used to interpret numeric data input (defaults to
#'   *"secs"*).
#' @param n Desired number of breaks. You may get slightly more or fewer breaks
#'   than requested.
#'
#' @return Returns a function for generating breaks.
#'
#' @examples
#' \dontrun{
#' x = 0:120
#' y = sin(2 * pi * x / 15) + rnorm(length(x), 0, 0.2)
#' ggplot(data.frame(x, y)) +
#'     aes(x = x, y = y) +
#'     scale_x_continuous(breaks = breaks_timespan()) +
#'     geom_line()
#' }
#'
#' @keywords internal
#' @export
breaks_timespan <- function(
        unit = c("secs", "mins", "hours", "days", "weeks"),
        n = 5
) {
    unit <- match.arg(unit)
    force(n)
    function(x) {
        x <- as.numeric(as.difftime(x, units = unit), units = "secs")
        range <- range(x, na.rm = TRUE)
        diff <- range[2] - range[1]

        ## scale of time range
        ## define nice steps for each unit
        if (diff <= 5 * 60) {
            scale <- 1 ## sec
            nice_steps <- c(1, 2, 5, 10, 15, 20, 30, 60, 120)
        } else if (diff <= 5 * 3600) {
            scale <- 60 ## min
            nice_steps <- c(1, 2, 5, 10, 15, 20, 30, 60, 120) * 60
        } else if (diff <= 5 * 86400) {
            scale <- 3600 ## hr
            nice_steps <- c(0.25, 0.5, 1, 2, 3, 4, 6, 8, 12, 24) * 3600
        } else {
            scale <- 86400 ## days
            nice_steps <- c(1, 7, 28) * 86400
        }

        ## scale to scale units
        range_scaled <- range / scale
        scaled_steps <- nice_steps / scale

        ## find optimal step size from nice_steps
        target_step <- diff(range_scaled) / n
        best_step <- scaled_steps[which.min(abs(scaled_steps - target_step))]

        ## generate breaks
        breaks_scaled <- seq(
            floor(range_scaled[1] / best_step) * best_step,
            ceiling(range_scaled[2] / best_step) * best_step,
            by = best_step
        )

        ## convert back to seconds
        round(as.numeric(as.difftime(breaks_scaled * scale, units = "secs")))
    }
}




#' Format Timespan Data as "h:mm:ss"
#'
#' Convert numeric timespan data to `h:mm:ss` format for pretty plotting.
#' Modified from [ggplot2::scale_x_time()].
#'
#' @param x A numeric vector.
#'
#' @details
#' If all values are less than 3600 (1 hour), then format is returned as
#'   `mm:ss`. If any value is greater than 3600, format is returned as
#'   `h:mm:ss` with leading zeroes.
#'
#' @return A character vector the same length as `x`.
#'
#' @examples
#' \dontrun{
#' x = 0:120
#' y = sin(2 * pi * x / 15) + rnorm(length(x), 0, 0.2)
#' ggplot(data.frame(x, y)) +
#'     aes(x = x, y = y) +
#'     scale_x_continuous(
#'         breaks = breaks_timespan(),
#'         labels = format_hmmss
#'     ) +
#'     geom_line()
#' }
#'
#' @keywords internal
#' @export
format_hmmss <- function(x) {
    validate_numeric(x)
    ## logical whether to handle NAs
    handle_na <- any(is.na(x))

    if (handle_na) {
        na_info <- preserve_na(x)
        x <- na_info$x_valid
    }

    sign <- ifelse(x < 0, "-", "")
    hrs <- abs(x) %/% 3600
    mins <- (abs(x) %% 3600) %/% 60
    secs <- abs(x) %% 60

    hmmss_string <- if (any(hrs > 0, na.rm = TRUE)) {
        sprintf("%s%d:%02d:%02d", sign, hrs, mins, secs)
    } else {
        sprintf("%s%02d:%02d", sign, mins, secs)
    }

    ## return y to original x length with NAs if handled
    if (handle_na) {
        return(restore_na(hmmss_string, na_info))
    } else {
        return(hmmss_string)
    }
}
