## theme_epl() ============================================================
test_that("theme_epl returns a ggplot2 theme object", {
    theme_obj <- theme_epl()
    expect_s3_class(theme_obj, "theme")
    expect_s3_class(theme_obj, "gg")
})

test_that("theme_epl border argument works correctly", {
    partial <- theme_epl(border = "partial")
    full <- theme_epl(border = "full")

    expect_s3_class(partial$panel.border, "element_blank")
    expect_s3_class(full$panel.border, "element_rect")
})

test_that("theme_epl accepts custom colours", {
    custom <- theme_epl(ink = "red", paper = "blue", accent = "#ff0000")
    expect_s3_class(custom, "theme")
})

## palette_epl() & palette_parvo() ========================================
test_that("palettes returns correct colour vector", {
    all_colours <- palette_epl()
    expect_type(all_colours, "character")
    expect_length(all_colours, 12)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}", all_colours)))

    all_colours <- palette_parvo()
    expect_type(all_colours, "character")
    expect_length(all_colours, 28)
    expect_true(all(
        grepl("^#[0-9A-Fa-f]{6}", all_colours) | all_colours %in% colours()
    ))
})

test_that("palettes subset by number works", {
    expect_length(palette_epl(3), 3)
    expect_length(palette_epl(1), 1)
    expect_error(palette_epl(2:4))

    expect_length(palette_parvo(1), 1)
    expect_length(palette_parvo(2), 1)
    expect_length(palette_parvo(2:3), 2)
})

test_that("palettes subset by name works", {
    red <- palette_epl("red")
    expect_named(red, "red")
    expect_equal(red[["red"]], "#ED0000FF")

    multi <- palette_epl(c("red", "blue"))
    expect_length(multi, 2)
    expect_named(multi, c("red", "blue"))

    power <- palette_parvo("Power")
    expect_named(power, "Power")
    expect_equal(power[["Power"]], "#e6ce3f")

    multi <- palette_parvo("Power", "VO2")
    expect_length(multi, 2)
    expect_named(multi, c("Power", "VO2"))
})

test_that("palette_epl interpolates when n > 12", {
    many <- palette_epl(20)
    expect_length(many, 20)

    ## palette_parvo does not interpolate
    expect_error(palette_parvo(50), "Exceeded.*colours available")
    expect_error(palette_parvo(1:50), "Exceeded.*colours available")
})

## scale_colour_epl() ==================================================
test_that("scale_color_epl is an alias for scale_colour_epl", {
    expect_identical(scale_color_epl, scale_colour_epl)
})

test_that("scale_*_epl returns a ggplot2 Scale object", {
    expect_s3_class(scale_colour_epl(), "Scale")
    expect_s3_class(scale_colour_epl(), "ScaleDiscrete")

    expect_s3_class(scale_fill_epl(), "Scale")
    expect_s3_class(scale_fill_epl(), "ScaleDiscrete")
})

test_that("scale_colour_epl uses correct aesthetics", {
    expect_equal(scale_colour_epl()$aesthetics, "colour")
    expect_equal(scale_fill_epl()$aesthetics, "fill")
})

test_that("scale functions pass through additional arguments", {
    scale <- scale_colour_epl(name = "Test")
    expect_equal(scale$name, "Test")
})

test_that("scale functions use palette_epl", {
    # Extract the palette function and call it
    expect_equal(scale_colour_epl()$palette(5), palette_epl(5))
    expect_equal(scale_fill_epl()$palette(5), palette_epl(5))

    # Test with NULL argument
    expect_equal(scale_colour_epl()$palette(NULL), palette_epl(NULL))

    # Test with character argument
    expect_equal(
        scale_colour_epl()$palette(c("light blue", "dark red")),
        palette_epl(c("light blue", "dark red"))
    )

    ## test with na.value
    expect_equal(scale_colour_epl()$na.value, "grey10")
    expect_equal(scale_fill_epl()$na.value, "grey10")
})

test_that("scale functions work in ggplot2 plots", {
    p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt, colour = factor(cyl))) +
        ggplot2::geom_point() +
        scale_colour_epl()

    expect_s3_class(p, "gg")
    expect_s3_class(p$scales$get_scales("colour"), "ScaleDiscrete")
})

## breaks_timespan() ==================================================
test_that("breaks_timespan returns a function", {
    breaks_fn <- breaks_timespan()
    expect_type(breaks_fn, "closure")
})

test_that(" breaks_timespan corresponds to nice_steps for each scale level", {

    # Test scale = 1 (diff <= 5 * 60)
    nice_steps_sec <- c(1, 2, 5, 10, 15, 20, 30, 60, 120)
    x_sec <- c(0, 150)  # 2.5 min range
    breaks_sec <- breaks_timespan("secs", n = 5)(x_sec)
    steps_sec <- unique(diff(breaks_sec))
    expect_true(all(steps_sec %in% nice_steps_sec))
    expect_type(breaks_sec, "double")
    expect_true(all(breaks_sec >= 0 & breaks_sec <= 150))
    expect_equal(length(breaks_sec), 5, tolerance = 2)

    # Test scale = 60 (5 * 60 < diff <= 5 * 3600)
    nice_steps_min <- c(1, 2, 5, 10, 15, 20, 30, 60, 120) * 60
    x_min <- c(0, 7200)  # 2 hour range
    breaks_min <- breaks_timespan("secs", n = 5)(x_min)
    steps_min <- unique(diff(breaks_min))
    expect_true(all(steps_min %in% nice_steps_min))
    expect_type(breaks_min, "double")
    expect_true(all(breaks_min >= 0 & breaks_min <= 7200))
    expect_equal(length(breaks_min), 5, tolerance = 2)

    # Test scale = 3600 (5 * 3600 < diff <= 5 * 86400)
    nice_steps_hr <- c(0.25, 0.5, 1, 2, 3, 4, 6, 8, 12, 24) * 3600
    x_hr <- c(0, 86400)  # 1 day range
    breaks_hr <- breaks_timespan("secs", n = 5)(x_hr)
    steps_hr <- unique(diff(breaks_hr))
    expect_true(all(steps_hr %in% nice_steps_hr))
    expect_type(breaks_hr, "double")
    expect_true(all(breaks_hr >= 0 & breaks_hr <= 86400))
    expect_equal(length(breaks_hr), 5, tolerance = 2)

    # Test scale = 86400 (diff > 5 * 86400)
    nice_steps_day <- c(1, 7, 28) * 86400
    x_day <- c(0, 86400 * 28)  # 28 day range
    breaks_day <- breaks_timespan("secs", n = 5)(x_day)
    steps_day <- unique(diff(breaks_day))
    expect_true(all(steps_day %in% nice_steps_day))
    expect_type(breaks_day, "double")
    expect_true(all(breaks_day >= 0 & breaks_day <= 86400 * 28))
    expect_equal(length(breaks_day), 5, tolerance = 2)
})

## format_hmmss() ==================================================
test_that("format_hmmss formats time correctly", {
    ## seconds
    expect_equal(format_hmmss(0), "00:00")
    expect_equal(format_hmmss(30), "00:30")
    expect_equal(format_hmmss(90), "01:30")
    expect_equal(format_hmmss(3599), "59:59")

    ## hours
    expect_equal(format_hmmss(3600), "1:00:00")
    expect_equal(format_hmmss(3661), "1:01:01")
    expect_equal(format_hmmss(7325), "2:02:05")

    ## negative
    expect_equal(format_hmmss(-30), "-00:30")
    expect_equal(format_hmmss(-3661), "-1:01:01")
})

test_that("format_hmmss handles vectors", {
    result <- format_hmmss(c(0, 30, 90, 3600))
    expect_length(result, 4)
    expect_equal(result, c("0:00:00", "0:00:30", "0:01:30", "1:00:00"))
})

test_that("format_hmmss handles NA values", {
    result <- format_hmmss(c(30, NA, 90))
    expect_length(result, 3)
    expect_true(is.na(result[2]))
})
