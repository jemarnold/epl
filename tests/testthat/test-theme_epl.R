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

test_that("palette_epl returns correct colour vector", {
    all_colours <- palette_epl()
    expect_type(all_colours, "character")
    expect_length(all_colours, 12)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}", all_colours)))
})

test_that("palette_epl subset by number works", {
    expect_length(palette_epl(3), 3)
    expect_length(palette_epl(1), 1)
    expect_error(palette_epl(2:4))
})

test_that("palette_epl subset by name works", {
    red <- palette_epl("red")
    expect_named(red, "red")
    expect_equal(red[["red"]], "#ED0000FF")

    multi <- palette_epl(c("red", "blue"))
    expect_length(multi, 2)
    expect_named(multi, c("red", "blue"))
})

test_that("palette_epl interpolates when n > 12", {
    many <- palette_epl(20)
    expect_length(many, 20)
})

test_that("breaks_timespan returns a function", {
    breaks_fn <- breaks_timespan()
    expect_type(breaks_fn, "closure")
})

test_that("breaks_timespan generates appropriate breaks", {
    breaks_fn <- breaks_timespan(n = 5)

    # Test seconds scale (< 5 min)
    breaks_sec <- breaks_fn(c(0, 120))
    expect_type(breaks_sec, "double")
    expect_true(all(breaks_sec >= 0 & breaks_sec <= 120))
    expect_equal(length(breaks_sec), 5, tolerance = 2)

    # Test minutes scale (< 5 hr)
    breaks_min <- breaks_fn(c(0, 600))
    expect_true(all(breaks_min >= 0 & breaks_min <= 600))
    expect_equal(length(breaks_min), 5, tolerance = 2)

    # Test hours scale
    breaks_hr <- breaks_fn(c(0, 7200))
    expect_true(all(breaks_hr >= 0 & breaks_hr <= 7200))
    expect_equal(length(breaks_hr), 5, tolerance = 2)
})

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
