test_that("find_peaks identifies correct peak window", {
    data <- tibble::tibble(
        time = 0:100,
        VO2 = c(rep(10, 30), rep(20, 30), rep(15, 41))
    )

    result <- find_peaks(data, x = "time", y = "VO2", span = 10)

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 1)
    expect_true(result$VO2 > 19 && result$VO2 <= 20)
})

test_that("find_peaks handles invalid values correctly", {
    data <- tibble::tibble(
        time = 0:50,
        VO2 = c(rep(10, 10), NA, NA, rep(20, 20), rep(15, 19))
    )

    result <- find_peaks(data, "time", "VO2", span = 10)

    expect_false(is.na(result$VO2))
    expect_true(result$VO2 > 15)

    data <- tibble::tibble(
        time = 0:50,
        VO2 = c(rep(10, 20), rep(20, 20), rep(15, 11)),
        infinite_col = Inf
    )

    ## invalid to NA for secondary columns
    result <- find_peaks(data, "time", "VO2", span = 10)
    expect_true(is.na(result$infinite_col))

    ## returns warning and takes first time span for y = invalid
    expect_warning(
        result <- find_peaks(data, y = "infinite_col", x = "time", span = 10),
        "No peaks detected.*span = .*10")

    expect_equal(result$samples, 11)
    expect_equal(result$time, mean(data$time[data$time <= 10]))

    ## static VO2 should return warning
    data <- tibble::tibble(time = 0:50, VO2 = rep(10, 51))

    expect_warning(
        result <- find_peaks(data, "time", "VO2", span = 10),
        "No peaks detected.*span = .*10")

    expect_equal(result$samples, 11)
    expect_equal(result$time, mean(data$time[data$time <= 10]))
})

test_that("find_peaks at end of file returns correct number of samples", {
    data <- tibble::tibble(time = 0:50, VO2 = 0:50)

    result <- find_peaks(data, "time", "VO2", span = 10)
    expect_equal(result$samples, 11)
    expect_equal(result$time, mean(tail(data$time, n = 11)))
    expect_equal(result$VO2, mean(tail(data$VO2, n = 11)))
})

test_that("find_peaks returns correct number of samples", {
    data <- tibble::tibble(
        time = seq(0, 100, by = 0.5),
        VO2 = rnorm(201, mean = 3000)
    )

    result <- find_peaks(data, "time", "VO2", span = 10)

    expect_true("samples" %in% names(result))
    expect_type(result$samples, "integer")
    expect_true(result$samples >= 20 && result$samples <= 21)
})

test_that("find_peaks respects between argument", {
    data <- tibble::tibble(
        time = 0:100,
        VO2 = c(rep(50, 20), rep(10, 50), rep(40, 31))
    )

    result <- find_peaks(data, "time", "VO2", span = 10,
                         between = c(30, 100))

    expect_true(result$VO2 < 45)
})

test_that("find_peaks summarises all numeric columns", {
    data <- tibble::tibble(
        time = 0:100,
        VO2 = c(rep(10, 30), rep(20, 30), rep(15, 41)),
        HR = c(rep(120, 30), rep(160, 30), rep(140, 41)),
        VE = c(rep(50, 30), rep(80, 30), rep(60, 41))
    )

    result <- find_peaks(data, "time", "VO2", span = 10)

    expect_true(all(c("VO2", "HR", "VE") %in% names(result)))
    expect_true(result$HR > 150)
    expect_true(result$VE > 70)
})

test_that("find_peaks handles non-numeric columns", {
    data <- tibble::tibble(
        time = 0:50,
        VO2 = c(rep(10, 20), rep(20, 20), rep(15, 11)),
        condition = "test",
    )

    result <- find_peaks(data, "time", "VO2", span = 10)

    expect_true("condition" %in% names(result))
    expect_equal(result$condition, "test")
})

test_that("find_peaks errors with invalid column names", {
    data <- tibble::tibble(time = 0:50, VO2 = rnorm(51))

    expect_error(
        find_peaks(data, y = "invalid", x = "time", span = 10),
        "not detected"
    )
    expect_error(
        find_peaks(data, y = "VO2", x = "invalid", span = 10),
        "not detected"
    )
    expect_error(
        find_peaks(data, y = NA, x = "invalid", span = 10),
        "not detected"
    )
    expect_error(
        find_peaks(data, y = NULL, x = "invalid", span = 10),
        "not detected"
    )
    expect_error(
        find_peaks(data, y = 1, x = "invalid", span = 10),
        "not detected"
    )
})

test_that("find_peaks errors with invalid between argument", {
    data <- tibble::tibble(time = 0:50, VO2 = rnorm(51))

    expect_error(
        find_peaks(data, "time", "VO2", span = 10, between = c(10)),
        "must be a valid.*?numeric"
    )

    expect_error(
        find_peaks(data, "time", "VO2", span = 10, between = c(10, 20, 30)),
        "must be a valid.*?numeric"
    )
})

test_that("find_peaks works with different span values", {
    data <- tibble::tibble(
        time = 0:100,
        VO2 = c(rep(10, 30), rep(20, 30), rep(15, 41))
    )

    result_short <- find_peaks(data, "time", "VO2", span = 5)
    result_long <- find_peaks(data, "time", "VO2", span = 30)

    expect_true(result_short$samples < result_long$samples)
})

test_that("find_peaks handles single peak correctly", {
    data <- tibble::tibble(time = 1:20, VO2 = c(0:10, 9:1))

    result <- find_peaks(data, x = "time", y = "VO2", span = 0, between = NULL)

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 1)
})
