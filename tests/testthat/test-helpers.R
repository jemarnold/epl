## validate_data_frame() ==================================================
test_that("validate_data_frame works", {
    expect_silent(validate_data_frame(mtcars))

    expect_error(validate_data_frame(1:10), "`data` should be a data frame")
    expect_error(validate_data_frame(matrix(1:9, 3, 3)), "`data` should be a data frame")
    expect_error(validate_data_frame(list(x = 1, y = 2)), "`data` should be a data frame")
    expect_error(validate_data_frame(NULL), "`data` should be a data frame")
})

## drop_rows_after_first_na() =============================================
test_that("drop_rows_after_first_na removes rows after first all-NA row", {
    df <- data.frame(x = c(1, 2, NA, 4), y = c(10, 20, NA, 40))
    result <- drop_rows_after_first_na(df)
    expect_equal(nrow(result), 2)
    expect_equal(result$x, c(1, 2))
})

test_that("drop_rows_after_first_na handles no all-NA rows", {
    df <- data.frame(x = c(1, 2, NA, 4), y = c(10, 20, 30, 40))
    result <- drop_rows_after_first_na(df)
    expect_identical(result, df)
})

test_that("drop_rows_after_first_na handles first row all-NA", {
    df <- data.frame(x = c(NA, 2, 3), y = c(NA, 20, 30))
    result <- drop_rows_after_first_na(df)
    expect_equal(nrow(result), 0)
    expect_equal(ncol(result), 2)
})


test_that("drop_rows_after_first_na handles multiple all-NA rows", {
    df <- data.frame(x = c(1, 2, NA, NA), y = c(10, 20, NA, NA))
    result <- drop_rows_after_first_na(df)
    expect_equal(nrow(result), 2)
    expect_equal(result$y, c(10, 20))
})

test_that("drop_rows_after_first_na preserves structure with no NAs", {
    df <- data.frame(x = 1:5, y = 6:10, z = letters[1:5])
    result <- drop_rows_after_first_na(df)
    expect_identical(result, df)
    expect_type(result$z, "character")
})

test_that("drop_rows_after_first_na handles structure with all NAs", {
    df <- data.frame(x = NA, y = NA)
    result <- drop_rows_after_first_na(df)
    expect_equal(nrow(result), 0)
    expect_equal(ncol(result), 2)
})

## between() ===============================================================
test_that("between() handles basic inclusive range (default)", {
    expect_equal(between(5, 1, 10), TRUE)
    expect_equal(between(1, 1, 10), TRUE)
    expect_equal(between(10, 1, 10), TRUE)
    expect_equal(between(0, 1, 10), FALSE)
    expect_equal(between(11, 1, 10), FALSE)
})

test_that("between() handles vectorised inputs", {
    expect_equal(
        between(c(0, 1, 5, 10, 11), 1, 10),
        c(FALSE, TRUE, TRUE, TRUE, FALSE)
    )
    expect_equal(
        between(1:10, 3, 7),
        c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
    )
})

test_that("between() handles invalid `left` and `right` values", {
    expect_error(
        between(1:5, c(2, 3), 4),
        "valid one-element.*numeric"
    )
    expect_error(
        between(1:5, 3, c(4, 5)),
        "valid one-element.*numeric"
    )
})

test_that("between() handles inclusive/exclusive", {
    ## both exclusive
    expect_false(between(1, 1, 10, inclusive = FALSE))
    expect_false(between(10, 1, 10, inclusive = FALSE))
    expect_true(between(5, 1, 10, inclusive = FALSE))
    expect_equal(between(c(1, 5, 10), 1, 10, inclusive = FALSE), c(FALSE, TRUE, FALSE))

    ## left inclusive
    expect_true(between(1, 1, 10, inclusive = "left"))
    expect_false(between(10, 1, 10, inclusive = "left"))
    expect_equal(between(c(1, 5, 10), 1, 10, inclusive = "left"), c(TRUE, TRUE, FALSE))

    ## right inclusive
    expect_false(between(1, 1, 10, inclusive = "right"))
    expect_true(between(10, 1, 10, inclusive = "right"))
    expect_equal(between(c(1, 5, 10), 1, 10, inclusive = "right"), c(FALSE, TRUE, TRUE))
})

test_that("between() detects positive non-zero values", {
    expect_true(between(0, 0, Inf))
    expect_true(between(Inf, 0, Inf))
    expect_false(between(0, 0, Inf, inclusive = FALSE))
    expect_true(between(1, 0, Inf, inclusive = FALSE))
    expect_false(between(-1, 0, Inf))
    expect_false(between(-1, 0, Inf, inclusive = FALSE))
    expect_equal(
        between(c(-1, 0, 0.001, 1), 0, Inf, inclusive = FALSE),
        c(FALSE, FALSE, TRUE, TRUE)
    )
})

test_that("between() handles NA values", {
    expect_error(between(NA, 1, 10), "`x`.*valid.*numeric")
    expect_equal(between(c(1, NA, 5), 1, 10), c(TRUE, NA, TRUE))
    expect_error(between(5, NA, 10), "`left`.*valid.*numeric")
    expect_error(between(5, 1, NA), "`right`.*valid.*numeric")
})

test_that("between() handles infinite values", {
    expect_true(between(Inf, 1, Inf), TRUE)
    expect_false(between(Inf, 1, Inf, inclusive = FALSE), FALSE)
    expect_true(between(-Inf, -Inf, 10), TRUE)
    expect_false(between(-Inf, -Inf, 10, inclusive = FALSE), FALSE)
})

test_that("between() handles negative ranges", {
    expect_true(between(-5, -10, -1))
    expect_true(between(-10, -10, -1))
    expect_false(between(-11, -10, -1))
})

test_that("between() handles degenerate ranges where left equals right", {
    expect_true(between(5, 5, 5), TRUE)
    expect_false(between(5, 5, 5, inclusive = FALSE))
    expect_false(between(5, 5, 5, inclusive = "left"))
    expect_false(between(5, 5, 5, inclusive = "right"))
})
