# Test replace_outliers() --------------------------------------------------
test_that("replace_outliers() returns unchanged vector with no outliers", {
    x <- 1:20
    result <- replace_outliers(x, width = 3)

    expect_equal(result, x)
})

test_that("replace_outliers() detects and replaces outliers with median", {
    x <- c(1:10, 100, 11:20)  # 100 is clear outlier
    result <- replace_outliers(x, width = 3)

    expect_type(result, "double")
    expect_length(result, length(x))
    expect_true(result[11] != 100)  # outlier replaced
    expect_true(result[11] == median(result[c(8:10, 12:14)]))  # outlier replaced
    expect_false(any(is.na(result)))
})

test_that("replace_outliers() detects and replaces outliers with NA", {
    x <- c(1:10, 100, 11:20)
    result <- replace_outliers(x, method = "NA", width = 3)

    expect_length(result, length(x))
    expect_true(is.na(result[11]))  # outlier replaced with NA
})

test_that("replace_outliers() handles NA values in input", {
    x <- c(1:5, NA, 7:10, 100, 12:15, NA, 17:20)
    result <- replace_outliers(x, width = 3)

    expect_length(result, length(x))
    expect_true(result[11] != 100)  # outlier replaced
    expect_true(is.na(result[6]))  # original NA preserved
    expect_true(is.na(result[16]))  # original NA preserved
})

test_that("replace_outliers() respects t0 threshold", {
    x <- c(1:10, 15, 11:20)  # mild outlier

    strict <- replace_outliers(x, width = 3, t0 = 1)
    lenient <- replace_outliers(x, width = 3, t0 = 5)

    expect_true(strict[11] != 15)  # detected with strict threshold
    expect_equal(lenient[11], 15)  # not detected with lenient threshold
})

test_that("replace_outliers() validates inputs correctly", {
    x <- 1:10

    expect_error(replace_outliers("text", width = 3), "x.*?numeric")  # non-numeric x
    expect_error(replace_outliers(x, method = "NA", width = -1), "width.*?integer")  # negative width
    expect_error(replace_outliers(x, method = "NA", width = 10), "width.*?half")  # width >= length(x)/2
    expect_error(replace_outliers(x, method = "NA", width = 3, t0 = -1), "t0.*?integer")  # negative t0

    ## haldes all NA
    x <- rep(NA_real_, 10)
    expect_error(replace_outliers(x, width = 3), "x.*?numeric") ## x is all NA

    ## handles all same values
    expect_equal(replace_outliers(c(1, 1, 1), width = 1), rep(1, 3))
})


# Test preserve_na() -------------------------------------------------------
test_that("preserve_na() correctly stores NA positions and clean values", {
    x <- c(1, NA, 3, NA, 5)
    na_info <- preserve_na(x)

    expect_type(na_info, "list")
    expect_named(na_info, c("x_valid", "x_length", "na_idx"))
    expect_equal(na_info$x_valid, x[!is.na(x)])
    expect_equal(na_info$x_length, 5)
    expect_equal(na_info$na_idx, is.na(x))
})

test_that("preserve_na() handles vector with no NA", {
    x <- 1:5
    na_info <- preserve_na(x)

    expect_equal(na_info$x_valid, x)
    expect_equal(na_info$x_length, 5)
    expect_equal(na_info$na_idx, is.na(x))
})

test_that("preserve_na() handles vector with all NA", {
    x <- rep(NA_real_, 5)
    na_info <- preserve_na(x)

    expect_length(na_info$x_valid, 0)
    expect_equal(na_info$x_length, 5)
    expect_equal(na_info$na_idx, is.na(x))
})


# Test restore_na() --------------------------------------------------------
test_that("restore_na() correctly restores NA positions", {
    x <- c(1, NA, 3, NA, 5)
    na_info <- preserve_na(x)
    y <- na_info$x_valid * 2  ## y_valid = c(2, 6, 10)
    result <- restore_na(y, na_info)

    expect_length(result, 5)
    expect_equal(result, x*2)
})

test_that("restore_na() handles no NA in original vector", {
    x <- 1:5
    na_info <- preserve_na(x)
    y <- na_info$x_valid * 2
    result <- restore_na(y, na_info)

    expect_equal(result, x*2)
})

test_that("restore_na() handles all NA in original vector", {
    x <- rep(NA_real_, 5)
    na_info <- preserve_na(x)
    y <- na_info$x_valid  # no clean values to process
    result <- restore_na(y, na_info)

    expect_length(result, 5)
    expect_true(all(is.na(result)))
})


# Integration test ---------------------------------------------------------
test_that("preserve_na() and restore_na() work together correctly", {
    x <- c(1, NA, 3, NA, 5, 6, NA, 8)
    na_info <- preserve_na(x)

    # Simulate processing
    y_processed <- na_info$x_valid + 10
    result <- restore_na(y_processed, na_info)

    expect_length(result, length(x))
    expect_equal(result[!is.na(x)], x[!is.na(x)] + 10)
    expect_true(is.na(result[2]))
    expect_true(is.na(result[4]))
    expect_true(is.na(result[7]))
})
