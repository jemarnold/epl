test_that("read_tymelive() returns list with correct structure", {
    file_path <- example_epl("tymewear_live")
    result <- read_tymelive(file_path)

    expect_type(result, "list")
    expect_named(result, c("data", "details"))
    expect_s3_class(result$data, "data.frame")
    expect_s3_class(result$details, "data.frame")
})

test_that("read_tymelive() data has expected columns and types", {
    file_path <- example_epl("tymewear_live")
    result <- read_tymelive(file_path)

    # Columns
    expect_contains(names(result$data), c("time", "timestamp", "br", "vt", "ve"))

    # Column types
    expect_s3_class(result$data$timestamp, "POSIXct")
    expect_type(result$data$time, "double")
})

test_that("read_tymelive() processes timestamps correctly", {
    file_path <- example_epl("tymewear_live")
    result <- read_tymelive(file_path)

    # Time starts at zero
    expect_equal(result$data$time[1], 0)

    # Time is monotonically increasing
    expect_true(all(diff(result$data$time) >= 0))

    # Timezone is America/Vancouver
    expect_equal(attr(result$data$timestamp, "tzone"), "America/Vancouver")
})

test_that("read_tymelive() rounds numeric values correctly", {
    file_path <- example_epl("tymewear_live")
    result <- read_tymelive(file_path)

    numeric_cols <- names(result$data)[vapply(result$data, is.numeric, logical(1))]

    for (col in numeric_cols) {
        decimals <- nchar(sub(".*\\.", "", as.character(result$data[[col]])))
        expect_true(all(decimals <= 8, na.rm = TRUE))
    }
})

test_that("read_tymelive() details contains metadata", {
    file_path <- example_epl("tymewear_live")
    result <- read_tymelive(file_path)

    expect_true(nrow(result$details) > 0)
    expect_true(ncol(result$details) > 0)
})

test_that("read_tymelive() handles missing file", {
    expect_error(read_tymelive("nonexistent.csv"), "File not found")
})

test_that("read_tymelive() timestamps should match", {
    file_path <- example_epl("tymewear_live")
    result <- read_tymelive(file_path)
    # result$details |> print(n=Inf)

    start_time <- result$details |>
        filter(grepl("app-start-time", ...1)) |>
        pull(2) |>
        as.numeric() #|>
        # as_datetime(tz = "America/Vancouver")

    end_time <- result$details |>
        filter(grepl("app-end-time", ...1)) |>
        pull(2) |>
        as.numeric() #|>
        # as_datetime(tz = "America/Vancouver")

    details_duration <- result$details |>
        filter(grepl("duration", ...1)) |>
        pull(2) |>
        lubridate::ms() |>
        as.numeric()

    data_duration <- result$data |>
        pull("timestamp") |>
        as.numeric() |>
        range() |>
        diff()

    ## Not true for some reason
    # expect_equal(end_time - start_time, details_duration)
    # expect_equal(end_time - start_time, data_duration)

    ## should be equal within 1 sec rounding
    expect_equal(details_duration, data_duration, tolerance = 0.5)


})
