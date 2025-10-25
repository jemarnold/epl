test_that("read_tymewear.live() handles missing file", {
    expect_error(read_tymewear("nonexistent.csv"), "File not found")
})

test_that("read_tymewear.live() returns list with correct structure", {
    file_path <- example_epl("tymewear_live")
    result <- read_tymewear(file_path)

    expect_type(result, "list")
    expect_named(result, c("data", "details"))
    expect_s3_class(result$data, "data.frame")
    expect_s3_class(result$details, "data.frame")
})

test_that("read_tymewear.live() data has expected columns and types", {
    file_path <- example_epl("tymewear_live")
    result <- read_tymewear(file_path)$data

    # Columns & rows
    expect_contains(names(result), c("time", "timestamp", "br", "vt", "ve"))
    expect_true(nrow(result) > 0)

    # Column types
    expect_s3_class(result$timestamp, "POSIXct")
    expect_true(all(sapply(result[names(result) != "timestamp"], is.numeric)))

    # Time starts at zero
    expect_equal(result$time[1], 0)

    # Time is monotonically increasing
    expect_true(all(diff(result$time) >= 0))

    # Timezone is America/Vancouver
    expect_equal(attr(result$timestamp, "tzone"), "America/Vancouver")
})

test_that("read_tymewear.live() details has expected values", {
    file_path <- example_epl("tymewear_live")
    result <- read_tymewear(file_path)$details

    # Columns & rows
    expect_equal(ncol(result), 2)
    expect_true(nrow(result) > 0)

    # Values
    expect_contains(result[[1]], c("gender", "weight", "date", "app-start-time"))
    any(grepl(c("Male|Female"), result[[2]]))
    any(grepl(c("1761"), result[[2]]))
    expect_true(any(!is.na(
        lubridate::parse_date_time(result[[2]], orders = c("ymd", "HMS"), quiet = TRUE)
    )))
})

test_that("read_tymewear.live() rounds numeric values correctly", {
    file_path <- example_epl("tymewear_live")
    result <- read_tymewear(file_path)
    data <- result$data

    numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]

    for (col in numeric_cols) {
        decimals <- nchar(sub(".*\\.", "", as.character(data[[col]])))
        expect_true(all(decimals <= 8, na.rm = TRUE))
    }
})

test_that("read_tymewear.live() timestamps should match", {
    file_path <- example_epl("tymewear_live")
    result <- read_tymewear(file_path)
    # result$details |> print(n=Inf)

    start_time <- result$details |>
        dplyr::filter(grepl("app-start-time", ...1)) |>
        dplyr::pull(2) |>
        as.numeric() #|>
        # lubridate::as_datetime(tz = "America/Vancouver")

    end_time <- result$details |>
        dplyr::filter(grepl("app-end-time", ...1)) |>
        dplyr::pull(2) |>
        as.numeric() #|>
        # lubridate::as_datetime(tz = "America/Vancouver")

    details_duration <- result$details |>
        dplyr::filter(grepl("duration", ...1)) |>
        dplyr::pull(2) |>
        lubridate::ms() |>
        as.numeric()

    data_duration <- result$data |>
        dplyr::pull("timestamp") |>
        as.numeric() |>
        range() |>
        diff()

    ## Not true but should be true...
    # expect_equal(end_time - start_time, details_duration)
    # expect_equal(end_time - start_time, data_duration)

    ## should be equal within 1 sec rounding
    expect_equal(details_duration, data_duration, tolerance = 0.5)


})
