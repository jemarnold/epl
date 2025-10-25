test_that("read_parvo() returns list with correct structure", {
    file_path <- example_epl("parvo_binned")
    result <- read_parvo(file_path)

    expect_type(result, "list")
    expect_named(result, c("data", "details", "events"))
    expect_s3_class(result$data, "data.frame")
    expect_s3_class(result$details, "data.frame")
    expect_s3_class(result$events, "data.frame")
})

test_that("read_parvo() data has expected columns and types", {
    file_path <- example_epl("parvo_binned")
    result <- read_parvo(file_path, add_timestamp = TRUE)

    # Columns
    expect_contains(names(result$data), c("TIME", "timestamp", "VO2", "VCO2"))

    # Column types
    expect_s3_class(result$data$timestamp, "POSIXct")
    expect_type(result$data$TIME, "double")
})

test_that("read_parvo() processes timestamps correctly", {
    file_path <- example_epl("parvo_binned")
    result <- read_parvo(file_path, add_timestamp = TRUE)

    # Time is monotonically increasing
    expect_true(all(diff(result$data$TIME) >= 0))

    # Timezone is America/Vancouver
    expect_equal(attr(result$data$timestamp, "tzone"), "America/Vancouver")
})

test_that("read_parvo() rounds numeric values correctly", {
    file_path <- example_epl("parvo_binned")
    result <- read_parvo(file_path)

    numeric_cols <- names(result$data)[vapply(result$data, is.numeric, logical(1))]

    for (col in numeric_cols) {
        decimals <- nchar(sub(".*\\.", "", as.character(result$data[[col]])))
        expect_true(all(decimals <= 8, na.rm = TRUE))
    }
})

test_that("read_parvo() details contains metadata", {
    file_path <- example_epl("parvo_binned")
    result <- read_parvo(file_path)

    expect_true(nrow(result$details) > 0)
    expect_true(ncol(result$details) > 0)
})

test_that("read_parvo() handles missing file", {
    expect_error(read_parvo("nonexistent.csv"), "File not found")
})

