test_that("read_parvo() handles missing file", {
    expect_error(read_parvo("nonexistent.csv"), "File not found")
})

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
    result <- read_parvo(file_path, add_timestamp = TRUE)$data

    # Columns
    expect_contains(names(result), c("TIME", "timestamp", "VO2", "VCO2"))
    expect_true(nrow(result) > 0)

    # Column types
    expect_s3_class(result$timestamp, "POSIXct")
    expect_true(all(sapply(result[c("TIME", "VO2", "VCO2")], is.numeric)))

    # Time is monotonically increasing
    expect_true(all(diff(result$TIME) >= 0))

    # Timezone is America/Vancouver
    expect_equal(attr(result$timestamp, "tzone"), "America/Vancouver")
})

test_that("read_parvo() details has expected columns & values", {
    file_path <- example_epl("parvo_binned")
    result <- read_parvo(file_path)$details

    # Columns & rows
    expect_true(nrow(result) > 0)
    expect_true(ncol(result) > 0)

    # Values
    expect_contains(names(result), c("Date", "Name", "Sex", "Age", "Height", "Weight"))
    expect_true(all(sapply(result[c("Date", "Name", "Sex")], is.character)))
    expect_true(all(sapply(result[!names(result) %in% c("Date", "Name", "Sex")], is.numeric)))
})

test_that("read_parvo() events has expected columns and types", {
    file_path <- example_epl("parvo_binned")
    result <- read_parvo(file_path)$events

    expect_contains(names(result), c("TIME", "Events"))
    expect_true(nrow(result) >= 0)
    expect_type(result$TIME, "double")
    expect_type(result$Events, "character")

    ## handles events
    read_events <- function() {
        rows <- which(grepl("Events", data_clean[[1]]))
        rows <- ifelse(length(rows) > 0, rows, nrow(data_clean))
        events_data <- data_clean |>
            dplyr::select(!!time_column := 1, "Events" = 2) |>
            dplyr::slice(rows:nrow(data_clean)) |>
            dplyr::filter(dplyr::if_any(2, \(.x) .x != "")) |>
            dplyr::mutate(
                dplyr::across(dplyr::all_of(time_column), \(.x) as.numeric(.x) * 60)
            )
        return(events_data)
    }
    time_column <- "TIME"
    ## handles empty events
    data_clean <- tibble::tribble(
        ~ V1, ~ V2, ~ V3, ~ V4, ~ V5, ~ V6,
        "Max VO2", "8.059644", "L/min", "119.225502", "ml/kg/min", "34.06443",
        "", "", "", "", "", "",
        "", "", "", "", "", "",
        "Summary", "", "", "", "", "",
        )
    result <- read_events()
    expect_s3_class(result, "data.frame")
    expect_contains(names(result), c("TIME", "Events"))
    expect_equal(nrow(result), 0)
    expect_type(result$TIME, "double")
    expect_type(result$Events, "character")
})

test_that("read_parvo() rounds numeric values correctly", {
    file_path <- example_epl("parvo_binned")
    result <- read_parvo(file_path)
    data <- result$data

    numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]

    for (col in numeric_cols) {
        decimals <- nchar(sub(".*\\.", "", as.character(data[[col]])))
        expect_true(all(decimals <= 8, na.rm = TRUE))
    }

    details <- result$details

    numeric_cols <- names(details)[vapply(details, is.numeric, logical(1))]

    for (col in numeric_cols) {
        decimals <- nchar(sub(".*\\.", "", as.character(details[[col]])))
        expect_true(all(decimals <= 8, na.rm = TRUE))
    }
})

test_that("read_parvo() details handles name formats correctly", {
    ## empirical table with example data
    details_table <- tibble::tribble(
        ~ format, ~ V1, ~ V2, ~ V3,
        "csv", "Name", ' "WF063V1', ' WF063V1"',
        "csv", "Name", ' "O3ED 748', ' "',
        "csv", "Name", ' "BP17', ' "',
        "csv", "Name", ' "repeated_id repeated_id', ' "',
        "csv", "Name", ' "repeated_comma, repeated_comma', ' "',
        "xlsx", "Name", 'ORONSAL_ID_01_V1, ORONSAL_ID_01_V1', "File number",
        "xlsx", "Name", 'ARNOLD, JEM', "File number",
    ) |>
        dplyr::mutate(
            dplyr::across(
                dplyr::everything(), \(.x) {
                    stringr::str_squish(
                        stringr::str_replace_all(.x, "[^A-Za-z0-9.,:_\\- ]", "")
                    )
                })
        )

    search_term <- "Name"
    col_offset <- c(1:2)

    ## take function from internal helper `find_parvo_details`
    name_list <- lapply(seq_len(nrow(details_table)), \(.i) {
        details_table <- details_table[.i,]
        col <- which(grepl(search_term, details_table))
        if (length(col) == 0) {
            return(NA)
        }
        row <- which(grepl(search_term, details_table[[col]]))
        vec <- unlist(details_table[row, col + col_offset], use.names = FALSE)
        string <- paste(vec, collapse = ", ")
        ## remove trailing "," and "File number" from .xlsx cell C
        clean_string <- sub("(,\\s*File number)?\\s*,?\\s*$", "", string, ignore.case = TRUE)
        ## remove redundant duplicated string after ", "
        sub("^(.+),\\s*\\1$", "\\1", clean_string)
    })

    expect_equal(
        unlist(name_list),
        ## desired output format
        c("WF063V1", "O3ED 748", "BP17", "repeated_id repeated_id",
          "repeated_comma", "ORONSAL_ID_01_V1", "ARNOLD, JEM")
    )

})
