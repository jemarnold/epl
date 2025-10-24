#' Read Tymewear Exported CSV Data
#'
#' Read from exported *"Live Processed CSV"* or *"Post Processed CSV"* files
#' and return recorded data table and file metadata.
#'
#' @param file_path The file path as a character string, including *.csv*
#' file type.
#'
#' @return A list with two [tibbles][tibble::tibble-package].
#' - `tymelive$data` contains the data table.
#' - `tymelive$details` contains the file metadata.
#'
#' @import tibble dplyr tidyr
#'
#' @name read_tymewear
NULL


#' @rdname read_tymewear
#' @order 1
#' @export
read_tymelive <- function(file_path) {
    ## read csv ================================
    ## validation: check file exists
    if (!file.exists(file_path)) {
        cli_abort(c(
            "{.arg file_path} = {.val {file_path}}",
            "x" = "File not found. Check that file exists."
        ))
    }

    ## read csv avoiding formatting issues
    data_raw <- read_csv_with_spaces(file_path)

    ## detect header row with "ts"
    header_row <- detect_header_row(data_raw, "ts", 100)

    ## details ===============================
    ## extract file details/metadata at the top of the file
    details <- data_raw |>
        slice(1:(header_row - 3)) |>
        ## drop pause_resume row which creates redundant columns
        filter(!grepl("pause_resume", ...1)) |>
        ## drops empty columns
        select(where(\(.x) any(nzchar(.x) & !is.na(.x)))) |>
        ## drops empty rows
        filter(if_any(everything(), \(.x) nzchar(.x) & !is.na(.x)))

    ## pull start time from details and correct for local timezone
    start_dttm <- details |>
        filter(grepl("app-start-time", ...1, ignore.case = TRUE)) |>
        pull(2) |>
        as.numeric() |>
        as_datetime(tz = "America/Vancouver")

    ## data table ====================================
    tyme_data <- data_raw |>
        row_to_names(header_row) |>
        suppressWarnings() |>
        ## drops empty columns introduced by horizontal data at the bottom
        (\(.df) {
            select(.df, all_of(seq_len(which(names(.df) == "")[1] - 1)))
        })() |>
        ## rename and select columns
        select(any_of(c(
            timestamp = "ts", BR = "rbr", Vt = "rvt", VE = "rve"
        ))) |>
        ## force type to numeric
        mutate(across(everything(), \(.x) as.numeric(.x))) |>
        suppressWarnings() |>
        ## drops rows after/including the first row with all NA
        (\(.df) {
            first_allna <- which(rowSums(!is.na(.df)) == 0)[1]

            if (!is.na(first_allna)) {
                slice(.df, seq_len(first_allna - 1))
            }
        })() |>
        mutate(
            ## convert timestamp to correct time zone and create time column
            timestamp = as_datetime(timestamp/1000, tz = "America/Vancouver"),
            time = as.numeric(timestamp - timestamp[1]),
            ## round to avoid floating point
            across(where(is.numeric), \(.x) round(.x, 5)),
        ) |>
        relocate(time, timestamp)

    ## return ================================
    return(list(
        data = tyme_data,
        details = details
    ))
}


#' @importFrom stringr str_split_fixed
#' @importFrom readr read_lines
#' @keywords internal
read_csv_with_spaces <- function(file_path) {
    ## read csv as raw lines. Avoids issues with multiple empty rows & columns
    read_lines(file_path) |>
        str_split_fixed(",", n = Inf) |> ## don't print this it takes ages!
        as_tibble(.name_repair = "unique_quiet")
}

# read_csv_with_spaces_base <- function(file_path) {
#     lines <- readLines(file_path, warn = FALSE)
#     splits <- strsplit(lines, ",", fixed = TRUE)
#     max_cols <- max(lengths(splits))
#     t(vapply(splits, `[`, character(max_cols), seq_len(max_cols))) |>
#         as_tibble(.name_repair = "unique_quiet")
# }




#' @keywords internal
detect_header_row <- function(data, label, max_row = 100) {
    ## detect header row with "label"
    header_match <- apply(
        data[seq_len(min(max_row, nrow(data))), , drop = FALSE],
        1, \(.row) any(.row == label, na.rm = TRUE)
    )
    header_row <- which(header_match)[1]

    ## validation: "label" must be detected to extract the proper data frame
    if (is.na(header_row) || length(header_row) != 1) {
        cli_abort("Error detecting data frame. Check file structure.")
        cli_abort(c(
            "Error detecting {.val {label}}.",
            "i" = "Strings are case sensitive and should match exactly. \\
            Check file structure."
        ))
    }

    return(header_row)
}
