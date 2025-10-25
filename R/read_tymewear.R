#' Read Tymewear Exported CSV Data
#'
#' Read *"Live Processed CSV"* or *"Post Processed CSV"* files exported from
#' Tymewear VitalPro and return recorded data table and file metadata.
#'
#' @param file_path The file path as a character string, including *.csv*
#' file type.
#'
#' @return A list with two [tibbles][tibble::tibble-package].
#' - `tymelive$data` contains the data table.
#' - `tymelive$details` contains the file metadata.
#'
#'
#' @name read_tymewear
NULL


#' @rdname read_tymewear
#' @order 1
#' @export
## file_path <- example_epl("tymewear_live")
read_tymelive <- function(file_path) {
    ## read csv ================================
    ## validation: check file exists
    validate_file_path(file_path)

    ## read csv avoiding formatting issues
    data_raw <- read_csv_robust(file_path)

    validate_data_frame(data_raw)

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
            timestamp = "ts", br = "rbr", vt = "rvt", ve = "rve"
        ))) |>
        ## force type to numeric
        mutate(across(everything(), \(.x) as.numeric(.x))) |>
        suppressWarnings() |>
        ## drops rows after/including the first row with all NA
        drop_rows_after_first_na() |>
        mutate(
            ## convert timestamp to correct time zone and create time column
            timestamp = as_datetime(timestamp / 1000, tz = "America/Vancouver"),
            time = as.numeric(timestamp - timestamp[1]),
            ## convert blank values to NA
            across(
                where(is.numeric),
                \(.x) if_else(!is.finite(.x), NA_real_, .x)
            ),
            ## round to avoid floating point
            across(where(is.numeric), \(.x) round(.x, 8)),
        ) |>
        relocate(time, timestamp)

    ## return ================================
    return(list(
        data = tyme_data,
        details = details
    ))
}



