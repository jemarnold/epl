#' Read Tymewear Exported CSV Data
#'
#' Read *"Live Processed CSV"* or *"Post Processed CSV"* files exported from
#' *Tymewear VitalPro* and return a list of two data frames with recorded data
#' and file details
#'
#' @param file_path The file path as a character string, including *.csv*
#'   file type.
#' @param ... Additional arguments passed to `read_tymewear`.
#'
#' @details
#' This generic function detects *Tymewear* exported file type by searching the
#'   top of the file for recognisable column data belonging to either
#'   *"tymewear.live"* or *"tymewear.post"*, and will read and import the
#'   resulting file.
#'
#' @return A list with two [tibbles][tibble::tibble-package].
#' - `tymelive$data` contains the data table.
#' - `tymelive$details` contains the file metadata.
#'
#' @examples
#' # retrieve example tymewear file
#' file_path <- example_epl("tymewear_live")
#' tyme <- read_tymewear(file_path)
#' tyme
#'
#' @export
read_tymewear <- function(file_path, ...) {
    ## validate file_path exists
    validate_file_path(file_path)
    ## read csv avoiding formatting issues
    data_raw <- read_csv_robust(file_path, n = 100)
    ## validate is data frame
    validate_data_frame(data_raw)

    ## try detecting header row to determine file type
    is_tymelive <- tryCatch(
        detect_header_row(data_raw, "ts", 100, 20),
        error = \(e) NULL
    )

    is_tymepost <- tryCatch(
        detect_header_row(data_raw, "Time", 50, 5),
        error = \(e) NULL
    )

    device_type <- if (is.numeric(is_tymelive)) {
        "live"
    } else if (is.numeric(is_tymepost)) {
        "post"
    } else {
        cli::cli_abort(c(
            "{.arg file_path} = {.val {file_path}}",
            "x" = "File does not appear to be a known {.val Tymewear} \\
            export. Check file structure."
        ))
    }

    # Create object with class for method dispatch
    file_path <- structure(
        list(file_path = file_path),
        class = c(device_type, "tymewear")
    )

    UseMethod("read_tymewear", file_path)
}



#' @export
read_tymewear.live <- function(file_path, ...) {
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
        dplyr::slice(1:(header_row - 3)) |>
        ## drop pause_resume row which creates redundant columns
        dplyr::filter(!grepl("pause_resume", ...1)) |>
        ## drops empty columns
        dplyr::select(
            dplyr::where(\(.x) any(nzchar(.x) & !is.na(.x)))
        ) |>
        ## drops empty rows
        dplyr::filter(
            dplyr::if_any(dplyr::everything(), \(.x) nzchar(.x) & !is.na(.x))
        )

    ## pull start time from details and correct for local timezone
    start_dttm <- details |>
        dplyr::filter(grepl("app-start-time", ...1, ignore.case = TRUE)) |>
        dplyr::pull(2) |>
        as.numeric() |>
        lubridate::as_datetime(tz = "America/Vancouver")

    ## data table ====================================
    tyme_data <- data_raw |>
        janitor::row_to_names(header_row) |>
        suppressWarnings() |>
        ## drops empty columns introduced by horizontal data at the bottom
        (\(.df) {
            .df |>
                dplyr::select(
                    dplyr::all_of(seq_len(which(names(.df) == "")[1] - 1))
                )
        })() |>
        ## rename and select columns
        dplyr::select(
            dplyr::any_of(
                c(timestamp = "ts", br = "rbr", vt = "rvt", ve = "rve")
            )
        ) |>
        ## force type to numeric
        dplyr::mutate(
            dplyr::across(dplyr::everything(), \(.x) as.numeric(.x))
        ) |>
        suppressWarnings() |>
        ## drops rows after/including the first row with all NA
        drop_rows_after_first_na() |>
        dplyr::mutate(
            ## convert timestamp to correct time zone and create time column
            timestamp = lubridate::as_datetime(
                timestamp / 1000,
                tz = "America/Vancouver"
            ),
            time = as.numeric(timestamp - timestamp[1]),
            ## convert blank values to NA
            dplyr::across(
                dplyr::where(is.numeric),
                \(.x) dplyr::if_else(!is.finite(.x), NA_real_, .x)
            ),
            ## round to avoid floating point
            dplyr::across(
                dplyr::where(is.numeric), \(.x) round(.x, 8)
            ),
        ) |>
        dplyr::relocate(time, timestamp)

    ## return ================================
    return(list(
        data = tyme_data,
        details = details
    ))
}




#' @export
read_tymewear.post <- function(file_path, ...) {
    cli::cli_abort(c(
        "{.arg file_path} = {.val {file_path}}",
        "!" = 'Class = c({.val tymewear}, {.val post})',
        "x" = "Read method currently under development for class: {.val post}"
    ))
}
