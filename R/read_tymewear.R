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
#' ## retrieve example tymewear file
#' file_path <- example_epl("tymewear_live")
#'
#' tyme <- read_tymewear(file_path)
#' tyme
#'
#' @export
read_tymewear <- function(file_path, ...) {
    # file_path <- r"(C:\OneDrive - UBC\Tymewear\Raw Data\TW07-tymepost-2026-01-16.csv)"
    ## validate file_path exists
    validate_file_path(file_path)
    ## read csv avoiding formatting issues
    data_raw <- read_csv_robust(file_path, n = 100)
    ## validate is data frame
    validate_data_frame(data_raw)

    ## try detecting header row to determine file type
    device_type <- if (
        !is.null(tryCatch(
            detect_header_row(data_raw, "ts"),
            error = \(e) NULL
        ))
    ) {
        "live"
    } else if (
        !is.null(tryCatch(
            detect_header_row(data_raw, "Breath by breath time"),
            error = \(e) NULL
        ))
    ) {
        "post"
    } else {
        cli::cli_abort(c(
            "{.arg file_path} = {.val {file_path}}",
            "x" = "File does not appear to be a known {.val Tymewear} export. Check file structure."
        ))
    }

    # Create object with class for method dispatch
    file_path <- structure(
        list(file_path = file_path),
        class = c(device_type, "tymewear")
    )

    UseMethod("read_tymewear", file_path)
}




#' @rdname read_tymewear
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
        ## rename columns
        dplyr::rename(parameter = 1, value = 2) |>
        ## drop pause_resume row which creates redundant columns
        dplyr::filter(!grepl("pause_resume", parameter)) |>
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
        dplyr::filter(grepl("app-start-time", parameter, ignore.case = TRUE)) |>
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
            dplyr::across(dplyr::everything(), \(.x) as.numeric(.x)),
            ## correct inflated magnitude ventilatory values
            dplyr::across("br", \(.x) .x / 10),
            dplyr::across("vt", \(.x) .x / 100),
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




#' @rdname read_tymewear
#' @export
read_tymewear.post <- function(file_path, ...) {
    ## read csv ================================
    ## validation: check file exists
    validate_file_path(file_path)

    ## read csv avoiding formatting issues
    data_raw <- read_csv_robust(file_path)

    validate_data_frame(data_raw)

    ## detect header row with "ts"
    header_row <- detect_header_row(data_raw, "Breath by breath time")

    ## details ===============================
    ## extract file details/metadata at the top of the file
    details <- data_raw |>
        dplyr::slice(1:4) |>
        ## rename columns
        dplyr::select(parameter = 1, value = 2)
    
    start_time <- as.POSIXct(
        paste(
            details$value[details$parameter == "Date"],
            details$value[details$parameter == "Start Time"]
        ),
        tryFormats = c(
            "%Y-%m-%d %H:%M:%S",
            "%Y-%d-%m %H:%M:%S"
        ),
        tz = "America/Vancouver"
    )

    end_time <- start_time +
        lubridate::hms(details$value[details$parameter == "Duration"])

    ## data table ====================================
    tyme_data <- data_raw |>
        janitor::row_to_names(header_row) |>
        suppressWarnings() |>
        ## rename and select columns
        dplyr::select(
            dplyr::any_of(
                c(
                    time = "Breath by breath time",
                    br = "BR breath by breath",
                    vt = "VT breath by breath",
                    ve = "VE breath by breath"
                )
            )
        ) |>
        ## drops rows after/including the first row with all NA
        drop_rows_after_first_na() |>
        ## force type to numeric
        dplyr::mutate(
            dplyr::across(dplyr::everything(), \(.x) as.numeric(.x)),
            ## correct inflated magnitude ventilatory values
            dplyr::across("vt", \(.x) .x / 100),
        ) |>
        suppressWarnings() |>
        ## drops empty rows
        dplyr::filter(dplyr::if_any(dplyr::everything(), \(.x) !is.na(.x))) |>
        dplyr::mutate(
            ## create timestamp
            timestamp = start_time + time,
            ## round to avoid floating point
            dplyr::across(dplyr::where(is.numeric), \(.x) round(.x, 8)),
        ) |>
        dplyr::relocate(time, timestamp)

    ## return ================================
    return(list(
        data = tyme_data,
        details = details
    ))
}


