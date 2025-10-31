#' Find peak values from rows within a given timespan
#'
#' Detects the peak values of `y` column within a `span` of `x` column (e.g.
#' timespan), returns the mean of all columns within those rows.
#'
#' @param data A data frame with at least numeric `x` and `y` columns.
#' @param x A character string for the column name of `x` (e.g. *"time"*.
#'   Must match column name exactly.
#' @param y A character string for the column name of `y` (e.g. *"VO2"*.
#'   Must match column name exactly.
#' @param span A numeric value defining timespan in which to calculate rolling
#'   local means, from which peak values will be taken. `span = 0` will take
#'   the single highest sample.
#' @param between An optional time range in the form `c(min, max)` in units of
#'   `x`, within which to look for peak values (inclusive).
#'
#' @return A [tibble][tibble::tibble-package] with one row of mean values
#'   from all columns across the peak-detected rows. `Samples` column shows
#'   the number of samples within the timespan.
#'
#' @details
#' The function uses a rolling window approach:
#'   1. For each non-NA value of `y`, calculates the mean of `y` within a
#'     window from time `t` to `t + span`.
#'   2. Identifies the window with the maximum mean value of `y`.
#'   3. Extracts all rows and columns within that peak window.
#'   4. Returns the mean of all numeric columns (and first value of
#'     non-numeric columns) within the peak window.
#'
#' @examples
#' parvo_data <- read_parvo(example_epl("parvo_ramp"))$data
#'
#' ## Find peak 30-second VO2
#' find_peaks(parvo_data, x = "TIME", y = "VO2", span = 30)
#'
#' ## search within specific time range
#' find_peaks(parvo_data, x = "TIME", y = "VO2", span = 60, between = c(1500, 2000))
#'
#' @export
find_peaks <- function(data, x, y, span = 30, between = NULL) {
    validate_data_frame(data)

    if (
        !is.character(x) || !is.character(y) || !all(c(x, y) %in% names(data))
    ) {
        cli::cli_abort(c(
            "{.val x} or {.val y} not detected in {.arg data}. \\
            Channel names are case sensitive and should match exactly."
        ))
    }

    validate_numeric(data[[x]])
    validate_numeric(data[[y]])
    validate_numeric(between, 2, msg = "two-element")

    if (!is.null(between)) {
        data <- data[between(data[[x]], between[1L], between[2L]), ]
    }

    ## vectorise data
    xvec <- data[[x]]
    yvec <- data[[y]]

    ## only x values where .t + span <= max(x)
    ## to avoid monotonically increasing signal returning last 1L sample
    xvec_range <- xvec[xvec <= max(xvec, na.rm = TRUE) - span]

    ## calculate window means of y along x
    window_means <- sapply(xvec_range, \(.t) {
        idx <- between(xvec, .t, .t + span)
        mean(yvec[idx], na.rm = TRUE)
    })

    ## for ALL invalid or static y values return first span
    if (all(!is.finite(window_means)) || all(diff(window_means) == 0)) {
        cli::cli_warn(
            "No peaks detected in {.val {y}}. Returning mean of rows in \\
            first `span = {.val {span}}` of {.val {x}}."
        )
        peak_idx <- 1L
    } else {
        ## find peak among window means
        peak_idx <- which.max(window_means)
    }

    peak_x <- xvec[peak_idx]

    ## extract peak window data for all x vals
    peak_idx_range <- between(xvec, peak_x, peak_x + span)
    peak_data <- data[peak_idx_range, ]

    ## summarise the mean of all data within the peak window
    samples <- nrow(peak_data)
    summary_data <- data.frame(
        samples = samples,
        lapply(peak_data, \(.col) {
            if (is.numeric(.col)) {
                mean(.col, na.rm = TRUE)
            } else {
                .col[1]
            }
        }),
        check.names = FALSE
    )

    ## non-finite to NA
    numeric_cols <- sapply(summary_data, is.numeric)
    summary_data[numeric_cols] <- lapply(summary_data[numeric_cols], \(.x) {
        ifelse(is.infinite(.x) | is.nan(.x), NA_real_, .x)
    })

    return(tibble::tibble(summary_data))
}
