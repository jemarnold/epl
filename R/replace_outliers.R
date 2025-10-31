#' Replace Local Outliers
#'
#' Detects local outliers in vector data with a Hampel filter using median
#' absolute deviation (MAD), and replaces with `NA` or the local median value.
#'
#' @param x A numeric vector.
#' @param width An integer defining the sample window in which to detect local
#'   outliers. Where `window = -width < idx < width`.
#' @param method A character string indicating how to handle replacement
#'   (see *Details* for more on each method):
#'   \describe{
#'      \item{`"median"`}{Replaces outliers with the median within a locally
#'      centred window defined by `width` (the default).}
#'      \item{`"NA"`}{Replaces outliers with `NA`.}
#'   }
#' @param t0 An integer for the local outlier threshold. Default `t0 = 3`
#'   (Pearson's rule; analogous to 3σ rule).
#'
#' @details
#' The default `method = "median"` will replace outliers with the local median
#'   value, as in [pracma::hampel()]. Otherwise, `method = "NA"` will
#'   replace outliers with `NA`.
#'
#' This function will pass through any missing `NA` values in the input vector
#'   `x`. `NA` values in `x` are excluded from processing and restored in the
#'   returned vector, but not replaced with the local median value.
#'
#' A high `t0` threshold makes the outlier filter more forgiving, a low one
#'   will declare more points to be outliers. `t0 = 3` (the default)
#'   corresponds to Pearson's 3 sigma edit rule, `t0 = 0` to Tukey's median
#'   filter.
#'
#' @return A numeric vector of filtered data.
#'
#' @seealso [pracma::hampel()]
#'
#' @examples
#' tyme_data <- read_tymewear(example_epl("tymewear_live"))$data
#' vt_filtered <- replace_outliers(tyme_data$vt, width = 7, method = "median")
#'
#' \dontrun{
#' ggplot(tyme_data) +
#'     aes(x = time, y = vt) +
#'     ylab("Tidal Volume (L)") +
#'     scale_x_continuous(
#'         name = "Time (mm:ss)",
#'         breaks = breaks_timespan(),
#'         labels = format_hmmss
#'     ) +
#'     scale_colour_epl() +
#'     geom_line(aes(colour = "BR")) +
#'     geom_point(
#'         data = slice(tyme_data, which(vt_filtered != vt)),
#'         aes(y = vt, colour = "outliers")
#'     )
#' }
#'
#' @export
replace_outliers <- function(
        x,
        width,
        method = c("median", "NA"),
        t0 = 3
) {
    validate_numeric(x)
    validate_numeric(
        width, 1, c(1, Inf), integer = TRUE, msg = "one-element positive"
    )
    if (width >= ceiling(length(x)/2)) {
        cli::cli_abort(
            "{.arg width} must not be greater than half the length of {.arg x}."
        )
    }
    method <- match.arg(method)
    method <- method == "median" ## into logical
    validate_numeric(t0, 1, c(0, Inf), integer = TRUE, msg = "one-element positive")

    ## logical whether to handle NAs
    handle_na <- any(is.na(x))

    if (handle_na) {
        na_info <- preserve_na(x)
        x <- na_info$x_valid
    }

    n <- length(x)
    y <- x
    L <- 1.4826 ## 1 / qnorm(0.75): MAD at the 75% percentile of |Z|

    ## vectorised window bounds
    start_idx <- pmax(1, seq_len(n) - width)
    end_idx <- pmin(n, seq_len(n) + width)

    ## vectorised median & MAD
    local_median <- vapply(seq_len(n), \(i) {
        # median(x[start_idx[i]:end_idx[i]]) ## inclusive of x[i]
        median(x[setdiff(start_idx[i]:end_idx[i], i)]) ## exclusive of x[i]
    }, numeric(1))

    local_outlier <- L * vapply(seq_len(n), \(i) {
        # median(abs(x[start_idx[i]:end_idx[i]] - local_median[i]))
        median(abs(x[setdiff(start_idx[i]:end_idx[i], i)] - local_median[i]))
    }, numeric(1))

    ## logical outlier positions
    is_outlier <- abs(x - local_median) > t0 * local_outlier
    ## fill outliers with median or NA
    if (method) {
        y[is_outlier] <- local_median[is_outlier]
    } else {
        y[is_outlier] <- NA_real_
    }
    ## return y to original x length with NAs if handled
    if (handle_na) {
        result <- restore_na(y, na_info)
    } else {
        result <- y
    }

    return(result)
}




#' Preserve and Restore NA Information Within a Vector
#'
#' `preserve_na()` stores `NA` vector positions and extracts valid non-`NA`
#' values for later restoration with `restore_na()`.
#'
#' @param x A vector containing missing `NA` values.
#'
#' @return
#' `preserve_na()` returns a list `na_info` with components:
#'   - `na_info$x_valid`: A vector with `NA` values removed.
#'   - `na_info$x_length`: A numeric value of the original input vector length.
#'   - `na_info$na_idx`: A logical vector preserving `NA` positions.
#'
#' `restore_na()` returns a vector `y` of the same length as the original
#'   input vector `x` with `NA` values restored to their original positions.
#'
#' @examples
#' \dontrun{
#' x <- c(1, NA, 3, NA, 5)
#' na_info <- preserve_na(x)
#' ## process with a function that would normally fail on NA
#' y <- na_info$x_valid * 2
#' result <- restore_na(y, na_info)
#' result
#'
#' x <- c("A", NA, "B", NA, "C")
#' na_info <- preserve_na(x)
#' ## process with a function that would normally fail on NA
#' y <- tolower(na_info$x_valid)
#' result <- restore_na(y, na_info)
#' result
#' }
#'
#' @keywords internal
preserve_na <- function(x) {
    na_info <- list(
        x_valid = x[!is.na(x)],
        x_length = length(x),
        na_idx = is.na(x)
    )
    return(na_info)
}




#' Preserve and Restore NA Information Within a Vector
#'
#' `restore_na()` restores `NA` values to their original vector positions
#' after processing valid non-`NA` values returned from `preserve_na()`.
#'
#' @param y A vector of valid non-`NA` values returned from `preserve_na()`.
#' @param na_info A list returned from `preserve_na()`.
#'
#' @rdname preserve_na
#' @keywords internal
restore_na <- function(y, na_info) {
    if (all(!na_info$na_idx)) {
        return(y)
    }
    ## fill original length of NAs
    result <- rep(NA, na_info$x_length)
    if (all(na_info$na_idx)) {
        return(result)
    }
    ## replace non-NA with processed output values
    result[!na_info$na_idx] <- y
    return(result)
}
