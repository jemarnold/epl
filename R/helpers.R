#' @keywords internal
validate_file_path <- function(file_path) {
    ## validation: check file exists
    if (!file.exists(file_path)) {
        cli::cli_abort(c(
            "{.arg file_path} = {.val {file_path}}",
            "x" = "File not found. Check that file exists."
        ))
    }
}




#' @keywords internal
validate_data_frame <- function(data) {
    if (!is.data.frame(data)) {
        cli::cli_abort("{.arg data} should be a data frame.")
    }
}



#' @keywords internal
validate_numeric <- function(
        arg,
        elements = Inf,
        range = NULL,
        inclusive = c("left", "right"),
        integer = FALSE,
        msg = ""
) {
    name <- substitute(arg)
    valid_arg <- !is.na(arg) & !is.nan(arg)

    if (elements == 1) {
        arg_length <- "value"
    } else {
        arg_length <- "vector"
    }

    if (is.finite(elements)) {
        elements_true <- sum(valid_arg) == elements
    } else if (is.infinite(elements)) {
        elements_true <- sum(valid_arg) > 0
    }

    range_true <- TRUE ## default condition
    if (!is.null(range)) {
        range_true <- all(
            between(arg[valid_arg], range[1L], range[2L], inclusive)
        )
    }
    if (!integer) {
        integer_true <- TRUE
        arg_class <- "numeric"
    } else {
        integer_true <- rlang::is_integerish(arg[valid_arg])
        arg_class <- "integer"
        arg_length <- ""
    }

    if (
        !is.null(arg) &&
        (!is.numeric(arg) || !integer_true || !elements_true || !range_true)
    ) {
        cli::cli_abort( ## "one-element positive"; "two-element"
            "{.arg {name}} must be a valid {msg} {.cls {arg_class}} \\
            {arg_length}."
        )
    }
}





#' @keywords internal
read_csv_robust <- function(file_path, n = Inf) {
    ## read csv as raw lines. Avoids issues with multiple empty rows & columns
    readr::read_lines(file_path, n_max = n) |>
        stringr::str_split_fixed(",", n = Inf) |> ## don't print this it takes ages!
        tibble::as_tibble(.name_repair = "unique_quiet")
}

## base R version
# read_csv_robust_base <- function(file_path) {
#     lines <- readLines(file_path, warn = FALSE)
#     splits <- strsplit(lines, ",", fixed = TRUE)
#     max_cols <- max(lengths(splits))
#     t(vapply(splits, `[`, character(max_cols), seq_len(max_cols))) |>
#         as_tibble(.name_repair = "unique_quiet")
# }




#' @keywords internal
detect_header_row <- function(data, label, max_row = 100, max_col = 50) {
    cols <- seq_len(min(max_row, nrow(data)))
    rows <- seq_len(min(max_col, ncol(data)))
    ## detect header row with "label"
    header_match <- apply(data[cols, rows, drop = FALSE], 1, \(.row) {
        any(.row == label, na.rm = TRUE)
    })
    header_row <- which(header_match)[1]

    ## validation: "label" must be detected to extract the proper data frame
    if (is.na(header_row) || length(header_row) != 1) {
        cli::cli_abort(c(
            "Error detecting {.val {label}} in data.",
            "i" = "Strings are case sensitive and should match exactly. \\
            Check file structure."
        ))
    }

    return(header_row)
}




#' @keywords internal
drop_rows_after_first_na <- function(data) {
    ## drops rows after/including the first row with all NA
    first_allna <- which(rowSums(!is.na(data)) == 0)[1]

    if (!is.na(first_allna)) {
        data[seq_len(first_allna - 1), ]
    } else {
        data
    }
}





#' Detect if numeric values fall between a range
#'
#' Vectorised inclusive within `x >= left` & `x <= right`, or exclusive
#' between `x > left` & `x < right`. Each side can be specified separately.
#'
#' @param x A numeric vector.
#' @param left,right Numeric boundary values. Both `left` and `right` are
#'   recycled to the size of `x`.
#' @param inclusive A character vector to specify which of `left` and/or
#'   `right` boundary values should be included in the range, or both
#'   (the default), or excluded if `FALSE`.
#'
#' @details
#' `inclusive = FALSE` can be used to test for positive non-zero values:
#'   `between(x, 0, Inf, inclusive = FALSE)`.
#'
#' @return A logical vector the same length as `x`.
#'
#' @seealso [dplyr::between()]
#'
#' @keywords internal
between <- function(x, left, right, inclusive = c("left", "right")) {
    validate_numeric(x)
    validate_numeric(left, 1L, msg = "one-element")
    validate_numeric(right, 1L, msg = "one-element")
    inclusive <- match.arg(
        as.character(inclusive),
        choices = c("left", "right", "FALSE"),
        several.ok = TRUE
    )

    if ("FALSE" %in% inclusive) {
        return(x > left & x < right)
    }

    left_compare <- if ("left" %in% inclusive) {
        x >= left
    } else {
        x > left
    }
    right_compare <- if ("right" %in% inclusive) {
        x <= right
    } else {
        x < right
    }

    return(left_compare & right_compare)
}
