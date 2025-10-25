#' @keywords internal
validate_file_path <- function(file_path) {
    ## validation: check file exists
    if (!file.exists(file_path)) {
        cli_abort(c(
            "{.arg file_path} = {.val {file_path}}",
            "x" = "File not found. Check that file exists."
        ))
    }
}




#' @keywords internal
validate_data_frame <- function(data) {
    if (!is.data.frame(data)) {
        cli_abort("{.arg data} should be a data frame.")
    }
}



#' @keywords internal
validate_numeric <- function(
        arg,
        elements = Inf,
        range = NULL,
        inclusive = TRUE,
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
        integer_true <- is_integerish(arg[valid_arg])
        arg_class <- "integer"
        arg_length <- ""
    }

    if (
        !is.null(arg) &&
        (!is.numeric(arg) || !integer_true || !elements_true || !range_true)
    ) {
        cli_abort( ## "one-element positive"; "two-element"
            "{.arg {name}} must be a valid {msg} {.cls {arg_class}} \\
            {arg_length}."
        )
    }
}




#' @importFrom stringr str_split_fixed
#' @importFrom readr read_lines
#' @keywords internal
read_csv_robust <- function(file_path) {
    ## read csv as raw lines. Avoids issues with multiple empty rows & columns
    read_lines(file_path) |>
        str_split_fixed(",", n = Inf) |> ## don't print this it takes ages!
        as_tibble(.name_repair = "unique_quiet")
}

# read_csv_robust_base <- function(file_path) {
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





drop_rows_after_first_na <- function(data) {
    ## drops rows after/including the first row with all NA
    first_allna <- which(rowSums(!is.na(data)) == 0)[1]

    if (!is.na(first_allna)) {
        data[seq_len(first_allna - 1), ]
    } else {
        data
    }
}





#' Detect if values fall within a range
#'
#' Vectorised inclusive or exclusive `x >= left` & `x <= right`
#'
#' @param x A numeric vector.
#' @param left,right Numeric boundary values. Both `left` and `right` are
#'  recycled to the size of `x`.
#' @param inclusive A logical to specify that `left` and `right` boundary
#'  values should be accepted in the range (the *default*) or excluded if
#'  `FALSE`.
#'
#' @details
#' `inclusive = FALSE` could be used to test for positive non-zero values:
#' `between(x, 0, Inf, inclusive = FALSE)`.
#'
#' @return A logical vector the same length as `x`.
#'
#' @seealso [dplyr::between()]
#'
#' @keywords internal
between <- function(x, left, right, inclusive = TRUE) {
    if (inclusive) {
        x >= left & x <= right
    } else {
        x > left & x < right
    }
}





#' Get path to `{epl}` example files
#'
#' @param file Name of file as character string. If kept as `NULL`, returns a
#' vector of all available files.
#'
#' @return
#' File paths for selected example files stored in this package.
#'
#' @examples
#' example_epl()
#' example_epl("tymewear_live")
#'
#' @export
example_epl <- function(file = NULL) {
    dir_files <- dir(system.file("extdata", package = "epl"))
    if (is.null(file)) {
        dir_files
    } else {
        file <- match.arg(file, choices = dir_files)
        system.file("extdata", file, package = "epl", mustWork = TRUE)
    }
}
