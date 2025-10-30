#' Get path to EPL example files
#'
#' @param file Name of file as character string. If kept as `NULL`, returns a
#'   vector of all available files.
#'
#' @return
#' File paths for selected example files stored in this package.
#'
#' @examples
#' ## lists all files
#' example_epl()
#'
#' ## partial matching will error if matches multiple
#' ## example_epl("tymewear")
#' #> Error in `example_epl()`:
#' #> ! Multiple files match "tymewear":
#' #> ℹ Matching files: "tymewear_live.csv" and "tymewear_post.csv"
#'
#' example_epl("tymewear_live")
#'
#' @export
example_epl <- function(file = NULL) {
    dir_files <- list.files(
        system.file("extdata", package = "epl"),
        pattern = "^[^~]" ## exclude open files
    )

    if (is.null(file)) {
        return(dir_files)
    }

    matches <- grep(file, dir_files, fixed = TRUE, value = TRUE)
    if (length(matches) > 1) {
        cli::cli_abort(c(
            "Multiple files match {.val {file}}:",
            "i" = "Matching files: {.val {matches}}"
        ))
    }

    file <- match.arg(file, choices = dir_files)
    system.file("extdata", file, package = "epl", mustWork = TRUE)
}
