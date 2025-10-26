library(tidyverse)
details_string <- c(
    "Date" = "Date",
    "Name" = "Name",
    "Age" = "Age",
    "Sex" = "Sex",
    "Height" = "Height",
    "Weight" = "Weight",
    "Temperature" = "Insp. temp",
    "Pressure" = "Baro. pressure",
    "Humidity" = "Insp. humidity",
    "STPD_to_BTPS" = "STPD to BTPS",
    "O2_gain" = "O2 Gain",
    "CO2_gain" = "CO2.*?gain"
)

details_clean <- data_clean[1:(header_row-2), ]

old <- function(data) {
    ## create dataframe with details
    parvo_details <- list(
        string = details_string,
        ## columns for where to search for string
        search_col = c(NA, 1, 1, 4, 1, 6, 2, 5, 8, 1, 3, 5),
        ## columns for where to retrieve string
        return_col = c(NA, 2, 2, 5, 4, 9, 3, 6, 9, 2, 4, 6)
    ) |>
        purrr::pmap(\(string, search_col, return_col) {
            ## extra formatting conditions for Date & Name
            if (string == "Date") {
                paste(
                    gsub(" ", "", paste(details_clean[3, c(2, 4, 6)], collapse = "-")),
                    gsub(" ", "", paste(details_clean[3, c(7, 9, 10)], collapse = ":"))
                )
            } else if (string == "Name") {
                rows <- grepl(string, details_clean[[search_col]])
                if (grepl(",", details_clean[rows, ][[return_col]])) {
                    ## remove hanging end ","
                    sub(",\\s*$", "", details_clean[rows, ][[return_col]])
                } else {
                    ## condition where "," in name forces column separation
                    name <- paste(
                        details_clean[rows, ][[return_col]],
                        details_clean[rows, ][[return_col + 1]],
                        sep = ", "
                    )
                    sub(",\\s*$", "", name)
                }
            } else {
                rows <- grepl(string, details_clean[[search_col]])
                details_clean[rows, ][[return_col]]
            }
        }) |>
        ## remove length 0 list items (e.g. missing CO2_gain)
        (\(.l) .l[lapply(.l, length) > 0])() |>
        as_tibble() |>
        mutate(
            ## convert Sex to avoid detection "F" as logical
            across(
                matches("sex", ignore.case = TRUE),
                \(.x) case_when(.x == "F" ~ "Female",
                                .x == "M" ~ "Male",
                                TRUE ~ .x)
            )
        ) |>
        ## convert columns from character & suppress confirmation
        type_convert() |>
        suppressMessages() |>
        mutate(
            across(
                where(is.numeric) & !any_of(tail(names(details_string), 3)),
                \(.x) signif(.x, 3)
            )
        )

    return(parvo_details)
}

new <- function(data) {
    ## columns for where to search for string
    search_col <- c(NA, 1, 1, 4, 1, 6, 2, 5, 8, 1, 3, 5)
    ## columns for where to retrieve string
    return_col <- c(NA, 2, 2, 5, 4, 9, 3, 6, 9, 2, 4, 6)

    ## create dataframe with details
    parvo_details <- sapply(seq_along(details_string), \(.i) {
        string <- details_string[.i]
        if (string != "Date") {
            rows <- which(grepl(string, details_clean[[search_col[.i]]]))
        }

        if (string == "Date") {
            ## extra formatting conditions for Date & Name
            paste(
                gsub(" ", "", paste(details_clean[3, c(2, 4, 6)], collapse = "-")),
                gsub(" ", "", paste(details_clean[3, c(7, 9, 10)], collapse = ":"))
            )
        } else if (string == "Name") {
            name_val <- details_clean[rows, return_col[.i]]

            if (grepl(",", name_val)) {
                ## remove hanging end ","
                sub(",\\s*$", "", name_val)
            } else {
                ## condition where "," in name forces column separation
                sub(",\\s*$", "", paste(
                    name_val,
                    details_clean[rows, return_col[.i] + 1],
                    sep = ", "
                ))
            }
        } else if (length(rows) > 0) {
            details_clean[rows, return_col[.i]]
        } else {
            character(0)
        }
    }, simplify = FALSE, USE.NAMES = TRUE)

    ## remove empty elements and convert to data frame
    parvo_details <- as.data.frame(
        parvo_details[lengths(parvo_details) > 0],
        stringsAsFactors = FALSE
    ) |>
        setNames(names(details_string)) |>
        tibble::as_tibble()

    ## convert Sex to avoid detection "F" as logical
    if ("Sex" %in% names(parvo_details)) {
        parvo_details$Sex <- ifelse(
            parvo_details$Sex == "F",
            "Female",
            "Male"
        )
    }

    ## type conversion and rounding
    num_cols <- sapply(parvo_details, \(.x) {
        !is.na(suppressWarnings(as.numeric(.x)))
    })
    num_cols["Date"] <- FALSE
    parvo_details[num_cols] <- lapply(parvo_details[num_cols], \(.x) {
        as.numeric(.x)
    })

    exact_cols <- c("STPD_to_BTPS", "O2_gain", "CO2_gain")
    round_cols <- names(parvo_details)[
        num_cols & !names(parvo_details) %in% exact_cols
    ]
    parvo_details[round_cols] <- lapply(parvo_details[round_cols], \(.x) {
        signif(.x, digits = 3)
    })

    return(parvo_details)
}

mid <- function(data) {
    search_offset <- list(
        "Name" = c(1:2),
        "Age" = 1,
        "Sex" = 1,
        "Height" = 3,
        "Weight" = 3,
        "Insp. temp" = 1,
        "Baro. pressure" = 1,
        "Insp. humidity" = 1,
        "STPD to BTPS" = 1,
        "O2 Gain" = 1,
        "CO2.*?gain" = 1.
        "Base O2" = 1,
        "Base CO2" = 1,
        "Measured O2" = 1,
        "Measured CO2" = 1
    )

    date <- paste(
        paste(details_clean[3, c(2, 4, 6)], collapse = "-"),
        paste(details_clean[3, c(7, 9, 10)], collapse = ":"),
        collapse = " "
    )

    find_value <- function(search_term, col_offset = 1) {
        col <- which(grepl(search_term, details_clean))
        if (length(col) == 0) {
            return(NA)
        }
        row <- which(grepl(search_term, details_clean[[col]]))
        vec <- unlist(details_clean[row, col + col_offset], use.names = FALSE)
        sub(",\\s*$", "", paste(vec, collapse = ", "))
    }

    parvo_details <- imap(search_offset, \(.x, .n) find_value(.n, .x)) |>
        as_tibble() |>
        mutate(
            Date = date,
            across(
                any_of(names(search_offset)[c(2, 4:11)]),
                \(.x) as.numeric(.x)
            ),
            across(any_of("Sex"), \(.x) if_else(.x == "M", "Male", "Female")),
        ) |>
        rename(`CO2 Gain` = "CO2.*?gain") |>
        relocate(Date)

    return(parvo_details)
}


bench::mark(
    old = old(details_clean),
    new = new(details_clean),
    mid = mid(details_clean),
    check = FALSE,
    iterations = 50
)
