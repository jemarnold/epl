#' Read Parvo Exported CSV Data
#'
#' Read *.CSV* or *.xlsx* (but not *.XLS*) files exported by Parvo Medics
#' TrueOne 2400 and return recorded data table and file metadata.
#'
#' @param file_path The file path as a character string, including *.csv* or
#' *.xlsx* file type.
#' @param time_column A character string for the time column in the data table.
#' *Defaults* to `time_column = "TIME"`. Used to detect the location of the data
#' table.
#' @param add_timestamp A logical to add a "timestamp" column to the data table
#' with date-time values, useful for synchronisation with other recordings by
#' time of day. Precise to ± 0.5 seconds.
#'
#' @details
#' This function can only parse *.CSV* files exported directly from a Parvo
#' metabolic cart. Obsolete *.XLS* exported file format cannot be read and
#' must be re-saved as *.xlsx* before reading with this function.
#'
#'
#' @return A list with two [tibbles][tibble::tibble-package].
#' - `parvo$data` contains the data table.
#' - `parvo$details` contains the file metadata.
#'
#' @export
read_parvo <- function(
        file_path,
        time_column = "TIME",
        add_timestamp = FALSE
) {
    ## read file ===========================================
    ## validation: check file exists
    if (!file.exists(file_path)) {
        cli_abort(
            "{.val file_path = {file_path}} not found. \\
            Check that file exists."
        )
    }

    ## read parvo file
    if (grepl("\\.xls(x)?$", file_path, ignore.case = TRUE)) {
        ## read parvo .xlsx
        data_raw <- tryCatch(
            readxl::read_excel(
                path = file_path,
                col_names = FALSE,
                .name_repair = "unique_quiet"
            ),
            error = \(e) {
                ## error message for default .XLS export (obsolete, unreadable format)
                if (grepl("libxls error", e$message)) {
                    cli_abort(c(
                        "{e}",
                        "i" = "Parvo `.XLS` export format cannot be opened. \\
                        Export as `.CSV` or re-save the file as `.xlsx`."
                    ))
                } else {
                    stop(e)
                }
            }
        )
    } else if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
        ## read raw lines from csv. Avoids issues with multiple empty rows
        all_lines <- readLines(file_path, warn = FALSE)
        row_vectors <- strsplit(all_lines, ",")
        ## pad row length with NA for explicit rectangular data table
        max_cols <- max(lengths(row_vectors))
        rows_to_pad <- lengths(row_vectors) < max_cols
        row_vectors[rows_to_pad] <- lapply(row_vectors[rows_to_pad], \(.x) {
            length(.x) <- max_cols
            .x
        })

        data_raw <- as_tibble(
            do.call(rbind, row_vectors),
            .name_repair = "unique_quiet"
        )
    } else {
        ## validation: check file types
        cli_abort(c(
            "{.val file_path = {file_path}}.",
            "i" = "Unrecognised file type. Only {.arg .xls(x)} or \\
            {.arg .csv} currently recognised."
        ))
    }

    data_clean <- data_raw |>
        mutate(
            across(
                everything(), \(.x) {
                    str_squish(str_replace_all(.x, "[^A-Za-z0-9.,: ]", ""))
                })
        )

    ## detect header row where time_column exists
    header_row <- which(apply(data_clean[1:30, ], 1, \(.row) {
        time_column %in% .row
    }))

    ## validation: time_column must be detected to extract the proper data frame
    if (rlang::is_empty(header_row) || length(header_row) > 2) {
        cli_abort(
            "Error detecting {.val time_column = {time_column}}. \\
            {.val time_column} is case sensitive and should match exactly."
        )
    }

    ## parvo_details =====================================
    ## strings to detect details
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
                    gsub(" ", "", paste(data_clean[3, c(2, 4, 6)], collapse = "-")),
                    gsub(" ", "", paste(data_clean[3, c(7, 9, 10)], collapse = ":"))
                )
            } else if (string == "Name") {
                rows <- grepl(string, data_clean[[search_col]])
                if (grepl(",", data_clean[rows, ][[return_col]])) {
                    ## remove hanging end ","
                    sub(",\\s*$", "", data_clean[rows, ][[return_col]])
                } else {
                    ## condition where "," in name forces column separation
                    name <- paste(
                        data_clean[rows, ][[return_col]],
                        data_clean[rows, ][[return_col + 1]],
                        sep = ", "
                    )
                    sub(",\\s*$", "", name)
                }
            } else {
                rows <- grepl(string, data_clean[[search_col]])
                data_clean[rows, ][[return_col]]
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

    ## parvo_data ============================================
    data_table <- data_clean[header_row:nrow(data_clean), ]

    parvo_names <- (\(df) {
        # Extract first 3 rows as character vectors
        row1 <- as.character(df[1, ])
        row2 <- as.character(df[2, ])
        row3 <- as.character(df[3, ])

        ## combine row1 with row2 (unless row2 is NA or in exclusion list)
        exclude <- c(NA, "STPD", "BTPS", "ATPS")
        names <- ifelse(
            row2 %in% exclude | is.na(row2),
            row1,
            paste0(row1, row2)
        )

        ## handle duplicates
        dup_idx <- duplicated(names)

        if (any(dup_idx)) {
            ## for each duplicate, find if row2 differs from first occurrence
            first_occurrence <- match(names, names)
            row2_differs <- row2 != row2[first_occurrence]

            ## choose row2 if it differs, otherwise row3
            append_vals <- ifelse(row2_differs, row2, row3)

            ## apply only to duplicates
            names[dup_idx] <- paste(
                names[dup_idx], append_vals[dup_idx], sep = "_"
            )
        }

        return(names)
    })(data_table)

    parvo_data <- data_table |>
        slice_tail(n = -4) |>
        setNames(make.unique(parvo_names)) |>
        (\(.df) {
            ## drops rows after/including the first row with all NA
            ## c(..., 0) ensures the last row will be included if no rows are all NA
            first_allna_row <- which(
                diff(c(rowSums(is.na(.df)) != ncol(.df), 0)) != 0
            )[1]

            slice_head(.df, n = first_allna_row)
        })() |>
        ## convert columns from character & suppress confirmation
        mutate(
            ## convert columns to numeric without warnings, ignore char cols
            across(everything(), \(.x) {
                suppressWarnings(
                    if (sum(!is.na(as.numeric(.x))) > sum(is.na(as.numeric(.x)))) {
                        as.numeric(.x)} else {.x})
            }),
            ## convert `TIME` in "mm:ss" or "min" to seconds
            across(matches(time_column), \(.x) {
                suppressWarnings(
                    if (is.character(.x)) {as.numeric(ms(.x))} else {.x * 60}
                )
            }),
        ) |>
        (\(.df) {
            if (all(c("VO2", "VCO2") %in% parvo_names)) {
                mutate(
                    .df,
                    ## confirm VO2 & VCO2 in L/min
                    VO2_test = max(VO2, na.rm = TRUE) > 1000,
                    VCO2_test = max(VCO2, na.rm = TRUE) > 1000,
                    VO2_L = if_else(VO2_test, VO2 / 1000, VO2),
                    VCO2_L = if_else(VCO2_test, VCO2 / 1000, VCO2),

                    ## overwrite non-physiological values to NA when VO2 < 100
                    ## TODO may need to adjust non-physiological criteria
                    ## TODO what other columns to exclude?
                    across(
                        -any_of(c(time_column, "HR", "WorkR")),
                        \(.x) if_else(VO2_L < 0.100, NA_real_, .x)
                    ),

                    ## Peronnet & Massicotte 1991 substrate oxidation in g/min
                    FatOx = 1.695 * VO2_L - 1.701 * VCO2_L, ## g/min
                    CarbOx = 4.585 * VCO2_L - 3.226 * VO2_L, ## g/min
                    ## NOT USED Jeukendrup & Wallis 2005 50-75% Intensity
                    # CarbOx = 4.210 * VCO2 - 2.962 * VO2, ## g/min
                    ## calculate O2eq in kJ/L & kcal/L from Péronnet & Massicotte 1991
                    O2kJ = 16.8835 + 4.8353 * pmin(pmax(VCO2_L / VO2_L, 0.7036), 0.9961), ## kJ/L
                    O2kcal = 4.0372 + 1.1563 * pmin(pmax(VCO2_L / VO2_L, 0.7036), 0.9961), ## kcal/L
                    ## VO2 in L/min, O2kJ in kJ/L = aerobic work expenditure in kJ/min
                    WEaer = VO2_L * O2kJ, ## kJ/min
                    ## aerobic power input in J/sec = Watts
                    Paer = VO2_L * O2kJ * 1000 / 60, ## W
                    ## VO2 in L/min, O2kcal in kcal/L = aerobic energy expenditure in kcal/min
                    EEaer = VO2_L * O2kcal, ## kcal/min
                    ## O2 pulse ml/bpm
                    O2pulse = if ("HR" %in% parvo_names) {VO2_L * 1000 / HR},
                    ## EC W/L/min
                    Economy = if ("WorkR" %in% parvo_names) {WorkR / VO2_L}, ## W/L/min
                    ## GE % external work / metabolic work
                    GE = if ("WorkR" %in% parvo_names) {
                        (WorkR * 60) / (VO2_L * O2kJ * 1000)
                    },
                ) |>
                    select(-c(VO2_test, VCO2_test, VO2_L, VCO2_L))
            } else {
                .df
            }
        })() |>
        mutate(
            METS = if ("VO2kg" %in% parvo_names) {
                VO2kg / 3.5
            },
            ## convert blank values to NA
            across(
                where(is.numeric),
                \(.x) ifelse(is.infinite(.x) | is.nan(.x), NA_real_, .x)
            ),
        ) |>
        ## drops rows where time_column is NA
        drop_na(all_of(time_column)) |>
        ## drops rows where all NA
        filter(!is.na(.data[[time_column]]))

    ## outliers_data ===========================================
    ## filter local outliers

    parvo_data_filtered <- parvo_data |>
        mutate(
            across(
                where(\(.x) {is.numeric(.x) & !all(is.na(.x) | .x == 0)}) &
                    !all_of(time_column),
                \(.x) replace_outliers(.x, width = width)
            )
        )

    ## compare dataframes with/without local outliers
    ## isTRUE(time_column) indicates rows with outliers
    outliers_data_pre <- as_tibble(parvo_data != parvo_data_filtered) |>
        mutate(
            !!time_column := as.logical(
                pmax(!!!syms(names(parvo_data)), na.rm = TRUE)
            )
        )

    ## dataframe with only rows with outliers from any columns
    ## returns a dataframe with the original outlier values
    outliers_data <- purrr::map2(
        parvo_data,
        outliers_data_pre,
        \(.df1, .df2) na_if(.df1 * .df2, 0)
    ) |>
        as_tibble() |>
        ## drops rows where all NA
        filter(if_any(everything(), \(.x) !is.na(.x)))

    if (.replace_outliers) {
        parvo_data <- parvo_data_filtered
    }

    ## peak_data ===========================================
    ## create a dataframe with values during VO2peak time window
    if (any(c("VO2", "VO2kg") %in% parvo_names)) {
        VO2peak_column <- ifelse("VO2" %in% parvo_names, "VO2", "VO2kg")

        peak_data <- find_peak_data(
            y = VO2peak_column,
            x = time_column,
            data = parvo_data,
            ## `peak_span` default to 30-sec VO2peak
            span = peak_span
        )
    } else {
        peak_data = tibble(!!time_column := NA_real_)
    }

    ## events_data ============================================
    rows <- which(grepl("Events", data_clean[[1]]))
    rows <- ifelse(length(rows) > 0, rows, nrow(data_clean))
    events_data <- data_clean |>
        select(!!time_column := 1, "Events" = 2) |>
        slice(rows:nrow(data_clean)) |>
        filter(if_any(2, \(.x) .x != "")) |>
        mutate(across(matches(time_column), \(.x) as.numeric(.x) * 60))

    ## add timestamp for sync =====================
    if (add_timestamp) {
        start_dttm <- lubridate::ymd_hms(
            parvo_details[["Date"]],
            tz = "America/Vancouver"
        )

        parvo_data <- parvo_data |>
            mutate(
                ## create `timestamp` column for synchronisation\
                across(
                    matches(time_column),
                    \(.x) start_dttm + .x,
                    .names = "timestamp"
                ),
            ) |>
            relocate(any_of("timestamp"), .after = all_of(time_column))
    }

    ## return =====================================
    return(
        list(
            "data" = parvo_data,
            "details" = parvo_details,
            "peaks" = peak_data,
            "events" = events_data,
            "outliers" = outliers_data
        )
    )
}
