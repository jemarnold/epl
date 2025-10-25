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
#' @return A list with three [tibbles][tibble::tibble-package].
#' - `parvo$data` contains the data table.
#' - `parvo$details` contains the file metadata.
#' - `parvo$events` contains manual event inputs.
#'
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_squish
#' @importFrom purrr imap
#'
#' @export
read_parvo <- function(
        file_path, ## file_path <- example_epl("parvo_binned")
        time_column = "TIME",
        add_timestamp = FALSE
) {
    ## read file ===========================================
    ## validation: check file exists
    validate_file_path(file_path)

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
        data_raw <- read_csv_robust(file_path)
    } else {
        ## validation: check file types
        cli_abort(c(
            "{.val file_path = {file_path}}.",
            "i" = "Unrecognised file type. Only {.arg .xls(x)} or \\
            {.arg .csv} currently recognised."
        ))
    }

    validate_data_frame(data_raw)

    data_clean <- data_raw |>
        mutate(
            across(
                everything(), \(.x) {
                    str_squish(str_replace_all(.x, "[^A-Za-z0-9.,: ]", ""))
                })
        )

    ## detect header row with time_column
    header_row <- detect_header_row(data_clean, time_column, 100)

    ## parvo_details =====================================
    ## strings to detect details
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
        "CO2.*?gain" = 1,
        "Base O2" = 1,
        "Base CO2" = 1,
        "Measured O2" = 1,
        "Measured CO2" = 1
    )

    details_table <- data_clean[1:(header_row - 2), ]

    ## aggregate date string
    date_string <- paste(
        paste(details_table[3, c(2, 4, 6)], collapse = "-"),
        paste(details_table[3, c(7, 9, 10)], collapse = ":"),
        collapse = " "
    )

    ## helper function to retrieve value where search string detected
    find_value <- function(search_term, col_offset = 1) {
        col <- which(grepl(search_term, details_table))
        if (length(col) == 0) {
            return(NA)
        }
        row <- which(grepl(search_term, details_table[[col]]))
        vec <- unlist(details_table[row, col + col_offset], use.names = FALSE)
        sub(",\\s*$", "", paste(vec, collapse = ", "))
    }

    ## iterate over search strings
    parvo_details <- purrr::imap(search_offset, \(.x, .n) find_value(.n, .x)) |>
        as_tibble() |>
        mutate(
            Date = date_string,
            across( ## convert appropriate columns to numeric
                any_of(names(search_offset)[c(2, 4:11)]),
                \(.x) as.numeric(.x)
            ), ## convert Sex == "F" ~ "Female" to avoid detection as logical
            across(any_of("Sex"), \(.x) if_else(.x == "F", "Female", "Male")),
        ) |>
        rename(`CO2 Gain` = "CO2.*?gain") |>
        relocate(Date)

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
        ## convert columns from character & suppress confirmation
        mutate(
            across(everything(), \(.x) {
                num_x <- suppressWarnings(as.numeric(.x))
                if (sum(!is.na(num_x)) > sum(is.na(num_x))) {
                    num_x
                } else {
                    .x
                }
            }),
            ## convert `TIME` in "mm:ss" or "min" to seconds
            across(all_of(time_column), \(.x) {
                if (is.character(.x)) {
                    as.numeric(ms(.x))
                } else {
                    .x * 60
                }
            }),
        ) |>
        ## drops rows after/including the first row with all NA
        drop_rows_after_first_na() |>
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
                    across(-any_of(c(time_column, "HR", "WorkR")), \(.x) {
                        if_else(VO2_L < 0.100, NA_real_, .x)
                    }),

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
                \(.x) if_else(!is.finite(.x), NA_real_, .x)
            ),
            ## round to avoid floating point
            across(where(is.numeric), \(.x) round(.x, 8)),
        ) |>
        ## drops rows where time_column is NA
        drop_na(all_of(time_column))

    ## events_data ============================================
    rows <- which(grepl("Events", data_clean[[1]]))
    rows <- ifelse(length(rows) > 0, rows, nrow(data_clean))
    events_data <- data_clean |>
        select(!!time_column := 1, "Events" = 2) |>
        slice(rows:nrow(data_clean)) |>
        filter(if_any(2, \(.x) .x != "")) |>
        mutate(across(all_of(time_column), \(.x) as.numeric(.x) * 60))

    ## add timestamp for sync =====================
    if (add_timestamp) {
        start_dttm <- lubridate::ymd_hms(
            parvo_details[["Date"]],
            tz = "America/Vancouver"
        )
        parvo_data$timestamp <- start_dttm + parvo_data[[time_column]]
        parvo_data <- parvo_data |>
            relocate(any_of("timestamp"), .after = all_of(time_column))
    }

    ## return =====================================
    return(
        list(
            "data" = parvo_data,
            "details" = parvo_details,
            "events" = events_data
        )
    )
}
