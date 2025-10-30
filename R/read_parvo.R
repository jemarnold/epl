#' Read Parvo Exported CSV Data
#'
#' Read *.CSV* or *.xlsx* (but not *.XLS*) files exported by *Parvo Medics
#' TrueOne 2400* and return a list of three data frames with recorded data,
#' file details, and events.
#'
#' @param file_path The file path as a character string, including *.csv* or
#'   *.xlsx* file type.
#' @param add_timestamp A logical to add a "timestamp" column to the data table
#'   with date-time values, useful for synchronisation with other recordings by
#'   time of day. Precise to ± 0.5 seconds.
#' @param ... Additional arguments (not currently used).
#'
#' @details
#' This function can only parse *.CSV* files exported directly from a Parvo
#'   metabolic cart. *.XLS* exported from Parvo are obsolete and the file
#'   format cannot be read. They must be re-saved as *.xlsx* before reading
#'   with this function.
#'
#' Data from all exported channels (e.g. `c("VO2", "VCO2", "Vt")`) will be
#'   exported as-is.
#'
#' Additional data columns will be calculated if the required exported data
#'   are present. All energetic calculations are derived from
#'   *Peronnet & Massicotte, 1991. Table of nonprotein respiratory quotient: an update*.
#'
#' These include:
#'   - `FatOx` and `CarbOx` are respective substrate oxidation rates in g/min.
#'   - `O2kJ` and `O2kcal` are energy equivalents of oxygen in kJ/L and kcal/L
#'      (kilojoules and kilocalories per litre `VO2`), respectively.
#'   - `O2work`, `O2power`, and `O2energy` are aerobic metabolic work
#'      expenditure, energy expenditure, and power output in kJ/min, kcal/min,
#'      and W (kilojoules per minute, kilocalories per minute, and joules per
#'      second), respectively.
#'   - `O2pulse` is a ratio of `VO2` to heart rate (`HR`) in ml/min/bpm
#'      (millilitres of `VO2` per heart beat).
#'   - `Economy` is a ratio of the oxygen cost of work, in W/L/min (external
#'      power output in watts per litre per minute of `VO2`)
#'   - `GE` (gross efficiency) is a ratio of external work to internal metabolic
#'      work, as a percent, accounting for `VO2` and substrate oxidation (`RER`).
#'   - `METS` (metabolic equivalent of task) is a deprecated method of
#'      estimating the oxygen cost of common physically active tasks adjusted
#'      for body mass, relative to resting metabolic rate (approximately 3.5
#'      mL/kg/min), calculated as `VO2kg / 3.5`.
#'
#' @return A list with three [tibbles][tibble::tibble-package].
#' - `parvo$data` contains the data table.
#' - `parvo$details` contains the file metadata.
#' - `parvo$events` contains manual event inputs.
#'
#' @examples
#' ## retrieve example parvo file
#' file_path <- example_epl("parvo_binned")
#'
#' parvo <- read_parvo(file_path, add_timestamp = TRUE)
#' parvo
#'
#' @export
read_parvo <- function(
        file_path, ## file_path <- example_epl("parvo_binned")
        add_timestamp = FALSE,
        ...
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
                    cli::cli_abort(c(
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
        cli::cli_abort(c(
            "{.val file_path = {file_path}}.",
            "i" = "Unrecognised file type. Only {.arg .xls(x)} or \\
            {.arg .csv} currently recognised."
        ))
    }

    validate_data_frame(data_raw)

    data_clean <- data_raw |>
        dplyr::mutate(
            dplyr::across(
                dplyr::everything(), \(.x) {
                    stringr::str_squish(
                        stringr::str_replace_all(.x, "[^A-Za-z0-9.,:_\\- ]", "")
                    )
                })
        )

    ## detect header row with "TIME"
    header_row <- detect_header_row(data_clean, "TIME", 100)

    ## parvo_details =====================================
    ## strings to detect details
    search_offset <- list(
        "Name" = c(1:2),
        "Sex" = 1,
        "Age" = 1,
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
    find_parvo_details <- function(search_term, col_offset = 1) {
        col <- which(grepl(search_term, details_table))
        if (length(col) == 0) {
            return(NA)
        }
        row <- which(grepl(search_term, details_table[[col]]))
        vec <- unlist(details_table[row, col + col_offset], use.names = FALSE)
        string <- paste(vec, collapse = ", ")
        ## remove trailing "," and "File number" from .xlsx cell C
        clean_string <- sub(
            "(,\\s*File number)?\\s*,?\\s*$",
            "",
            string,
            ignore.case = TRUE
        )
        ## remove redundant duplicated string after ", "
        sub("^(.+),\\s*\\1$", "\\1", clean_string)
    }

    ## iterate over search strings
    parvo_details <- purrr::imap(search_offset, \(.x, .n) {
        find_parvo_details(.n, .x)
    }) |>
        tibble::as_tibble() |>
        dplyr::mutate(
            Date = date_string,
            ## convert Sex == "F" ~ "Female" to avoid detection as logical
            dplyr::across(
                dplyr::any_of("Sex"), \(.x) {
                    dplyr::if_else(.x == "F", "Female", "Male")
                }),
            dplyr::across( ## convert appropriate columns to numeric
                dplyr::any_of(names(search_offset)[3:length(search_offset)]),
                \(.x) as.numeric(.x)
            ),
            dplyr::across(
                dplyr::where(is.numeric), \(.x) round(.x, 8)
            ),
        ) |>
        dplyr::rename(`CO2 Gain` = "CO2.*?gain") |>
        dplyr::relocate(Date)

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
        dplyr::slice_tail(n = -4) |>
        stats::setNames(make.unique(parvo_names)) |>
        dplyr::mutate(
            ## convert columns from character & suppress confirmation
            dplyr::across(dplyr::everything(), \(.x) {
                num_x <- suppressWarnings(as.numeric(.x))
                if (sum(!is.na(num_x)) > sum(is.na(num_x))) {
                    num_x
                } else {
                    .x
                }
            }),
            ## convert `TIME` in "mm:ss" or "min" to seconds
            TIME = if (is.character(TIME)) {
                as.numeric(lubridate::ms(TIME, quiet = TRUE))
            } else {
                as.numeric(TIME * 60)
            },
        ) |>
        ## drops rows after/including the first row with all NA
        drop_rows_after_first_na() |>
        (\(.df) {
            if (all(c("VO2", "VCO2") %in% parvo_names)) {
                dplyr::mutate(
                    .df,
                    ## confirm VO2 & VCO2 in L/min
                    VO2_test = max(VO2, na.rm = TRUE) > 1000,
                    VCO2_test = max(VCO2, na.rm = TRUE) > 1000,
                    VO2_L = dplyr::if_else(VO2_test, VO2 / 1000, VO2),
                    VCO2_L = dplyr::if_else(VCO2_test, VCO2 / 1000, VCO2),
                    ## Peronnet & Massicotte 1991 substrate oxidation in g/min
                    FatOx = 1.695 * VO2_L - 1.701 * VCO2_L, ## g/min
                    CarbOx = 4.585 * VCO2_L - 3.226 * VO2_L, ## g/min
                    ## NOT USED Jeukendrup & Wallis 2005 50-75% Intensity
                    # CarbOx = 4.210 * VCO2 - 2.962 * VO2, ## g/min
                    ## calculate O2eq in kJ/L & kcal/L from Péronnet & Massicotte 1991
                    O2kJ = 16.8835 + 4.8353 * pmin(pmax(VCO2_L / VO2_L, 0.7036), 0.9961), ## kJ/L
                    O2kcal = 4.0372 + 1.1563 * pmin(pmax(VCO2_L / VO2_L, 0.7036), 0.9961), ## kcal/L
                    ## VO2 in L/min, O2kJ in kJ/L = aerobic work expenditure in kJ/min
                    O2work = VO2_L * O2kJ, ## kJ/min
                    ## VO2 in L/min, O2kcal in kcal/L = aerobic energy expenditure in kcal/min
                    O2energy = VO2_L * O2kcal, ## kcal/min
                    ## aerobic power input in J/sec = Watts
                    O2power = VO2_L * O2kJ * 1000 / 60, ## W
                    ## O2 pulse ml/bpm
                    O2pulse = if ("HR" %in% parvo_names) {VO2_L * 1000 / HR},
                    ## EC W/L/min
                    Economy = if ("WorkR" %in% parvo_names) {WorkR / VO2_L}, ## W/L/min
                    ## GE % external work / metabolic work
                    GE = if ("WorkR" %in% parvo_names) {
                        (WorkR * 60) / (VO2_L * O2kJ * 1000)
                    },
                ) |>
                    dplyr::select(-c(VO2_test, VCO2_test, VO2_L, VCO2_L))
            } else {
                .df
            }
        })() |>
        dplyr::mutate(
            METS = if ("VO2kg" %in% parvo_names) {
                VO2kg / 3.5
            },
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
        ## drops rows where TIME is NA
        tidyr::drop_na(TIME)

    ## events_data ============================================
    rows <- which(grepl("Events", data_clean[[1]]))
    rows <- ifelse(length(rows) > 0, rows, nrow(data_clean))
    events_data <- data_clean |>
        dplyr::select(TIME = 1, Events = 2) |>
        dplyr::slice(rows:nrow(data_clean)) |>
        dplyr::filter(dplyr::if_any(2, \(.x) .x != "")) |>
        dplyr::mutate(TIME = as.numeric(TIME) * 60)

    ## add timestamp for sync =====================
    if (add_timestamp) {
        start_dttm <- lubridate::ymd_hms(
            parvo_details[["Date"]],
            tz = "America/Vancouver"
        )
        parvo_data$timestamp <- start_dttm + parvo_data$TIME
        parvo_data <- parvo_data |>
            dplyr::relocate(dplyr::any_of("timestamp"), .after = TIME)
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
