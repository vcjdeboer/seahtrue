# Parquet reader for the plate dataset -------------------------------------

#' Read a plate dataset (Parquet) and validate it
#'
#' @description
#' Reads one plate dataset: the Parquet file the parser writes for one
#' plate, one row per well per tick, at a plate dataset schema version this
#' seahtrue knows. The file is read as data only. Nothing in it is evaluated,
#' parsed as code or deserialised, and no R or Arrow metadata stored in it is
#' applied. The reader refuses a plate dataset that:
#' * has an unknown or missing `plate_dataset_schema_version`;
#' * has a missing, extra or differently typed column;
#' * breaks the schema's rules (one value per plate, per well, per
#'   measurement or per tick; 96 wells named A01 to H12; every well at every
#'   tick).
#'
#' It converts each tick's O2 emission to O2 (mmHg) with the Stern-Volmer
#' relation, using the plate's F0 and uncorrected Ksv. It converts each
#' tick's pH emission to pH on a line through the plate's calibration pH at
#' the pH target emission. It then runs seahtrue's validation and returns the
#' plate with its `validation_output`.
#'
#' @param path Character. Path to one plate dataset (`.parquet`).
#' @param date_processed POSIXct. Recorded as `date_processed`. It is not
#'   data: pass a fixed value to make two reads of the same plate dataset
#'   identical.
#' @param max_file_bytes Numeric. Largest plate dataset file accepted, in
#'   bytes (default 512 MB).
#'
#' @return A one-row nested tibble in seahtrue's plate structure:
#' \describe{
#'   \item{plate_id}{the sha256 of the plate's instrument file}
#'   \item{filepath_seahorse}{the plate dataset's file name (no directory)}
#'   \item{date_run}{when the plate was measured (UTC)}
#'   \item{date_processed}{as passed}
#'   \item{assay_info}{plate-wide values: plate_id, date_run, the pH target
#'     emission, the calibration constants, and plate_dataset_schema_version
#'     and plate_dataset_schema_sha256}
#'   \item{injection_info}{measurement, interval, injection}
#'   \item{raw_data}{one row per well per tick, with O2 (mmHg), pH and the
#'     background columns. Every well is kept. `flagged_well` and
#'     `plate_flagged_well` are two manual flags a person set while looking
#'     at the data; both are carried as data. Their one effect: a Background
#'     well flagged in either column is left out of the background average.
#'     A plate whose every Background well is flagged is refused.}
#'   \item{rate_data}{zero rows: no OCR or ECAR is computed by this reader}
#'   \item{validation_output}{seahtrue's validation result. Because the
#'     reader refuses any plate without 96 wells, `all_96_wells_are_present`
#'     is always TRUE on a returned plate.}
#' }
#'
#' @export
read_plate_dataset <- function(path,
                               date_processed = Sys.time(),
                               max_file_bytes = 512 * 1024^2) {
    rlang::check_required(path)
    if (!is.character(path) || length(path) != 1L || is.na(path)) {
        refuse_plate_dataset("`path` must be one file path.")
    }
    if (!inherits(date_processed, "POSIXct") || length(date_processed) != 1L) {
        refuse_plate_dataset("`date_processed` must be one POSIXct value.")
    }
    if (!file.exists(path) || dir.exists(path)) {
        refuse_plate_dataset("The plate dataset file does not exist.")
    }
    if (file.size(path) > max_file_bytes) {
        refuse_plate_dataset(sprintf(
            "The plate dataset is larger than %s bytes.", format(max_file_bytes)
        ))
    }

    contract <- plate_dataset_schema_v1()

    # Everything up to here reads only the file's footer.
    meta <- nanoparquet::read_parquet_metadata(path)
    version <- check_plate_dataset_schema_version(meta)
    footer_rows <- check_plate_dataset_size(meta)
    check_plate_dataset_columns(meta, contract)

    df <- nanoparquet::read_parquet(
        path,
        options = nanoparquet::parquet_options(
            class = "data.frame",
            use_arrow_metadata = FALSE
        )
    )
    df <- as_plain_columns(df, names(contract))
    if (nrow(df) != footer_rows) {
        refuse_plate_dataset("The rows read differ from the file's own row count.")
    }
    if (nrow(df) > PLATE_DATASET_MAX_ROWS) {
        refuse_plate_dataset("The plate dataset has more rows than allowed.")
    }

    check_plate_dataset_values(df, contract)
    date_run <- parse_date_run(df$date_run[1])

    plate <- build_plate(df, date_run, date_processed, basename(path), version)
    validate_preprocessed(plate, define_qc_ranges(
        O2_min = 50, O2_max = 180, pH_min = 6.8, pH_max = 7.6
    ))
}

# Limits ----------------------------------------------------------------------

PLATE_DATASET_WELLS <- 96L
PLATE_DATASET_MAX_TICKS <- 20000L
PLATE_DATASET_MAX_ROWS <- PLATE_DATASET_WELLS * PLATE_DATASET_MAX_TICKS
PLATE_DATASET_MAX_UNCOMPRESSED <- 2 * 1024^3
PLATE_DATASET_MAX_STRING_BYTES <- 256L

# Coefficients of the pH sensor slope: slope = s0 + s1 * pH_gain1 +
# s2 * pH_gain2 (pH units per emission unit). Taken from the lab's
# established pH conversion; provenance is recorded with the work item that
# added this reader.
PH_SLOPE_S0 <- 0.0001338185
PH_SLOPE_S1 <- -0.1334797
PH_SLOPE_S2 <- -4.386826e-06

# Refusals ----------------------------------------------------------------------

# Every refusal is a classed error. Values taken from the file are shown only
# through quote_file_value(), never interpolated as a template.
refuse_plate_dataset <- function(message) {
    rlang::abort(message, class = "seahtrue_plate_dataset_refused")
}

quote_file_value <- function(x) {
    x <- as.character(x)[1]
    if (is.na(x)) return("<missing>")
    x <- iconv(x, from = "UTF-8", to = "ASCII", sub = "?")
    if (is.na(x)) return("<unreadable>")
    if (nchar(x, type = "bytes") > 64L) x <- paste0(substr(x, 1L, 64L), "...")
    encodeString(x, quote = "\"")
}

# Footer checks -------------------------------------------------------------------

check_plate_dataset_schema_version <- function(meta) {
    kv <- meta$file_meta_data$key_value_metadata[[1]]
    known <- known_plate_dataset_schema_versions()
    known_text <- paste(known, collapse = ", ")
    found <- NULL
    if (is.data.frame(kv) && nrow(kv) > 0) {
        found <- as.character(kv$value[as.character(kv$key) ==
                                            "plate_dataset_schema_version"])
    }
    if (length(found) == 0L) {
        refuse_plate_dataset(paste0(
            "The plate dataset has no plate_dataset_schema_version; ",
            "known versions: ", known_text, "."
        ))
    }
    if (length(found) > 1L || !(found %in% known)) {
        refuse_plate_dataset(paste0(
            "Unknown plate dataset schema version ", quote_file_value(found[1]),
            "; known versions: ", known_text, "."
        ))
    }
    found
}

check_plate_dataset_size <- function(meta) {
    rows <- as.numeric(meta$file_meta_data$num_rows[1])
    if (is.na(rows) || rows > PLATE_DATASET_MAX_ROWS) {
        refuse_plate_dataset("The plate dataset has more rows than allowed.")
    }
    uncompressed <- sum(as.numeric(meta$column_chunks$total_uncompressed_size))
    if (is.na(uncompressed) || uncompressed > PLATE_DATASET_MAX_UNCOMPRESSED) {
        refuse_plate_dataset("The plate dataset is too large once uncompressed.")
    }
    rows
}

check_plate_dataset_columns <- function(meta, contract) {
    schema <- meta$schema
    leaves <- schema[!is.na(schema$r_col), , drop = FALSE]
    found <- as.character(leaves$name)
    expected <- names(contract)
    missing <- setdiff(expected, found)
    extra <- setdiff(found, expected)
    if (length(missing) > 0L) {
        refuse_plate_dataset(paste0(
            "The plate dataset lacks columns: ",
            paste(missing, collapse = ", "), "."
        ))
    }
    if (length(extra) > 0L || anyDuplicated(found)) {
        shown <- vapply(utils::head(extra, 10L), quote_file_value, "")
        refuse_plate_dataset(paste0(
            "The plate dataset has columns outside schema version 1: ",
            paste(shown, collapse = ", "), "."
        ))
    }
    if (!identical(found, expected)) {
        refuse_plate_dataset("The plate dataset's columns are not in schema order.")
    }
    for (name in expected) {
        leaf <- leaves[leaves$name == name, , drop = FALSE]
        want <- contract[[name]]
        ok <- identical(as.character(leaf$type), want$physical)
        if (ok && !is.na(want$logical)) {
            ok <- identical(as.character(leaf$converted_type), "UTF8") ||
                identical(logical_type_name(leaf$logical_type), want$logical)
        }
        if (!ok) {
            refuse_plate_dataset(paste0(
                "Column ", name, " is not of type ", want$physical,
                if (!is.na(want$logical)) paste0(" (", want$logical, ")"), "."
            ))
        }
    }
    invisible(TRUE)
}

logical_type_name <- function(x) {
    x <- x[[1]]
    if (is.null(x) || length(x) == 0L) return(NA_character_)
    if (is.list(x)) x <- x[[1]]
    as.character(x)[1]
}

# Value checks ------------------------------------------------------------------

as_plain_columns <- function(df, columns) {
    out <- lapply(columns, function(name) {
        x <- df[[name]]
        if (!is.atomic(x)) {
            refuse_plate_dataset(paste0("Column ", name, " is not plain data."))
        }
        attributes(x) <- NULL
        x
    })
    names(out) <- columns
    attr(out, "row.names") <- .set_row_names(length(out[[1]]))
    class(out) <- "data.frame"
    out
}

check_plate_dataset_values <- function(df, contract) {
    for (name in names(contract)) {
        x <- df[[name]]
        if (!contract[[name]]$nullable && anyNA(x)) {
            refuse_plate_dataset(paste0("Column ", name, " has missing values."))
        }
        if (is.double(x) && any(is.infinite(x) | is.nan(x))) {
            refuse_plate_dataset(paste0("Column ", name, " has a non-finite value."))
        }
        if (is.character(x) &&
            any(nchar(x, type = "bytes", allowNA = TRUE) >
                    PLATE_DATASET_MAX_STRING_BYTES, na.rm = TRUE)) {
            refuse_plate_dataset(paste0("Column ", name, " has an over-long value."))
        }
    }

    sha <- df$instrument_file_sha256[1]
    if (!grepl("^[0-9a-f]{64}$", sha)) {
        refuse_plate_dataset(paste0(
            "instrument_file_sha256 is not a lowercase sha256: ",
            quote_file_value(sha), "."
        ))
    }

    bad_wells <- !grepl("^[A-H](0[1-9]|1[0-2])$", df$well)
    if (any(bad_wells)) {
        refuse_plate_dataset(paste0(
            "Well names must be A01 to H12; found ",
            quote_file_value(df$well[which(bad_wells)[1]]), "."
        ))
    }
    wells <- unique(df$well)
    if (length(wells) != PLATE_DATASET_WELLS) {
        refuse_plate_dataset(sprintf(
            "Schema version 1 needs %d wells; found %d.",
            PLATE_DATASET_WELLS, length(wells)
        ))
    }
    if (anyDuplicated(df[, c("well", "tick")])) {
        refuse_plate_dataset("A well has the same tick twice.")
    }
    ticks <- unique(df$tick)
    if (length(ticks) > PLATE_DATASET_MAX_TICKS) {
        refuse_plate_dataset("The plate dataset has more ticks than allowed.")
    }
    if (nrow(df) != length(wells) * length(ticks)) {
        refuse_plate_dataset("Not every well has every tick.")
    }
    if (any(df$tick < 0L) || any(df$measurement < 1L)) {
        refuse_plate_dataset("Ticks must be 0 or more and measurements 1 or more.")
    }

    for (name in names(contract)) {
        level <- contract[[name]]$level
        by <- switch(level,
            plate = rep(1L, nrow(df)),
            well = df$well,
            measurement = df$measurement,
            tick = df$tick,
            NULL
        )
        if (!is.null(by) && !constant_within(df[[name]], by)) {
            refuse_plate_dataset(paste0(
                "Column ", name, " must have one value per ", level, "."
            ))
        }
    }
    invisible(TRUE)
}

constant_within <- function(x, by) {
    key <- ifelse(is.na(x), "\001NA", as.character(x))
    all(tapply(key, by, function(v) length(unique(v)) == 1L))
}

parse_date_run <- function(x) {
    pattern <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]{1,9})?Z$"
    if (!grepl(pattern, x)) {
        refuse_plate_dataset(paste0(
            "date_run is not ISO 8601 UTC: ", quote_file_value(x), "."
        ))
    }
    value <- as.POSIXct(sub("Z$", "", x), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
    if (is.na(value)) {
        refuse_plate_dataset(paste0(
            "date_run is not a valid time: ", quote_file_value(x), "."
        ))
    }
    value
}

# Conversions -------------------------------------------------------------------

# O2 conversion: Stern-Volmer, (1 / Ksv) * (F0 / emission - 1), with the
# plate's uncorrected Ksv.
convert_O2_emission <- function(O2_em_corr, O2_F0, O2_ksv) {
    (1 / O2_ksv) * ((O2_F0 / O2_em_corr) - 1)
}

# pH conversion: a line through the calibration pH at the pH target emission,
# with its slope from the pH sensor gains.
convert_pH_emission <- function(pH_em_corr_corr, pH_target_emission, pH_0,
                                pH_gain1, pH_gain2) {
    slope <- PH_SLOPE_S0 + PH_SLOPE_S1 * pH_gain1 + PH_SLOPE_S2 * pH_gain2
    pH_0 + slope * (pH_em_corr_corr - pH_target_emission)
}

# Building the plate --------------------------------------------------------------

CALIBRATION_CONSTANTS <- c(
    "O2_target_emission", "O2_F0", "O2_ksv", "O2_0_mmHg", "O2_0_mM",
    "chamber_volume", "tau_AC", "tau_W", "tau_C", "tau_P", "plate_volume",
    "pH_0", "pH_gain1", "pH_gain2"
)

build_plate <- function(df, date_run, date_processed, file_name, version) {
    # get_timing_info() and calc_background() rely on tick order.
    df <- df[order(df$tick, df$well), , drop = FALSE]
    rownames(df) <- NULL

    plate_id <- df$instrument_file_sha256[1]
    elapsed <- df$time_s - min(df$time_s)

    raw <- tibble::tibble(
        plate_id = plate_id,
        well = df$well,
        measurement = df$measurement,
        tick = df$tick,
        timescale = round(elapsed),
        minutes = elapsed / 60,
        group = df$group,
        interval = df$interval,
        injection = df$injection,
        O2_em_corr = df$O2_em_corr,
        pH_em_corr = df$pH_em_corr,
        O2_mmHg = convert_O2_emission(df$O2_em_corr, df$O2_F0, df$O2_ksv),
        pH_em_corr_corr = correct_pH_em_corr(
            df$pH_em_corr, df$pH_cal_em, df$pH_target_emission
        ),
        bufferfactor = df$bufferfactor,
        cell_n = df$cell_n,
        normalisation_unit = df$normalisation_unit,
        normalisation_scale_factor = df$normalisation_scale_factor,
        flagged_well = df$flagged_well,
        plate_flagged_well = df$plate_flagged_well
    )
    raw$pH <- convert_pH_emission(
        raw$pH_em_corr_corr, df$pH_target_emission, df$pH_0,
        df$pH_gain1, df$pH_gain2
    )
    # A zero emission, F0, Ksv or calibration emission would give Inf or NaN,
    # which validation would pass over silently.
    for (name in c("O2_mmHg", "pH_em_corr_corr", "pH")) {
        if (!all(is.finite(raw[[name]]))) {
            refuse_plate_dataset(paste0(
                "The plate dataset gives a non-finite ", name, " value."
            ))
        }
    }

    # A background well is exactly a well whose group is "Background";
    # calc_background() selects it that way and leaves out wells marked
    # flagged_well. A background well flagged by a person in either flag
    # column is left out of the background average (Vincent: "flagged
    # background should be excluded!"). The flags exclude nothing else:
    # every well stays in raw_data, with both flag columns as data.
    background_wells <- raw$group == "Background"
    flagged_either <- raw$flagged_well | raw$plate_flagged_well
    if (any(background_wells) && all(flagged_either[background_wells])) {
        refuse_plate_dataset(paste0(
            "Every Background well is flagged, so no background can be ",
            "computed."
        ))
    }
    for_background <- raw
    for_background$flagged_well <- flagged_either
    background <- calc_background(for_background)
    raw <- dplyr::left_join(raw, background, by = "tick")

    raw <- raw[, c(
        "plate_id", "well", "measurement", "tick", "timescale", "minutes",
        "group", "interval", "injection", "O2_em_corr", "pH_em_corr",
        "O2_mmHg", "pH", "pH_em_corr_corr", "O2_em_corr_bkg",
        "pH_em_corr_bkg", "O2_mmHg_bkg", "pH_bkgd", "pH_em_corr_corr_bkg",
        "bufferfactor", "cell_n", "normalisation_unit",
        "normalisation_scale_factor", "flagged_well", "plate_flagged_well"
    )]

    first <- df[1, , drop = FALSE]
    assay_info <- tibble::tibble(
        plate_id = plate_id,
        date_run = date_run,
        pH_target_emission = first$pH_target_emission,
        !!!as.list(first[, CALIBRATION_CONSTANTS, drop = FALSE]),
        plate_dataset_schema_version = version,
        plate_dataset_schema_sha256 = PLATE_DATASET_SCHEMA_V1_SHA256
    )

    injection_info <- unique(df[, c("measurement", "interval", "injection")])
    injection_info <- tibble::as_tibble(
        injection_info[order(injection_info$measurement), , drop = FALSE]
    )

    tibble::tibble(
        plate_id = plate_id,
        filepath_seahorse = list(tibble::tibble(
            directory_path = NA_character_,
            base_name = file_name,
            full_path = NA_character_
        )),
        date_run = date_run,
        date_processed = date_processed,
        assay_info = list(assay_info),
        injection_info = list(injection_info),
        raw_data = list(raw),
        rate_data = list(tibble::tibble(
            plate_id = character(),
            well = character(),
            measurement = integer(),
            group = character()
        ))
    )
}
