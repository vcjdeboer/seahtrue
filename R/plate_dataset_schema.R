# Plate dataset schema -------------------------------------------------------
#
# The plate dataset is the Parquet file the parser writes for one plate: one
# row per well per tick. Its schema is defined, versioned, by the parser
# (parser/plate_dataset_schema_v1.json in the lab system that owns the parser);
# this file is a copy of version 1 of that schema, and the parser's spelling
# and types win. read_plate_dataset() refuses any plate dataset whose
# plate_dataset_schema_version (Parquet key-value metadata) is not listed in
# known_plate_dataset_schema_versions().

#' Known plate dataset schema versions
#' @return character vector of the plate dataset schema versions this
#'   seahtrue can read.
#' @noRd
known_plate_dataset_schema_versions <- function() {
  "1"
}

# sha256 of the parser's schema file (parser/plate_dataset_schema_v1.json)
# this copy was taken from, and the parser commit it was read at. The sha256
# is returned in assay_info by read_plate_dataset(), so a caller can compare
# it with the parser's file without using seahtrue internals.
PLATE_DATASET_SCHEMA_V1_SHA256 <-
  "5b6ea40df01cd3ee2fca80b04aad0a23b17451aa31e92a1386c9cdb7910bc165"
PLATE_DATASET_SCHEMA_V1_PARSER_COMMIT <-
  "b4d6a762a563ca58188a3c73860171e155c8917f"

# Version 1 column contract, in the parser's column order. level says at
# which level a column's value must be constant: "plate" (whole file),
# "well" (within a well), "measurement" (within a measurement), "tick"
# (within a tick, across wells), or "row" (free per well per tick). physical
# is the Parquet physical type; logical is the Parquet logical type
# ("STRING" for text); nullable says whether a value may be missing.
plate_dataset_schema_v1 <- function() {
  col <- function(name, physical, level, nullable = FALSE, logical = NA_character_) {
    list(name = name, physical = physical, logical = logical,
         level = level, nullable = nullable)
  }
  str <- function(name, level, nullable = FALSE) {
    col(name, "BYTE_ARRAY", level, nullable, "STRING")
  }
  dbl <- function(name, level, nullable = FALSE) col(name, "DOUBLE", level, nullable)
  i32 <- function(name, level) col(name, "INT32", level)

  cols <- list(
    str("instrument_file_sha256", "plate"),
    str("date_run", "plate"),
    str("well", "row"),
    str("group", "well"),
    col("flagged_well", "BOOLEAN", "well"),
    dbl("cell_n", "well", nullable = TRUE),
    str("normalisation_unit", "well", nullable = TRUE),
    dbl("normalisation_scale_factor", "well", nullable = TRUE),
    dbl("bufferfactor", "well", nullable = TRUE),
    i32("measurement", "tick"),
    i32("interval", "measurement"),
    str("injection", "measurement"),
    i32("tick", "row"),
    dbl("time_s", "tick"),
    dbl("O2_em_corr", "row"),
    dbl("pH_em_corr", "row"),
    dbl("O2_cal_em", "well"),
    dbl("pH_cal_em", "well"),
    dbl("O2_target_emission", "plate"),
    dbl("pH_target_emission", "plate"),
    dbl("O2_F0", "plate"),
    dbl("O2_ksv", "plate"),            # the uncorrected Ksv
    dbl("O2_0_mmHg", "plate"),
    dbl("O2_0_mM", "plate"),
    dbl("chamber_volume", "plate"),
    dbl("tau_AC", "plate"),
    dbl("tau_W", "plate"),
    dbl("tau_C", "plate"),
    dbl("tau_P", "plate"),
    dbl("plate_volume", "plate"),
    dbl("pH_0", "plate"),
    dbl("pH_gain1", "plate"),
    dbl("pH_gain2", "plate")
  )
  names(cols) <- vapply(cols, `[[`, "", "name")
  cols
}

# Background wells: a background well is exactly a well whose group is
# "Background" (a machine-written name; the parser maps a lab member's own
# labels for such wells to it). seahtrue's calc_background() and plot
# functions select background wells by that group name.
