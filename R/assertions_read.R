validate_O2_pH_levels <- function(xf_raw_pr, qc_ranges) {

  # Extract QC thresholds
  O2_min <- qc_ranges %>% purrr::pluck("O2", "min")
  O2_max <- qc_ranges %>% purrr::pluck("O2", "max")
  pH_min <- qc_ranges %>% purrr::pluck("pH", "min")
  pH_max <- qc_ranges %>% purrr::pluck("pH", "max")

  # Safety check
  required_cols <- c("well", "group", "tick", "measurement", "O2_mmHg", "pH")
  if (!all(required_cols %in% names(xf_raw_pr))) {
    cli::cli_alert_danger("Missing columns in input:")
    cli::cli_alert_info(paste(setdiff(required_cols, names(xf_raw_pr)), collapse = ", "))
    stop("Aborting validate_O2_pH_levels()")
  }

  # Whole-plate vectorised QC. The previous implementation nested the data per
  # well and ran the tick-range check well-by-well via purrr::map(); with ~96
  # wells that is hundreds of small dplyr calls, whose per-call overhead is
  # negligible in native R but crippling under webR/wasm (the book timed out at
  # quarto-live's 30s elapsed limit). Grouping by well+group and computing the
  # flags in a handful of grouped operations produces identical output at a
  # fraction of the cost. Note: a tick is unique to one measurement, so a row's
  # tick equalling its own measurement's min/max is equivalent to the previous
  # `tick %in% <all measurements' min/max ticks>` membership test.
  tagged <- xf_raw_pr %>%
    dplyr::group_by(.data$well, .data$group) %>%
    dplyr::mutate(start_tick = min(.data$tick, na.rm = TRUE)) %>%
    dplyr::group_by(.data$well, .data$group, .data$measurement) %>%
    dplyr::mutate(
      meas_min_tick = min(.data$tick, na.rm = TRUE),
      meas_max_tick = max(.data$tick, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      .data$tick == .data$start_tick |
        .data$tick == .data$meas_min_tick |
        .data$tick == .data$meas_max_tick
    ) %>%
    dplyr::mutate(
      position = dplyr::case_when(
        .data$tick == .data$start_tick    ~ "start",
        .data$tick == .data$meas_min_tick ~ "first",
        .data$tick == .data$meas_max_tick ~ "last",
        TRUE                              ~ NA_character_
      ),
      out_O2 = !(.data$O2_mmHg >= O2_min & .data$O2_mmHg <= O2_max),
      out_pH = !(.data$pH >= pH_min & .data$pH <= pH_max)
    )

  flags_df <- tagged %>%
    dplyr::group_by(.data$well, .data$group) %>%
    dplyr::summarise(
      start_O2 = any(.data$out_O2[.data$position == "start"], na.rm = TRUE),
      first_O2 = any(.data$out_O2[.data$position == "first"], na.rm = TRUE),
      last_O2  = any(.data$out_O2[.data$position == "last"],  na.rm = TRUE),
      start_pH = any(.data$out_pH[.data$position == "start"], na.rm = TRUE),
      first_pH = any(.data$out_pH[.data$position == "first"], na.rm = TRUE),
      last_pH  = any(.data$out_pH[.data$position == "last"],  na.rm = TRUE),
      .groups = "drop"
    )

  debug_df <- tagged %>%
    dplyr::filter(.data$out_O2 | .data$out_pH) %>%
    dplyr::select(.data$well, .data$group,
                  .data$measurement, .data$tick,
                  .data$position, .data$O2_mmHg,
                  .data$out_O2, .data$pH, .data$out_pH)

  return(list(flag = flags_df, debug = debug_df))
}

validate_well_number <- function(xf_raw_pr) {
    number_of_wells_per_plate <-
        xf_raw_pr %>%
        dplyr::pull(.data$well) %>%
        unique() %>%
        length()

    all_96_wells_are_present <- number_of_wells_per_plate == 96

    return(all_96_wells_are_present)
}

get_timing_info <- function(xf_raw_pr) {
    number_of_ticks_per_measurement <-
        xf_raw_pr %>%
        dplyr::slice(1, .by = c(.data$measurement, .data$tick)) %>%
        count(.data$measurement) %>%
        dplyr::rename(number_of_ticks = .data$n)

    time_info <- xf_raw_pr %>%
        dplyr::slice(1, .by = c(.data$measurement, .data$tick)) %>%
        dplyr::mutate(
            st = first(.data$timescale),
            end = last(.data$timescale),
            .by = .data$measurement
        ) %>%
        dplyr::select(.data$st, .data$end) %>%
        dplyr::mutate(per_meas = .data$end - .data$st) %>%
        unique() %>%
        dplyr::mutate(lagged = lag(.data$end)) %>%
        dplyr::mutate(lagged = case_when(is.na(.data$lagged) ~ 0,
            .default = .data$lagged
        )) %>%
        dplyr::mutate(wait_mix = .data$st - .data$lagged) %>%
        dplyr::select(-.data$lagged) %>%
        dplyr::bind_cols(number_of_ticks_per_measurement) %>%
        dplyr::select(
          .data$measurement,
          .data$number_of_ticks,
          .data$st, .data$end, .data$per_meas, .data$wait_mix
        ) %>%
        dplyr::mutate(
            st_min = .data$st / 60,
            end_min = .data$end / 60,
            per_meas_min = .data$per_meas / 60,
            wait_mix_min = .data$wait_mix / 60
        ) %>%
        dplyr::rename(
            st_sec = .data$st,
            end_sec = .data$end,
            per_meas_sec = .data$per_meas,
            wait_mix_sec = .data$wait_mix
        )

    return(time_info)
}


#' Validate Preprocessed Seahorse Plate Data
#'
#' @description
#' Validates preprocessed Seahorse XF plate data using a set of rules,
#' including:
#' - Physiological ranges for oxygen consumption (O2) and pH
#' - Presence of all 96 wells
#' - Proper timing structure
#'
#' The validation uses custom logic and QC thresholds. This function is intended to be
#' run after preprocessing (`preprocess_xf_plate()`), and appends a `validation_output`
#' column to the data containing all validation results.
#'
#' @param preprocessed_xf_plate A preprocessed XF plate data object returned by
#'   `preprocess_xf_plate()`, typically a tibble with nested data columns.
#' @param qc_ranges A list of threshold ranges for oxygen and pH levels,
#'   typically defined using a helper function such as `define_qc_ranges()`.
#'
#' @return A tibble (same as `preprocessed_xf_plate`) with an added column
#'   `validation_output`, which is a list-column containing the results of all
#'   validation checks.
#'
#' @noRd
#'
#' @examples
#' suppressMessages({
#'   path <- system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx",
#'     package = "seahtrue"
#'   )
#'   xf <- read_xfplate(path) |>
#'     preprocess_xf_plate()
#'   qc <- define_qc_ranges()
#'   validate_preprocessed(xf, qc)
#' })
validate_preprocessed <- function(preprocessed_xf_plate, qc_ranges) {
    
  xf_raw_pr <- preprocessed_xf_plate %>%
        purrr::pluck("raw_data", 1)

    all_96_wells_are_present <-
        validate_well_number(xf_raw_pr)
    time_info <-
        get_timing_info(xf_raw_pr)
  
    
    failed_ticks_combined <-
      validate_O2_pH_levels(xf_raw_pr, qc_ranges)
    
    all_validator_rules <-
      list(
        tick_range_rule = qc_ranges,
        "all_96_wells_are_present" = "all_96_wells_are_present")

    validation_output <-
      list(all_96_wells_are_present = all_96_wells_are_present,
           time_info = time_info,
           failed_ticks_combined = failed_ticks_combined,
           all_validator_rules = all_validator_rules
      )

    cli::cli_alert_info("Finished validating the input data")


    return(preprocessed_xf_plate %>%
        dplyr::mutate(validation_output = list(validation_output)))
}
