validate_O2_pH_levels <- function(xf_raw_pr, qc_ranges) {
  
  check_tick_ranges_for_well <- function(df, ranges) {
    # Extract QC thresholds
    O2_min <- ranges %>% purrr::pluck("O2", "min")
    O2_max <- ranges %>% purrr::pluck("O2", "max")
    pH_min <- ranges %>% purrr::pluck("pH", "min")
    pH_max <- ranges %>% purrr::pluck("pH", "max")
    
    # Safety check
    required_cols <- c("well", "group", "tick", "measurement", "O2_mmHg", "pH")
    if (!all(required_cols %in% names(df))) {
      cli::cli_alert_danger("Missing columns in input:")
      cli::cli_alert_info(paste(setdiff(required_cols, names(df)), collapse = ", "))
      stop("Aborting check_tick_ranges()")
    }
    
    if (nrow(df) == 0) {
      return(list(
        flags = tibble::tibble(
          well = NA_character_,
          group = NA_character_,
          start_O2 = NA, first_O2 = NA, last_O2 = NA,
          start_pH = NA, first_pH = NA, last_pH = NA
        ),
        debug = tibble::tibble()
      ))
    }
    
    start_tick <- min(df$tick, na.rm = TRUE)
    
    tick_bounds <- df %>%
      dplyr::group_by(.data$measurement) %>%
      dplyr::summarise(
        min_tick = min(.data$tick, na.rm = TRUE),
        max_tick = max(.data$tick, na.rm = TRUE),
        .groups = "drop"
      )
    
    tick_first <- tick_bounds %>% purrr::pluck("min_tick")
    tick_last  <- tick_bounds %>% purrr::pluck("max_tick")
    
    ticks_to_check <- unique(c(start_tick, tick_first, tick_last))
    
    tick_subset <- df %>%
      dplyr::filter(.data$tick %in% ticks_to_check) %>%
      dplyr::mutate(
        position = dplyr::case_when(
          tick == start_tick      ~ "start",
          tick %in% tick_first    ~ "first",
          tick %in% tick_last     ~ "last",
          TRUE                    ~ NA_character_
        ),
        out_O2 = !(.data$O2_mmHg >= O2_min & .data$O2_mmHg <= O2_max),
        out_pH = !(.data$pH >= pH_min & .data$pH <= pH_max)
      )
    
    flags <- tick_subset %>%
      dplyr::summarise(
        start_O2 = any(.data$out_O2[.data$position == "start"], na.rm = TRUE),
        first_O2 = any(.data$out_O2[.data$position == "first"], na.rm = TRUE),
        last_O2  = any(.data$out_O2[.data$position == "last"],  na.rm = TRUE),
        start_pH = any(.data$out_pH[.data$position == "start"], na.rm = TRUE),
        first_pH = any(.data$out_pH[.data$position == "first"], na.rm = TRUE),
        last_pH  = any(.data$out_pH[.data$position == "last"],  na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        well = unique(df$well),
        group = unique(df$group)
      ) %>%
      dplyr::relocate(.data$well, .data$group)
    
    debug_out <- tick_subset %>%
      dplyr::filter(.data$out_O2 | .data$out_pH) %>%
      dplyr::mutate(
        well = unique(df$well),
        group = unique(df$group)
      ) %>%
      dplyr::select(.data$well, .data$group, 
                    .data$measurement, .data$tick, 
                    .data$position, .data$O2_mmHg, 
                    .data$out_O2, .data$pH, .data$out_pH)
    
    return(list(flags = flags, debug = debug_out))
  }
  
  nested <- xf_raw_pr %>%
    dplyr::group_by(.data$well, .data$group) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data = purrr::map(.data$data, ~ dplyr::mutate(.x, well = well, group = group)),
      qc_result = purrr::map(.data$data, ~ check_tick_ranges_for_well(.x, ranges = qc_ranges))
    )
  
  flags_df <- nested %>%
    dplyr::pull(.data$qc_result) %>%
    purrr::map_dfr(purrr::pluck, "flags")
  
  debug_df <- nested %>%
    dplyr::pull(.data$qc_result) %>%
    purrr::map_dfr(purrr::pluck, "debug")
  
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
#' - Physiological ranges for oxygen consumption (O₂) and pH
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
