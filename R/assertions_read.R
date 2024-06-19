validate_O2_pH_levels <- function(xf_raw_pr,
                                  tick_range_rule,
                                  start_tick_range_rule) {
    # internal functions
    validate_ticks <- function(nested_df, rule) {
        nested_df %>%
            dplyr::mutate(
                validate =
                    purrr::map(
                        .x = .data$data,
                        .f = ~ .x %>%
                            validate::confront(., rule) %>%
                            validate::summary() %>%
                            dplyr::as_tibble() %>%
                            dplyr::filter(fails != 0)
                    )
            ) %>%
            tidyr::unnest(.data$validate) %>%
            dplyr::select(-.data$data) %>%
            tidyr::nest(.by = c(.data$well, .data$expression)) %>% 
            dplyr::mutate(
                failed =
                    purrr::map(
                        .x = .data$data,
                        .f = ~ .x %>%
                            dplyr::select(.data$measurement) %>%
                            dplyr::pull(.data$measurement)
                    )
            ) %>%
            dplyr::mutate(n_failed = purrr::map_dbl(
                .x = .data$data,
                .f = ~ .x %>% nrow()
            )) %>%
            dplyr::select(-.data$data)
    }


    get_failed_values <- function(failed_df, original_df, tick = "first") {
        if (tick == "first") {
            df <- failed_df %>%
                tidyr::unnest(c(.data$failed)) %>%
                dplyr::mutate(
                    values =
                        purrr::map2(
                            .x = .data$failed,
                            .y = .data$well,
                            .f = ~ original_df %>%
                                dplyr::filter(well == .y) %>%
                                dplyr::filter(measurement == .x) %>%
                                dplyr::slice(1) %>%
                                dplyr::select(.data$O2_mmHg, .data$pH)
                        )
                ) %>%
                tidyr::unnest(c(.data$values)) %>%
                dplyr::mutate(param = stringr::str_sub(.data$expression, 10, 11)) %>%
                dplyr::mutate(label = "first_tick")
        }

        if (tick == "last") {
            df <- failed_df %>%
                tidyr::unnest(c(.data$failed)) %>%
                dplyr::mutate(
                    values =
                        purrr::map2(
                            .x = .data$failed,
                            .y = .data$well,
                            .f = ~ original_df %>%
                                dplyr::filter(well == .y) %>%
                                dplyr::filter(measurement == .x) %>%
                                dplyr::slice_tail(n = 1) %>%
                                dplyr::select(.data$O2_mmHg, .data$pH)
                        )
                ) %>%
                tidyr::unnest(c(.data$values)) %>%
                dplyr::mutate(param = stringr::str_sub(.data$expression, 10, 11)) %>%
                dplyr::mutate(label = "last_tick")
        }

        if (tick == "start") {
            df <- failed_df %>%
                tidyr::unnest(c(.data$failed)) %>%
                dplyr::mutate(
                    values =
                        purrr::map2(
                            .x = .data$failed,
                            .y = .data$well,
                            .f = ~ original_df %>%
                                dplyr::filter(well == .y) %>%
                                dplyr::slice(1) %>%
                                dplyr::select(.data$O2_mmHg, .data$pH)
                        )
                ) %>%
                tidyr::unnest(c(.data$values)) %>%
                dplyr::mutate(param = stringr::str_sub(.data$expression, 10, 11)) %>%
                dplyr::mutate(label = "start_tick")
        }

        return(df)
    }

    # first tick of measurement fail
    failed_df_first_tick <- xf_raw_pr %>%
        dplyr::slice_head(
            n = 1,
            by = c(.data$well, .data$measurement)
        ) %>%
        tidyr::nest(.by = c(.data$well, .data$measurement)) %>%
        validate_ticks( tick_range_rule) %>%
        get_failed_values( xf_raw_pr, tick = "first")

    failed_df_last_tick <- xf_raw_pr %>%
        dplyr::slice_tail(
            n = 1,
            by = c(.data$well, .data$measurement)
        ) %>%
        tidyr::nest(.by = c(.data$well, .data$measurement)) %>%
        validate_ticks(tick_range_rule) %>%
        get_failed_values(xf_raw_pr, tick = "last")

    failed_df_start_tick <- xf_raw_pr %>%
        dplyr::slice_head(
            n = 1,
            by = c(.data$well)
        ) %>%
        tidyr::nest(.by = c(.data$well, .data$measurement)) %>%
        validate_ticks(start_tick_range_rule) %>%
        get_failed_values(xf_raw_pr, tick = "start")

    failed_ticks_combined <- dplyr::bind_rows(
        failed_df_last_tick,
        failed_df_first_tick,
        failed_df_start_tick
    ) %>%
        tidyr::nest(.by = c(.data$well, .data$n_failed, .data$param, .data$label)) %>%
        dplyr::arrange(.data$well, .data$label)

    return(failed_ticks_combined)
}

validatie_tick_is_linear <- function(xf_raw_pr) {
    # is linear for tick
    xf_raw_pr %>%
        dplyr::slice(1, .by = c(.data$measurement, .data$tick)) %>%
        dplyr::select(.data$well, .data$measurement, .data$tick, .data$group) %>%
        dplyr::pull(.data$tick) %>%
        validate::is_linear_sequence(begin = 0)
}

validate_for_NA <- function(xf_raw_pr, NA_rule) {
  
    # NA_df_summary <- validate::confront(xf_raw_pr, NA_rule) %>%
    #     validate::summary() %>%
    #     # pull(expression) %>%
    #     dplyr::mutate(
    #         param_NA =
    #             purrr::map_chr(
    #                 .x = .data$expression,
    #                 .f = ~ stringr::str_extract_all(
    #                     .x, "\\([^()]+\\)"
    #                 ) %>%
    #                     purrr::pluck(1) %>%
    #                     stringr::str_sub(2, -2) %>%
    #                     paste0("_NA")
    #             )
    #     ) %>%
    #     dplyr::as_tibble()
    
    df_with_NAs_identified <- validate::confront(xf_raw_pr, NA_rule) %>%
        validate::values() %>%
        dplyr::as_tibble() %>%
        dplyr::rename_with(~ paste0(.x, "_NA")) %>% 
        #dplyr::rename_with(~ NA_df_summary$param_NA, names(.)) %>%
        dplyr::select(where(~ any(.x == FALSE))) %>%
        dplyr::bind_cols(
          xf_raw_pr %>%
            dplyr::select(.data$well, .data$measurement, .data$tick, .data$group)) %>% 
        dplyr::select(.data$well, .data$measurement, 
                      .data$tick, .data$group, dplyr::everything())
          
    return(df_with_NAs_identified)
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


#' The preprocessed data is validated against rules with the
#' help of the validator package
#' @description
#' The input file that is read and preprocessed is validated
#' for a number of rules. These rules are both physiologic, 
#' in a sense that the parameters that are analyzed with the 
#' Seahorse (O2 and pH) arae checked whether they are in 
#' a physiolgical range (for example). Also there is a rule 
#' for NAs and the timing info is organized.
#' 
#' @param preprocessed_xf_plate 
#'
#' @return  the input prepocessed xf plate data tibble
#' with added extra column with a list of validation output
#' @importFrom validate is_complete in_range
#' @noRd
#' @examples
#' suppressMessages(
#'  system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", 
#'  package = "seahtrue") |> 
#'    read_xfplate() |> 
#'    preprocess_xf_plate() |> 
#'    validate_preprocessed()
#' )
validate_preprocessed <- function(preprocessed_xf_plate) {
    xf_raw_pr <- preprocessed_xf_plate %>%
        purrr::pluck("raw_data", 1)

    # rules
    tick_range_rule <- validate::validator(
        in_range(.data$O2_mmHg, min = 10, max = 200),
        in_range(.data$pH, min = 7, max = 7.5)
    )

    start_tick_range_rule <- validate::validator(
        in_range(.data$O2_mmHg, min = 140, max = 170),
        in_range(.data$pH, min = 7.2, max = 7.6)
    )
    
    NA_rule_names <- c("well", "measurement","tick","timescale","minutes",
      "group","interval","injection","pH_em_corr","O2_em_corr",
      "O2_mmHg","pH","pH_em_corr_corr","O2_em_corr_bkg",
      "pH_em_corr_bkg","O2_mmHg_bkg","pH_bkgd","pH_em_corr_corr_bkg",
      "bufferfactor","cell_n","flagged_well")
    
    NA_rule <- validate::validator(
      .data = data.frame(rule = c("is_complete(well)",
                                  "is_complete(measurement)",
                                  "is_complete(tick)",
                                  "is_complete(timescale)",
                                  "is_complete(minutes)",
                                  "is_complete(group)",
                                  "is_complete(interval)",
                                  "is_complete(injection)",
                                  "is_complete(pH_em_corr)",
                                  "is_complete(O2_em_corr)",
                                  "is_complete(O2_mmHg)",
                                  "is_complete(pH)",
                                  "is_complete(pH_em_corr_corr)",
                                  "is_complete(O2_em_corr_bkg)",
                                  "is_complete(pH_em_corr_bkg)",
                                  "is_complete(O2_mmHg_bkg)",
                                  "is_complete(pH_bkgd)",
                                  "is_complete(pH_em_corr_corr_bkg)",
                                  "is_complete(bufferfactor)",
                                  "is_complete(cell_n)",
                                  "is_complete(flagged_well)"),
                         name = NA_rule_names,
                         description = NA_rule_names))
      
    # call validate functions
    df_with_NAs_identified <-
        validate_for_NA(xf_raw_pr, NA_rule)
    all_96_wells_are_present <-
        validate_well_number(xf_raw_pr)
    time_info <-
        get_timing_info(xf_raw_pr)
    tick_is_linear <-
        validatie_tick_is_linear(xf_raw_pr)
    failed_ticks_combined <-
        validate_O2_pH_levels(
            xf_raw_pr,
            tick_range_rule,
            start_tick_range_rule
        )


    all_validator_rules <-
        list(
            NA_rule = NA_rule,
            tick_range_rule = tick_range_rule,
            start_tick_range_rule = start_tick_range_rule,
            "is_linear_sequence_tick" = "is_linear_sequence_tick",
            "all_96_wells_are_present" = "all_96_wells_are_present"
        )

    validation_output <-
        list(
            all_96_wells_are_present = all_96_wells_are_present,
            tick_is_linear = tick_is_linear,
            time_info = time_info,
            df_with_NAs_identified = df_with_NAs_identified,
            failed_ticks_combined = failed_ticks_combined,
            all_validator_rules = all_validator_rules
        )

    cli::cli_alert_info("Finished validating the input data")


    return(preprocessed_xf_plate %>%
        dplyr::mutate(validation_output = list(validation_output)))
}
