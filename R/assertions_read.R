validate_O2_pH_levels <- function(xf_raw_pr,
                                  tick_range_rule,
                                  start_tick_range_rule) {
    # internal functions
    validate_ticks <- function(nested_df, rule) {
        nested_df %>%
            dplyr::mutate(
                validate =
                    purrr::map(
                        .x = data,
                        .f = ~ .x %>%
                            validate::confront(., rule) %>%
                            validate::summary() %>%
                            dplyr::as_tibble() %>%
                            dplyr::filter(fails != 0)
                    )
            ) %>%
            tidyr::unnest(validate) %>%
            dplyr::select(-data) %>%
            tidyr::nest(.by = c(well, expression)) %>% 
            dplyr::mutate(
                failed =
                    purrr::map(
                        .x = data,
                        .f = ~ .x %>%
                            dplyr::select(measurement) %>%
                            dplyr::pull(measurement)
                    )
            ) %>%
            dplyr::mutate(n_failed = purrr::map_dbl(
                .x = data,
                .f = ~ .x %>% nrow()
            )) %>%
            dplyr::select(-data)
    }


    get_failed_values <- function(failed_df, original_df, tick = "first") {
        if (tick == "first") {
            df <- failed_df %>%
                tidyr::unnest(c(failed)) %>%
                dplyr::mutate(
                    values =
                        purrr::map2(
                            .x = failed,
                            .y = well,
                            .f = ~ original_df %>%
                                dplyr::filter(well == .y) %>%
                                dplyr::filter(measurement == .x) %>%
                                dplyr::slice(1) %>%
                                dplyr::select(O2_mmHg, pH)
                        )
                ) %>%
                tidyr::unnest(c(values)) %>%
                dplyr::mutate(param = stringr::str_sub(expression, 10, 11)) %>%
                dplyr::mutate(label = "first_tick")
        }

        if (tick == "last") {
            df <- failed_df %>%
                tidyr::unnest(c(failed)) %>%
                dplyr::mutate(
                    values =
                        purrr::map2(
                            .x = failed,
                            .y = well,
                            .f = ~ original_df %>%
                                dplyr::filter(well == .y) %>%
                                dplyr::filter(measurement == .x) %>%
                                dplyr::slice_tail(n = 1) %>%
                                dplyr::select(O2_mmHg, pH)
                        )
                ) %>%
                tidyr::unnest(c(values)) %>%
                dplyr::mutate(param = stringr::str_sub(expression, 10, 11)) %>%
                dplyr::mutate(label = "last_tick")
        }

        if (tick == "start") {
            df <- failed_df %>%
                tidyr::unnest(c(failed)) %>%
                dplyr::mutate(
                    values =
                        purrr::map2(
                            .x = failed,
                            .y = well,
                            .f = ~ original_df %>%
                                dplyr::filter(well == .y) %>%
                                dplyr::slice(1) %>%
                                dplyr::select(O2_mmHg, pH)
                        )
                ) %>%
                tidyr::unnest(c(values)) %>%
                dplyr::mutate(param = stringr::str_sub(expression, 10, 11)) %>%
                dplyr::mutate(label = "start_tick")
        }

        return(df)
    }

    # first tick of measurement fail
    failed_df_first_tick <- xf_raw_pr %>%
        dplyr::slice_head(
            n = 1,
            by = c(well, measurement)
        ) %>%
        tidyr::nest(.by = c(well, measurement)) %>%
        validate_ticks(., tick_range_rule) %>%
        get_failed_values(., xf_raw_pr, tick = "first")

    failed_df_last_tick <- xf_raw_pr %>%
        dplyr::slice_tail(
            n = 1,
            by = c(well, measurement)
        ) %>%
        tidyr::nest(.by = c(well, measurement)) %>%
        validate_ticks(., tick_range_rule) %>%
        get_failed_values(., xf_raw_pr, tick = "last")

    failed_df_start_tick <- xf_raw_pr %>%
        dplyr::slice_head(
            n = 1,
            by = c(well)
        ) %>%
        tidyr::nest(.by = c(well, measurement)) %>%
        validate_ticks(., start_tick_range_rule) %>%
        get_failed_values(., xf_raw_pr, tick = "start")

    failed_ticks_combined <- dplyr::bind_rows(
        failed_df_last_tick,
        failed_df_first_tick,
        failed_df_start_tick
    ) %>%
        tidyr::nest(.by = c(well, n_failed, param, label)) %>%
        dplyr::arrange(well, label)

    return(failed_ticks_combined)
}

validatie_tick_is_linear <- function(xf_raw_pr) {
    # is linear for tick
    xf_raw_pr %>%
        dplyr::slice(1, .by = c(measurement, tick)) %>%
        dplyr::select(well, measurement, tick, group) %>%
        dplyr::pull(tick) %>%
        validate::is_linear_sequence(begin = 0)
}

validate_for_NA <- function(xf_raw_pr, NA_rule) {
    NA_df_summary <- validate::confront(xf_raw_pr, NA_rule) %>%
        validate::summary() %>%
        # pull(expression) %>%
        dplyr::mutate(
            param_NA =
                purrr::map_chr(
                    .x = expression,
                    .f = ~ stringr::str_extract_all(
                        .x, "\\([^()]+\\)"
                    ) %>%
                        purrr::pluck(1) %>%
                        stringr::str_sub(2, -2) %>%
                        paste0(., "_NA")
                )
        ) %>%
        dplyr::as_tibble()

    df_with_NAs_identified <- validate::confront(xf_raw_pr, NA_rule) %>%
        validate::values() %>%
        dplyr::as_tibble() %>%
        dplyr::rename_with(~ NA_df_summary$param_NA, names(.)) %>%
        dplyr::select(where(~ any(. == FALSE))) %>%
        dplyr::bind_cols(xf_raw_pr %>%
            dplyr::select(well, measurement, tick, group), .)

    return(df_with_NAs_identified)
}

validate_well_number <- function(xf_raw_pr) {
    number_of_wells_per_plate <-
        xf_raw_pr %>%
        dplyr::pull(well) %>%
        unique() %>%
        length()

    all_96_wells_are_present <- number_of_wells_per_plate == 96

    return(all_96_wells_are_present)
}

get_timing_info <- function(xf_raw_pr) {
    number_of_ticks_per_measurement <-
        xf_raw_pr %>%
        dplyr::slice(1, .by = c(measurement, tick)) %>%
        count(measurement) %>%
        dplyr::rename(number_of_ticks = n)

    time_info <- xf_raw_pr %>%
        dplyr::slice(1, .by = c(measurement, tick)) %>%
        dplyr::mutate(
            st = first(timescale),
            end = last(timescale),
            .by = measurement
        ) %>%
        dplyr::select(st, end) %>%
        dplyr::mutate(per_meas = end - st) %>%
        unique() %>%
        dplyr::mutate(lagged = lag(end)) %>%
        dplyr::mutate(lagged = case_when(is.na(lagged) ~ 0,
            .default = lagged
        )) %>%
        dplyr::mutate(wait_mix = st - lagged) %>%
        dplyr::select(-lagged) %>%
        dplyr::bind_cols(number_of_ticks_per_measurement) %>%
        dplyr::select(
            measurement,
            number_of_ticks,
            st, end, per_meas, wait_mix
        ) %>%
        dplyr::mutate(
            st_min = st / 60,
            end_min = end / 60,
            per_meas_min = per_meas / 60,
            wait_mix_min = wait_mix / 60
        ) %>%
        dplyr::rename(
            st_sec = st,
            end_sec = end,
            per_meas_sec = per_meas,
            wait_mix_sec = wait_mix
        )

    return(time_info)
}

validate_preprocessed <- function(preprocessed_xf_plate) {
    xf_raw_pr <- preprocessed_xf_plate %>%
        purrr::pluck("raw_data", 1)

    # rules
    tick_range_rule <- validate::validator(
        in_range(O2_mmHg, min = 10, max = 200),
        in_range(pH, min = 7, max = 7.5)
    )

    start_tick_range_rule <- validate::validator(
        in_range(O2_mmHg, min = 140, max = 170),
        in_range(pH, min = 7.2, max = 7.6)
    )

    NA_rule <- validate::validator(
        is_complete(well),
        is_complete(measurement),
        is_complete(tick),
        is_complete(timescale),
        is_complete(minutes),
        is_complete(group),
        is_complete(interval),
        is_complete(injection),
        is_complete(pH_em_corr),
        is_complete(O2_em_corr),
        is_complete(O2_mmHg),
        is_complete(pH),
        is_complete(pH_em_corr_corr),
        is_complete(O2_em_corr_bkg),
        is_complete(pH_em_corr_bkg),
        is_complete(O2_mmHg_bkg),
        is_complete(pH_bkgd),
        is_complete(pH_em_corr_corr_bkg),
        is_complete(bufferfactor),
        is_complete(cell_n),
        is_complete(flagged_well)
    )


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
