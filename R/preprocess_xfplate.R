# MAIN FUNCTION preprocess_xfplate ------------------------------------
#' Preprocessing the seahorse plate data that was read in read_xfplate.r
#'
#' @description
#' This function performs the preprocessing of the xf_raw and the xf_rate 
#' tibbles
#' and generates the final output nested tibble
#'
#' @param xf list with all necessary seahorse data.
#'
#' @return xf_plate_pr is returned this is a nested tibble with the following 
#' 7 columns:
#'  * plate_id = plate_id (from assay configuration sheet)
#'  * filepath_seahorse = the original input file and its path on local drive
#'  * date = date that seahorse was run (from assay configuration sheet)
#'  * assay_info = parameters from Assay Configuration sheet
#'  * injection_info = info from Operation log sheet
#'  * raw_data = preprocessed raw data from Raw sheet
#'  * rate_data = preprocessed rate data from Rate sheet
#'  One assay (one plate), contains all data in one row
#'
#' @noRd
#' @keywords internal
#'

preprocess_xfplate <- function(xf) {
    xf_raw_pr <- preprocess_xf_raw(
        xf$raw,
        xf$pHcal,
        xf$inj,
        xf$assayinfo,
        xf$buffer,
        xf$norm,
        xf$flagged
    )

    xf_rate_pr <- preprocess_xf_rate(
        xf$rate,
        xf$norm,
        xf$inj,
        xf$flagged
    )

    xf_plate_pr <- xf_raw_pr %>%
        tidyr::nest(.by = plate_id) %>%
        dplyr::mutate(
            filepath_seahorse = list(tibble::tibble(
                directory_path = dirname(xf$filepath_seahorse),
                base_name = basename(xf$filepath_seahorse),
                full_path = xf$filepath_seahorse
            )),
            date_run = xf$assayinfo$date_run,
            date_processed = Sys.time(),
            assay_info = list(tibble::tibble(xf$assayinfo)),
            rate_data = list(tibble::tibble(xf_rate_pr)),
            injection_info = list(tibble::tibble(xf$inj))
        ) %>%
        dplyr::select(plate_id, filepath_seahorse,
            date_run, date_processed, assay_info,
            injection_info,
            raw_data = data, rate_data
        )

    cli::cli_alert_info("Finished preprocessing of the input data")

    return(xf_plate_pr)
}

#' Preprocess xf raw
#'
#' @description
#' This function uses seahorse excel data that was read using functions 
#' from read_xfplate.
#' The function will preprocess and merge data into the xf_raw data tibble, 
#' to produce
#' the xf_raw_pr tibble by:
#'   * changing columns names
#'   * adding new time columns
#'   * adding injection info columns
#'   * add plate_id column
#'   * adding flagged, norm and bufferfactor
#'   * calculating background data
#'   * calculating raw pH emission data
#'
#' @param xf_raw List (tibble) that contains 'Raw' Seahorse information.
#' @param xf_pHcal List (tibble) that contains well and the corresponding 
#' pH calibration emission info.
#' @param xf_inj A list (tibble) that contains injection information.
#' @param xf_assayinfo List (tibble) with assay information.
#' @param xf_buffer List (tibble) that contains well and bufferfactor.
#' @param xf_norm List consisting [1] well names and the corresponding 
#' normalization values and
#' [2] check if normalization data is available (TRUE/FALSE).
#' @param xf_flagged Vector that contains wells that were "unselected" (flagged).
#'
#' @note The returned preprocessed "Raw" tibble doesn't only contain 
#' data of the "Raw" sheet,
#' but for example also data from the "Assay Configuration sheet".
#'
#' @return A preprocessed "Raw" data list (tibble). With the following columns:
#'
#' plate_id, well, measurement, tick, timescale, minutes, group, interval, 
#' injection,
#' O2_em_corr, pH_em_corr, O2_mmHg, pH, pH_em_corr_corr, O2_em_corr_bkg,
#' pH_em_corr_bkg, O2_mmHg_bkg, pH_bkgd, pH_em_corr_corr_bkg, bufferfactor,
#' cell_n, flagged_well
#'
#' @noRd
#' @keywords internal
#'

preprocess_xf_raw <- function(xf_raw,
                              xf_pHcal,
                              xf_inj,
                              xf_assayinfo,
                              xf_buffer,
                              xf_norm,
                              xf_flagged) {
    # convert time column
    xf_raw_pr <- convert_timestamp(xf_raw)

    # add flag well columnn
    xf_flagged <- xf_flagged %>% pull(well)
    xf_raw_pr$flagged_well <- FALSE
    xf_raw_pr$flagged_well[xf_raw_pr$well %in% xf_flagged] <- TRUE

    # correct pH_em_corr
    xf_raw_pr$pH_em_corr_corr <-
        correct_pH_em_corr(
            xf_raw_pr$pH_em_corr,
            xf_pHcal$pH_cal_em,
            xf_assayinfo$pH_targetEmission[1]
        )


    # calculate backgrounds and join
    background <- calc_background(xf_raw_pr)
    xf_raw_pr <- xf_raw_pr %>%
        dplyr::left_join(background, by = c("tick"))

    # add injection info
    xf_raw_pr <- xf_raw_pr %>%
        dplyr::left_join(xf_inj, by = "measurement")

    # add plate_id to df
    xf_raw_pr$plate_id <- xf_assayinfo$plate_id

    # add norm_info
    xf_raw_pr <- xf_raw_pr %>%
        dplyr::left_join(xf_norm, by = c("well"))

    # add bufferfactor
    xf_raw_pr <- xf_raw_pr %>%
        dplyr::left_join(xf_buffer, by = c("well"))



    # select columns that are needed
    xf_raw_pr <- xf_raw_pr %>%
        dplyr::select(
            plate_id, well, measurement, tick,
            timescale, minutes, group, interval, injection,
            O2_em_corr, pH_em_corr, O2_mmHg, pH, pH_em_corr_corr,
            O2_em_corr_bkg, pH_em_corr_bkg, O2_mmHg_bkg, pH_bkgd,
            pH_em_corr_corr_bkg, bufferfactor, cell_n, flagged_well
        )

    return(xf_raw_pr)
}



#' Preprocess xf_rate
#'
#' @description This function edits the xf_rate tibble to create a preprocessed 
#' tibble.
#' The rate df contains OCR and ECAR information from the seahorse excel file.
#'
#' @note The final preprocessed tibble contains data from both the seahorse 
#' "Rate" sheet and "Assay Configuration" sheet.
#'
#' @param xf_rate Tibble that contains original rate data tibble
#' @param xf_norm Tibble consisting of well names and the corresponding 
#' normalization values
#' @param xf_inj Tibble of injection info
#' @param xf_flagged Tibble that contains wells that were "unselected" 
#' (flagged).
#'
#' @return Preprocessed Rate tibble
#'
#' @noRd
#' @keywords internal
#'

preprocess_xf_rate <- function(xf_rate,
                               xf_norm,
                               xf_inj,
                               xf_flagged) {
    # add norm_info to rate data
    OCR_from_excel <- xf_rate %>%
        dplyr::left_join(xf_norm, by = c("well"))

    OCR_from_excel <- OCR_from_excel %>%
        dplyr::left_join(xf_inj, by = c("measurement"))

    OCR_from_excel$flagged_well <- FALSE
    OCR_from_excel$flagged_well[OCR_from_excel$well %in% xf_flagged] <- TRUE

    return(OCR_from_excel)
}

# DEPENDENT FUNCTIONS ---------------------------------------------------------------


## convert_timestamp() -----------------------------------------------------

#' Convert timestamp
#'
#' @description Convert the the time column in the WAVE input dataframe 
#' to a time scale in seconds.
#'
#' @note This function is called at the preprocess script.
#'
#' @param xf_raw_pr A list (tibble dataframe) for preprocessing.
#' @return A new dataframe with new columns added  to xf_raw_pr. New columns
#'  are: "totalMinutes", "minutes", "timescale".
#'
#' @noRd
#' @keywords internal
#'

convert_timestamp <- function(xf_raw_pr) {
    xf_raw_pr %>%
        mutate(timestamp = as.character(timestamp)) %>%
        mutate(
            times =
                stringr::str_split(timestamp, ":")
        ) %>%
        mutate(
            totalMinutes =
                purrr::map_dbl(times, ~ .x %>%
                    as.numeric() %>%
                    {
                        first(.) * 60 + nth(., 2) + nth(., 3) / 60
                    })
        ) %>%
        mutate(minutes = totalMinutes - first(totalMinutes)) %>%
        mutate(timescale = round(minutes * 60)) %>%
        select(-times)
}



## correct_pH_em_corr() ------------------------------------------------------
#' Corrected pH emission
#'
#' @description Calculate the pH emission corrected for calibration not reaching 
#' target emission.
#'
#' @note This function is called at the preprocess script.
#'
#' @param pH_em_corr pH corrected emission derived from the seahorse "Raw" sheet.
#' @param pH_cal_em pH emission derived from the seahorse "Calibration" sheet.
#' @param pH_targetEmission pH target emission derived from the seahorse 
#' "Calibration" sheet.
#'
#' @return a vector with corrected pH_em_corr 'pH_em_corr_corr'
#' @noRd
#' @keywords internal

correct_pH_em_corr <- function(pH_em_corr, pH_cal_em, pH_targetEmission) {
    correct_pH_em_corr <- (pH_targetEmission / pH_cal_em) * pH_em_corr
}

## calc_background() -------------------------------------------------------

#' Calculate backgrounds
#'
#' @description Calculates all pH and O2 background gorup means of all wells
#' assigned to the 'Background' group
#'
#' @param xf_raw_pr A list (tibble dataframe) for preprocessing.
#'
#' @return A new dataframe  'background' with for each measurement the mean 
#' background for:
#' O2_em_corr, pH_em_corr, O2_mmHg, pH and pH_em_corr_corr
#'
#' @noRd
#' @keywords internal
#'
calc_background <- function(xf_raw_pr) {
    background <- xf_raw_pr %>%
        filter(flagged_well == FALSE) %>%
        dplyr::select(
            group, well, tick, O2_em_corr,
            pH_em_corr, O2_mmHg, pH, pH_em_corr_corr
        ) %>%
        dplyr::filter(group == "Background") %>%
        dplyr::summarize(
            O2_em_corr_bkg = mean(O2_em_corr),
            pH_em_corr_bkg = mean(pH_em_corr),
            O2_mmHg_bkg = mean(O2_mmHg),
            pH_bkgd = mean(pH),
            pH_em_corr_corr_bkg = mean(pH_em_corr_corr),
            .by = c(tick)
        )

    return(background)
}

# END ---------------------------------------------------------------------
