# MAIN FUNCTION preprocess_xfplate ------------------------------------
#' Preprocessing the seahorse plate data that was read in read_xfplate.r
#'
#' @description
#' This function performs the preprocessing of the xf_raw and the xf_rate tibbles
#' and generates the final output nested tibble
#'
#' @param xf list with all necessary seahorse data.
#'
#' @return xf_plate_pr is returned this is a nested tibble with the following 7 columns:
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
#' @examples
#' preprocess_xfplate(xf_donor_A)

preprocess_xfplate <- function(xf){
  
  logger::log_info("Preprocessing collected seahorse information.")

  # Use our xf list with all the necessary Seahorse data to fill this data tibble.
  
  logger::log_info("Edit created Raw dataframe.")
  xf_raw_pr <- preprocess_xf_raw(xf$raw,
                                 xf$pHcal,
                                 xf$inj,
                                 xf$assayinfo,
                                 xf$buffer,
                                 xf$norm,
                                 xf$flagged)

  logger::log_info("Edit created Rate dataframe.")
  xf_rate_pr <- preprocess_xf_rate(xf$rate,
                                   xf$norm,
                                   xf$flagged)

  logger::log_info("Creating new plate dataset with collected and preprocessed information.")
  xf_plate_pr <- xf_raw_pr %>%
    dplyr::group_by(plate_id) %>%
    tidyr::nest() %>%
    dplyr::mutate(filepath_seahorse = list(tibble::tibble(
                  directory_path = dirname(as.character(xf$filepath_seahorse)),
                  base_name = basename(as.character(xf$filepath_seahorse)),
                  full_path = xf$filepath_seahorse
                )),
                  date = xf$assayinfo$date_run,
                  assay_info = list(tibble::tibble(xf$assayinfo)),
                  rate_data = list(tibble::tibble(xf_rate_pr)),
                  injection_info = list(tibble::tibble(xf$inj))) %>%
    dplyr::select(plate_id, filepath_seahorse, date, assay_info, injection_info,
                  raw_data = data, rate_data)
  
  logger::log_info(glue::glue("Finished preprocessing collected seahorse information."))

  return(xf_plate_pr)
}


#' Preprocess xf raw
#'
#' @description
#' This function uses seahorse excel data that was read using functions from read_xfplate.
#' The function will preprocess and merge data into the xf_raw data tibble, to produce
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
#' @param xf_pHcal List (tibble) that contains well and the corresponding pH calibration emission info.
#' @param xf_inj A list (tibble) that contains injection information.
#' @param xf_assayinfo List (tibble) with assay information.
#' @param xf_buffer List (tibble) that contains well and bufferfactor.
#' @param xf_norm List consisting [1] well names and the corresponding normalization values and
#' [2] check if normalization data is available (TRUE/FALSE).
#' @param xf_flagged Vector that contains wells that were "unselected" (flagged).
#'
#' @note The returned preprocessed "Raw" tibble doesn't only contain data of the "Raw" sheet,
#' but for example also data from the "Assay Configuration sheet".
#'
#' @return A preprocessed "Raw" data list (tibble). With the following columns:
#'
#' plate_id, well, measurement, tick, timescale, minutes, group, interval, injection,
#' O2_em_corr, pH_em_corr, O2_mmHg, pH, pH_em_corr_corr, O2_em_corr_bkg,
#' pH_em_corr_bkg, O2_mmHg_bkg, pH_bkgd, pH_em_corr_corr_bkg, bufferfactor,
#' cell_n, flagged_well
#'
#' @noRd
#' @keywords internal
#'
#' @examples
#' preprocess_xf_raw(xf_donor_A$raw, xf_donor_A$pHcal, xf_donor_A$inj, xf_donor_A$assayinfo, xf_donor_A$buffer, xf_donor_A$norm, xf_donor_A$flagged)
#' preprocess_xf_raw(xf_donor_B$raw, xf_donor_B$pHcal, xf_donor_B$inj, xf_donor_B$assayinfo, xf_donor_B$buffer, xf_donor_B$norm, xf_donor_B$flagged)
#' preprocess_xf_raw(xf_donor_C$raw, xf_donor_C$pHcal, xf_donor_C$inj, xf_donor_C$assayinfo, xf_donor_C$buffer, xf_donor_C$norm, xf_donor_C$flagged)

preprocess_xf_raw <- function(xf_raw,
                              xf_pHcal,
                              xf_inj,
                              xf_assayinfo,
                              xf_buffer,
                              xf_norm,
                              xf_flagged) {

  logger::log_info("Preprocessing: Convert the original integer column to integers again, instead of double.")
  # convert the original integer column to integers again, instead of double
  xf_raw_pr <- xf_raw %>%
    tibble::as_tibble() %>%
    dplyr::mutate(across(c(Measurement,
                           Tick,
                           `O2 Light Emission`,
                           `O2 Dark Emission`,
                           `O2 Ref Light`,
                           `O2 Ref Dark`,
                           `pH Light`,
                           `pH Dark`,
                           `pH Ref Light`,
                           `pH Ref Dark`),
                         as.integer))

  logger::log_info("Preprocessing data: Rename columns")
  # rename columns
  xf_raw_pr <- rename_columns(xf_raw_pr)

  # convert time column
  logger::log_info("Preprocessing Raw sheet: Convert time column")
  xf_raw_pr <- convert_timestamp(xf_raw_pr)

  # correct pH_em_corr
  logger::log_info("Preprocessing Raw sheet: Coorrect pH emission")
  xf_raw_pr$pH_em_corr_corr <- correct_pH_em_corr(xf_raw_pr$pH_em_corr,
                                                  xf_pHcal$pH_cal_em,
                                                  xf_assayinfo$pH_targetEmission[1])


  # calculate backgrounds and join
  logger::log_info("Preprocessing Raw sheet: Calculate backgrounds")
  background <- calc_background(xf_raw_pr)

  xf_raw_pr <- xf_raw_pr %>%
    dplyr::left_join(background, by = c("measurement"), relationship = "many-to-many")

  # add injection info
  logger::log_info("Preprocessing Raw sheet: Add injection info")
  xf_raw_pr <- dplyr::left_join(xf_raw_pr, xf_inj, by = "measurement")

  #add plate_id to df
  logger::log_info("Preprocessing Raw sheet: Add plate id")
  xf_raw_pr$plate_id <- xf_assayinfo$plate_id

  #add norm_info
  logger::log_info("Preprocessing Raw sheet: Normalisation info")
  xf_raw_pr <- xf_raw_pr %>% dplyr::left_join(xf_norm, by = c("well"))

  #add bufferfactor
  logger::log_info("Preprocessing Raw sheet: Add bufferfactor")
  xf_raw_pr <- xf_raw_pr %>% dplyr::left_join(xf_buffer, by = c("well"))

  #add flag well columnn
  logger::log_info("Preprocessing Raw sheet: Add flag well column")
  xf_raw_pr$flagged_well <- FALSE
  xf_raw_pr$flagged_well[xf_raw_pr$well %in% xf_flagged] <- TRUE

  # select columns that are needed
  logger::log_info("Preprocessing: Select columns that are needed")
  xf_raw_pr <- xf_raw_pr %>% dplyr::select(
    plate_id, well, measurement, tick, timescale, minutes, group, interval, injection,
    O2_em_corr, pH_em_corr, O2_mmHg, pH, pH_em_corr_corr, O2_em_corr_bkg,
    pH_em_corr_bkg, O2_mmHg_bkg, pH_bkgd, pH_em_corr_corr_bkg, bufferfactor, cell_n, flagged_well
  )

  return(xf_raw_pr)
}


#' Preprocess xf_rate
#'
#' @description This function edits the xf_rate tibble to create a preprocessed tibble.
#' The rate df contains OCR and ECAR information from the seahorse excel file.
#'
#' @note The final preprocessed tibble contains data from both the seahorse "Rate" sheet and "Assay Configuration" sheet.
#'
#' @param xf_rate List that contains [1] original rate data tibble and [2] background correction info (if correction was performed).
#' @param xf_norm List consisting [1] well names and the corresponding normalization values and
#' [2] check if normalization data is available (TRUE/FALSE).
#' @param xf_flagged Vector that contains wells that were "unselected" (flagged).
#'
#' @return Preprocessed Rate tibble
#'
#' @noRd
#' @keywords internal
#'
#' @examples
#' preprocess_xf_rate(xf_donor_A$rate, xf_donor_A$norm, xf_donor_A$flagged)
#' preprocess_xf_rate(xf_donor_B$rate, xf_donor_B$norm, xf_donor_B$flagged)
#' preprocess_xf_rate(xf_donor_C$rate, xf_donor_C$norm, xf_donor_C$flagged)
preprocess_xf_rate <- function(xf_rate,
                               xf_norm,
                               xf_flagged){
  #add norm_info to rate data
  logger::log_info("Add normalisation information to xf Rate datasframe.")
  OCR_from_excel <- xf_rate %>% dplyr::left_join(xf_norm, by = c("well"))

  logger::log_info("Add flagged wells to xf Rate dataframe.")
  OCR_from_excel$flagged_well <- FALSE
  OCR_from_excel$flagged_well[OCR_from_excel$well %in% xf_flagged] <- TRUE

  return(OCR_from_excel)
}

# DEPENDENT FUNCTIONS ---------------------------------------------------------------

## rename_columns() --------------------------------------------------------

#' Rename the columns of the Raw data sheet.
#'
#' @description
#' This function renames the columns of the xf_raw dataframe that was
#' read using read_excel from the WAVE excel input file.
#'
#' @note This function is called at the preprocess script.
#'
#' @param xf_raw_pr A list (tibble dataframe) for preprocessing.
#'
#' @return A dataframe with renamed column names.
#'
#' @noRd
#' @keywords internal
#'
#' @examples
#' rename_columns(xf_donor_A$raw)

rename_columns <- function(xf_raw_pr) {

  # change column names into terms without spaces
  colnames(xf_raw_pr) <- c(
    "measurement", "tick", "well", "group",
    "time", "temp_well", "temp_env", "O2_isvalid", "O2_mmHg",
    "O2_light", "O2_dark", "O2ref_light", "O2ref_dark",
    "O2_em_corr", "pH_isvalid", "pH", "pH_light", "pH_dark",
    "pHref_light",
    "pHref_dark", "pH_em_corr", "interval"
  )

  return(xf_raw_pr)
}

## select_columns() -------------------------------------------------------
#'
#' Select columns of the xf_raw dataframe used for further analysis.
#'
#' @param xf_raw_pr  A list (tibble dataframe) for preprocessing.
#'
#' @return Selected columns. The selected columns with new names are:
#'   ("measurement","tick", "well", "group", "time", "O2_em_corr","pH_em_corr", "O2_mmHg", "pH")
#'
#' @noRd
#' @keywords internal
#'
#'

select_columns <- function(xf_raw_pr){
  xf_raw_pr <- xf_raw_pr %>%
    dplyr::select(
      "measurement", "tick", "well",
      "group", "time", "O2_em_corr",
      "pH_em_corr", "O2_mmHg", "pH"
    )

  return(xf_raw_pr)
  }


## convert_timestamp() -----------------------------------------------------

#' Convert timestamp
#'
#' @description Convert the the time column in the WAVE input dataframe to a time scale in seconds.
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

  # first make sure that the data is sorted correctly
  xf_raw_pr <- dplyr::arrange(xf_raw_pr, tick, well)

  # add three columns to df (totalMinutes, minutes and time) by converting the timestamp into seconds
  xf_raw_pr$time <- as.character((xf_raw_pr$time))
  times <- strsplit(xf_raw_pr$time, ":")
  xf_raw_pr$totalMinutes <- sapply(times, function(x) {
    x <- as.numeric(x)
    x[1] * 60 + x[2] + x[3] / 60
  })
  xf_raw_pr$minutes <- xf_raw_pr$totalMinutes - xf_raw_pr$totalMinutes[1] # first row needs to be first timepoint!
  xf_raw_pr$timescale <- round(xf_raw_pr$minutes * 60)

  return(xf_raw_pr)
}


## correct_pH_em_corr() ------------------------------------------------------
#' Corrected pH emission
#'
#' @description Calculate the pH emission corrected for calibration not reaching target emission.
#'
#' @note This function is called at the preprocess script.
#'
#' @param pH_em_corr pH corrected emission derived from the seahorse "Raw" sheet.
#' @param pH_cal_em pH emission derived from the seahorse "Calibration" sheet.
#' @param pH_targetEmission pH target emission derived from the seahorse "Calibration" sheet.
#'
#' @return a vector with corrected pH_em_corr 'pH_em_corr_corr'
#' @noRd
#' @keywords internal

correct_pH_em_corr <- function(pH_em_corr, pH_cal_em, pH_targetEmission){

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
#' @return A new dataframe  'background' with for each measurement the mean background for:
#' O2_em_corr, pH_em_corr, O2_mmHg, pH and pH_em_corr_corr
#'
#' @noRd
#' @keywords internal
#'
calc_background <- function(xf_raw_pr){

  background <- xf_raw_pr %>%
    dplyr::select(group, well, measurement, timescale, O2_em_corr,
           pH_em_corr, O2_mmHg, pH, pH_em_corr_corr) %>%
    dplyr::filter(group == "Background") %>%
    dplyr::reframe(
      measurement,
      O2_em_corr_bkg = mean(O2_em_corr),
      pH_em_corr_bkg = mean(pH_em_corr),
      O2_mmHg_bkg = mean(O2_mmHg),
      pH_bkgd = mean(pH),
      pH_em_corr_corr_bkg = mean(pH_em_corr_corr)
    )

  return(background)
}

# END ---------------------------------------------------------------------
