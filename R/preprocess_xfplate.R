# preprocess_xfplate
# vincent de boer
# september 24th, 2022
# derived from first funs_preprocess_plate_data_new.R function

library(readxl)

# MAIN FUNCTION preprocess_xfplate ------------------------------------

#' Preprocessing the plate data that was read in funs_read_plate_data.R
#'
#' @description
#' The preprocess_plate_data function preprocesses the data by:
#'   * changing columns names
#'   * adding new time columns
#'   * adding injection info columns
#'   * add plate_id column
#'   * calculating background data
#'   * calculating raw pH emission data
#'
#' @param plate_df A list of all data read from the original excel wace exported file
#' @return a list with the new dataframe and the assay_info. A new dataframe called XFe96data is returned.
#' The preprocessed dataframe has the following columns:
#'   * plate_id
#'   * well
#'   * measurement
#'   * tick
#'   * timescale
#'   * minutes
#'   * group
#'   * interval
#'   * injection
#'   * O2_em_corr
#'   * pH_em_corr
#'   * O2_mmHg
#'   * pH
#'   * pH_em_corr_corr
#'   * backgrounds for O2_em_corr, pH_em_corr, O2_mmHg, pH, pH_em_corr_corr
#'
#' The assay_info has the following information:
#'   * ....
#'
#' @examples
#' preprocess_plate_data(plate_df)
#'

preprocess_xf_raw <- function(xf_raw,
                              xf_pHcal,
                              xf_inj,
                              xf_assayinfo,
                              xf_buffer,
                              xf_norm,
                              xf_flagged) {

  # Read_excel guesses numeric types as double. Not all data is of type double,
  # some data has to be converted to integer.
  xf_raw_pr <- xf_raw %>%
    as_tibble() %>%
    mutate(across(c(Measurement,
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

  # rename columns
  xf_raw_pr <- rename_columns(xf_raw_pr)

  # convert time column
  xf_raw_pr <- convert_timestamp(xf_raw_pr)

  # correct pH_em_corr
  xf_raw_pr$pH_em_corr_corr <-correct_pH_em_corr(xf_raw_pr$pH_em_corr,
                                                 xf_pHcal$pH_cal_em,
                                                 xf_assayinfo$pH_targetEmission[1])

  # calculate backgrounds and join
  background <- calc_background(xf_raw_pr)
  xf_raw_pr <- xf_raw_pr %>% left_join(background, by = c("measurement", "timescale"))

  # add injection info
  xf_raw_pr <- left_join(xf_raw_pr, xf_inj, by = "measurement")

  #add plate_id to df
  xf_raw_pr$plate_id <- xf_assayinfo$plate_id

  #add norm_info
  xf_raw_pr <- xf_raw_pr %>% left_join(xf_norm, by = c("well"))

  #add bufferfactor
  xf_raw_pr <- xf_raw_pr %>% left_join(xf_buffer, by = c("well"))

  #add flag well columnn
  xf_raw_pr$flagged_well <- FALSE
  xf_raw_pr$flagged_well[xf_raw_pr$well %in% xf_flagged] <- TRUE

  # select columns that are needed
  xf_raw_pr <- xf_raw_pr %>% select(
    plate_id, well, measurement, tick, timescale, minutes, group, interval, injection,
    O2_em_corr, pH_em_corr, O2_mmHg, pH, pH_em_corr_corr, O2_em_corr_bkg,
    pH_em_corr_bkg, O2_mmHg_bkg, pH_bkgd, pH_em_corr_corr_bkg, bufferfactor, cell_n, flagged_well
  )

  return(xf_raw_pr)
}


preprocess_xf_rate <- function(xf_rate,
                               xf_norm,
                               xf_flagged){
  #add norm_info to rate data
  OCR_from_excel <- xf_rate %>% left_join(xf_norm, by = c("well"))

  #add flagged wells to xf rate
  OCR_from_excel$flagged_well <- FALSE
  OCR_from_excel$flagged_well[OCR_from_excel$well %in% xf_flagged] <- TRUE

  return(OCR_from_excel)
}

preprocess_xfplate <- function(xf){

  xf_raw_pr <- preprocess_xf_raw(xf$raw,
                                 xf$pHcal,
                                 xf$inj,
                                 xf$assayinfo,
                                 xf$buffer,
                                 xf$norm,
                                 xf$flagged)

  xf_rate_pr <- preprocess_xf_rate(xf$rate,
                                   xf$norm,
                                   xf$flagged)

  #output all plate data
  data_tibble <- xf_raw_pr %>%
    group_by(plate_id) %>%
    nest() %>%
    mutate(filepath_seahorse = xf$filepath_seahorse,
           date = xf$assayinfo$date_run,
           assayinfo = list(tibble(xf$assayinfo)),
           rate_data = list(tibble(xf_rate_pr)),
           injection_info = list(tibble(xf$inj))) %>%
    select(plate_id, filepath_seahorse, date, assay_info, injection_info,
           raw_data = data, rate_data)

  return(data_tibble)
}

# DEPENDENT FUNCTIONS ---------------------------------------------------------------

## rename_columns() --------------------------------------------------------

#' Rename the columns of the Raw data sheet
#'
#' @description
#' This function is called in the preprocessing function and renames the columns of the dataframe that was
#'   read in by read_excel from the WAVE excel output file, excel sheet "Raw".
#'
#' @param plate_df A dataframe that was read by read_excel from the WAVE excel output file (sheet "Raw")
#' @return A new dataframe with adjusted and selected column names. The selected columns with new names are:
#'   ("measurement","tick", "well", "group", "time", "O2_em_corr","pH_em_corr", "O2_mmHg", "pH")
#' @examples
#' rename_columns(XFe96data)
rename_columns <- function(plate_df) {

  # change column names into terms without spaces
  colnames(plate_df) <- c(
    "measurement", "tick", "well", "group",
    "time", "temp_well", "temp_env", "O2_isvalid", "O2_mmHg",
    "O2_light", "O2_dark", "O2ref_light", "O2ref_dark",
    "O2_em_corr", "pH_isvalid", "pH", "pH_light", "pH_dark",
    "pHref_light",
    "pHref_dark", "pH_em_corr"
  )

  plate_df <- plate_df %>%
    select(
      "measurement", "tick", "well",
      "group", "time", "O2_em_corr",
      "pH_em_corr", "O2_mmHg", "pH"
    )


  return(plate_df)
} # called in preprocessing


## convert_timestamp() -----------------------------------------------------

#' Convert the the time column in the WAVE input dataframe to a time scale in seconds
#'
#' @param plate_df A dataframe generated in adjust_columns
#' @return A new dataframe with new columns added  to \code{plate_df}. New columns
#'  are: "totalMinutes", "minutes", "timescale"
#' @examples
#' convert_timestamp(XFe96data)
convert_timestamp <- function(plate_df) {

  # first make sure that the data is sorted correctly
  plate_df <- dplyr::arrange(plate_df, tick, well)

  # add three columns to df (totalMinutes, minutes and time) by converting the timestamp into seconds
  plate_df$time <- as.character((plate_df$time))
  times <- strsplit(plate_df$time, ":")
  plate_df$totalMinutes <- sapply(times, function(x) {
    x <- as.numeric(x)
    x[1] * 60 + x[2] + x[3] / 60
  })
  plate_df$minutes <- plate_df$totalMinutes - plate_df$totalMinutes[1] # first row needs to be first timepoint!
  plate_df$timescale <- round(plate_df$minutes * 60)

  return(plate_df)
} #called in preprocessing


## correct_pH_em_corr() ------------------------------------------------------

correct_pH_em_corr <- function(pH_em_corr, pH_cal_em, pH_targetEmission){

  correct_pH_em_corr <- (pH_targetEmission / pH_cal_em) * pH_em_corr

}

## calc_background() -------------------------------------------------------

calc_background <- function(plate_df){
  background <- XFe96data %>%
    select(group, well, measurement, timescale, O2_em_corr,
           pH_em_corr, O2_mmHg, pH, pH_em_corr_corr) %>%
    filter(group == "Background") %>%
    group_by(measurement, timescale) %>%
    summarize(
      O2_em_corr_bkg = mean(O2_em_corr),
      pH_em_corr_bkg = mean(pH_em_corr),
      O2_mmHg_bkg = mean(O2_mmHg),
      pH_bkgd = mean(pH),
      pH_em_corr_corr_bkg = mean(pH_em_corr_corr)
    )
  return(background)
}

# END ---------------------------------------------------------------------


