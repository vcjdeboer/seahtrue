# read_xfplate() -------------------------------------------------------
#' Read necessary Seahorse plate data from Seahorse Excel file.
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.
#'
#' @return xf list with all necessary Seahorse data.
#'
#' @noRd
#' @keywords internal
#'
#' @examples
#' read_xfplate(system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue"))
#' read_xfplate(system.file("extdata", "20200110_SciRep_PBMCs_donor_B.xlsx", package = "seahtrue"))
#' read_xfplate(system.file("extdata", "20200110_SciRep_PBMCs_donor_C.xlsx", package = "seahtrue"))
read_xfplate <- function(filepath_seahorse) {

    rlang::check_required(filepath_seahorse)

    cli::cli_alert(glue::glue("Start function to read seahorse plate data from Excel file:
                                {filepath_seahorse}"))

    # read data
    xf_raw <- get_xf_raw(filepath_seahorse)
    xf_rate <- get_xf_rate(filepath_seahorse) #outputs list of 2
    xf_norm <- get_xf_norm(filepath_seahorse) #outputs list of 2
    xf_buffer <- get_xf_buffer(filepath_seahorse)
    xf_inj <- get_xf_inj(filepath_seahorse)
    xf_pHcal <- get_xf_pHcal(filepath_seahorse)
    xf_O2cal <- get_xf_O2cal(filepath_seahorse)
    xf_flagged <- get_xf_flagged(filepath_seahorse)
    xf_assayinfo <- get_xf_assayinfo(filepath_seahorse,
                                     norm_available = xf_norm[[2]],
                                     xls_ocr_backgroundcorrected = xf_rate[[2]])
    xf_norm <- xf_norm[[1]]
    xf_rate <- xf_rate[[1]]

    # make the output list
    xf <- list(
      raw = xf_raw,
      rate = xf_rate,
      assayinfo = xf_assayinfo,
      inj = xf_inj,
      pHcal = xf_pHcal,
      O2cal = xf_O2cal,
      norm = xf_norm,
      flagged = xf_flagged,
      buffer = xf_buffer,
      filepath_seahorse = filepath_seahorse
    )
    
    logger::log_info(glue::glue("Parsing all collected seahorse information from file: {filepath_seahorse}"))

    return(xf)

}

# get_xf_raw() --------------------------------------------------------------
#' Get the data of the Seahorse 'Raw'-sheet
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.
#'
#' @return xf_raw tibble (list) with 'Raw' Seahorse information.
#`
#' @noRd
#' @keywords internal
#' @import dplyr readxl

#'
#' @examples
#' get_xf_raw(system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue"))
#' get_xf_raw(system.file("extdata", "20200110_SciRep_PBMCs_donor_B.xlsx", package = "seahtrue"))
#' get_xf_raw(system.file("extdata", "20200110_SciRep_PBMCs_donor_C.xlsx", package = "seahtrue"))

 get_xf_raw <- function(filepath_seahorse){
    logger::log_info("Collecting data from 'Raw' sheet")

    xf_raw <- readxl::read_excel(filepath_seahorse,
                         sheet = "Raw",
                         col_types = c("numeric", # Measurment
                                       "numeric", # Tick
                                       "text", # Well
                                       "text", # Group
                                       "text", # TimeStamp
                                       "numeric", # Well Temperature
                                       "numeric", # Environment Temperature
                                       "text", # O2 is Valid
                                       "numeric", # O2 (mmHg)
                                       "numeric", # O2 Light Emission
                                       "numeric", # O2 Dark Emission
                                       "numeric", # O2 Ref Light
                                       "numeric", # O2 Ref Dark
                                       "numeric", # O2 Corrected Em.
                                       "text", # pH Is Valid
                                       "numeric", # pH
                                       "numeric", # pH Light
                                       "numeric", # pH Dark
                                       "numeric", # pH Ref Light
                                       "numeric",# pH Ref Dark
                                       "numeric" # pH Corrected Em.
                         ))
   

    logger::log_info("Finished collecting data from 'Raw' sheet.")
    
    return(xf_raw)
    
      }

# get_xf_norm -------------------------------------------------------------

#' Get normalization info from the Assay Configuration sheet.
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.
#'
#'
#' @return List consisting of [1] well names and the corresponding normalization values and
#' [2] check if normalization data is available (TRUE/FALSE).
#' @noRd
#' @keywords internal
#'
#' @examples
#' get_xf_norm(system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue"))
#' get_xf_norm(system.file("extdata", "20200110_SciRep_PBMCs_donor_B.xlsx", package = "seahtrue"))
#' get_xf_norm(system.file("extdata", "20200110_SciRep_PBMCs_donor_C.xlsx", package = "seahtrue"))
get_xf_norm <- function(filepath_seahorse){

    logger::log_info("Collecting normalisation info from 'Assay Configuration' sheet.")

    norm_info <- get_platelayout_data(filepath_seahorse,
                                      my_sheet = "Assay Configuration",
                                      my_range = "B84:N92",
                                      my_param = "cell_n")
    

    if (sum(is.na(norm_info$cell_n)) >90){
      norm_available <- FALSE
    } else {
      norm_available <- TRUE}

    xf_norm <- list(norm_info, norm_available)
    
    logger::log_info("Finished collecting normalisation info from 'Assay configuration' sheet.")

    return(xf_norm)
}

# get_xf_flagged() -----------------------------------------------------

#' Get unselected (flagged) wells from the Assay Configuration sheet.
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.
#'
#' @return Vector that contains wells that were "unselected" (flagged).
#' @noRd
#' @keywords internal
#' @examples
#' get_xf_flagged(system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue"))
#' get_xf_flagged(system.file("extdata", "20200110_SciRep_PBMCs_donor_B.xlsx", package = "seahtrue"))
#' get_xf_flagged(system.file("extdata", "20200110_SciRep_PBMCs_donor_C.xlsx", package = "seahtrue"))
get_xf_flagged <- function(filepath_seahorse){

    logger::log_info("Collecting unselected (flagged) wells from the Assay Configuration sheet.")

    x <<- tidyxl::xlsx_cells(filepath_seahorse, "Assay Configuration")
    formats <- tidyxl::xlsx_formats(filepath_seahorse, "Assay Configuration")

    # subset to only the platelayout with the cells that show the "unselected" wells by user
    subset_x <- x %>% 
      dplyr::filter(row %in% c(12:19)) %>% 
      dplyr::filter(col %in% c(3:14))

    # get the "unselected" (flagged) wells (based on color fill)
    flagged_df <- subset_x[subset_x$local_format_id %in%
                             which(formats$local$fill$patternFill$fgColor$rgb == "FFFFFFFF"),
                           c("address")]

    # optionally alignment format can be used
    # subset_x[subset_x$local_format_id %in%
    #            which(formats$local$alignment$horizontal == "center"),
    #          c("address")]

    # changed the cell address to well names
    new_col_names <- flagged_df %>%
      dplyr::pull(address) %>% substr(1,1) %>%
      stringr::str_c(collapse = "---") %>%
      stringr::str_replace_all(c("C" = "01", "D" = "02", "E" = "03", "F" = "04", "G" = "05", "H" = "06",
                                 "I" = "07", "J" = "08", "K" = "09", "L" = "10", "M" = "11", "N" = "12"))
    new_col_names <-   unlist(stringr::str_split(new_col_names, "---"))

    new_row_names <- flagged_df %>%
      dplyr::pull(address) %>% substr(2,3) %>%
      stringr::str_c(collapse = "---") %>%
      stringr::str_replace_all(c("12" = "A", "13" = "B", "14" = "C", "15" = "D", "16" = "E", "17" = "F",
                                 "18" = "G", "18" = "H"))
    new_row_names <-   unlist(stringr::str_split(new_row_names, "---"))

    # output the wells that were "unselected" (flagged)
    flagged_vector <- paste0(new_row_names, new_col_names)

    flagged_tibble <- tibble::tibble(
      well = flagged_vector,
      flag = TRUE
    )
    
    logger::log_info("Finished collecting unselected (flagged) wells from the Assay Configuration sheet.")

    return(flagged_tibble)
}

# get_originalRateTable() -------------------------------------------------
#' Get the OCR from the excel file
#'
#' @details
#' [2]If rate data was not already corrected, a background subtraction was performed and the second element of this list contains TRUE (logical).
#' [2]If rate data was already corrected, there is no need for background subtraction
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.wesd
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.
#'
#' @return List that contains [1] original rate data tibble and [2] background correction info (if correction was performed).
#'
#' @noRd
#' @keywords internal
#' @import dplyr readxl
#'
#' @examples
#' get_originalRateTable(system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue"))
#' get_originalRateTable(system.file("extdata", "20200110_SciRep_PBMCs_donor_B.xlsx", package = "seahtrue"))
#' get_originalRateTable(system.file("extdata", "20200110_SciRep_PBMCs_donor_C.xlsx", package = "seahtrue"))

get_originalRateTable <- function(filepath_seahorse){

  logger::log_info("Collecting OCR from 'Rate' sheet.")

  original_rate_df <- readxl::read_excel(filepath_seahorse, sheet = "Rate")

  # because rate data can be either background corrected or not this should be checked first
  # first verify whether a "Background" group exists in the  original_rate_df


  if ("Background" %in% {original_rate_df$Group %>% unique()}) {

    logger::log_info("A background group was found in the RATE sheet")


    check_background <- original_rate_df %>%
      dplyr::filter(Group == "Background") %>%
      dplyr::select(OCR) %>%
      dplyr::reframe(mean = mean(OCR)) %>%
      dplyr::pull(mean)

    if (check_background == 0) {
      corrected_allready <- TRUE
    } else {
      corrected_allready <-  FALSE
    }

  } else {

    #in case when there is no Background group we work with the original data
    # that is in the input file "Rate" sheet
    # please note that there will be warning logged, but the columns will be
    # labeled incorrectly as if the data is background corrected

    logger::log_info("WARNING: no background group was found in the 'Rate' sheet")

    corrected_allready <-  TRUE

  }

  if (corrected_allready == TRUE){
    colnames(original_rate_df) <-
      c("measurement","well", "group",
        "time_wave", "OCR_wave_bc",
        "ECAR_wave_bc", "PER_wave_bc")
    original_rate_df <- original_rate_df %>%
      dplyr::mutate(OCR_wave = 0, ECAR_wave = 0)

    original_rate_df <- original_rate_df %>%
      dplyr::select(measurement, well, group,
                    time_wave, OCR_wave, OCR_wave_bc,
                    ECAR_wave, ECAR_wave_bc)

  } else{
    colnames(original_rate_df) <-
      c("measurement","well", "group",
        "time_wave", "OCR_wave",
        "ECAR_wave", "PER_wave")

    #do background substraction forr wave table
    background <- original_rate_df %>%
      dplyr::filter(group=="Background") %>%
      dplyr::summarize(bkg_OCR_wave = mean(OCR_wave),
                       bkg_ECAR_wave = mean(ECAR_wave),
                       .by = measurement)
  
    original_rate_df <- dplyr::left_join(original_rate_df,
                                         background,
                                         by = c("measurement"), copy = TRUE)

    original_rate_df$OCR_wave_bc <- original_rate_df$OCR_wave - original_rate_df$bkg_OCR_wave
    original_rate_df$ECAR_wave_bc <- original_rate_df$ECAR_wave - original_rate_df$bkg_ECAR_wave

    original_rate_df <- original_rate_df %>%
      dplyr::select(measurement, well, group,
                    time_wave, OCR_wave, OCR_wave_bc,
                    ECAR_wave, ECAR_wave_bc)
  }

  original_rate_df_list <- list(original_rate_df, corrected_allready)

  logger::log_info("Finished collecting OCR from 'Rate' sheet.")

  return(original_rate_df_list)

}

# Additional utility functions
# check_excel_positions() -------------------------------------------------------
#' Used as util function in get_xf_assayinfo()
#'
#' @param df The meta_df that was read from Assay Configuration sheet.
#' @param pos_vector  a vector of cell positions that should be checked
#' @param name_vector a vector with the strings that should be at the positions
#' that were given in 'pos_vector'
#'
#' @return either TRUE or FALSE depending on if the check passed or failed
#' @noRd
#' @keywords internal
#'

check_excel_positions <- function(df, pos_vector, name_vector){

  logger::log_info("Check if excel df contains data name on certain position.")
  tf_values <- mapply(function(pos, name) {
    true_false <- name %in% df[[1]][pos]
    return(true_false)
  }, pos_vector, name_vector)

  check_tf_list <- function(tf_values){
    if (all(tf_values)) {
      logger::log_info("'Assay Configuration' sheet contains all values.")
      return(TRUE)
    } else {
      logger::log_error("'Assay Configuration' sheet doesn't contain all values.")
      stop()
    }
  }

  tf <- check_tf_list(tf_values)

  return(tf)
}


# get_xf_rate -------------------------------------------------------------

#' @title Get original rate table
#'
#' @details
#' [2]If rate data was not already corrected a background subtraction was performed and the second element of this list contains TRUE (logical).
#' [2]If rate data was already corrected there is no need for background subtraction.
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.
#'
#' @return List that contains [1] original rate data tibble and [2] background correction info (if correction was performed).
#'
#' @noRd
#' @keywords internal
#
#' @examples
#' get_xf_rate(system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue"))
#' get_xf_rate(system.file("extdata", "20200110_SciRep_PBMCs_donor_B.xlsx", package = "seahtrue"))
#' get_xf_rate(system.file("extdata", "20200110_SciRep_PBMCs_donor_C.xlsx", package = "seahtrue"))

get_xf_rate <- function(filepath_seahorse){
    #first item is table, second item is background_corrected logical
    xf_rate_list <- get_originalRateTable(filepath_seahorse)
    
    logger::log_info("Finished collecting data from 'Rate' sheet.")
    
    return(xf_rate_list)
}

# get_xf_buffer -----------------------------------------------------------

#' Get buffer factor (capacity) info
#'
#' @details buffer factor(BF): Buffer capacity of the measurement system, comprising the assay medium and XF assay
#' conditions (instrument, sensor, labware).
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.

#' @return List (tibble) that contains well and bufferfactor.
#' @noRd
#' @keywords internal
#'
#' @examples
#' get_xf_buffer(system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue"))
#' get_xf_buffer(system.file("extdata", "20200110_SciRep_PBMCs_donor_B.xlsx", package = "seahtrue"))
#' get_xf_buffer(system.file("extdata", "20200110_SciRep_PBMCs_donor_C.xlsx", package = "seahtrue"))
get_xf_buffer <- function(filepath_seahorse){

    logger::log_info("Collecting buffer factor info from 'Assay configuration' sheet.")


    bufferfactor_info <- get_platelayout_data(filepath_seahorse,
                                              my_sheet = "Assay Configuration",
                                              my_range = "B96:N104",
                                              my_param = "bufferfactor")
    
    logger::log_info("Finished collecting bufferfactor information from 'Assay configuration' sheet.")
    

    return(bufferfactor_info)

}

# get_xf_pHcal ------------------------------------------------------------

#' Get the pH calibration emission data.
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.
#'

#' @return List (tibble) that contains well and the corresponding pH calibration emission info.
#'
#' @keywords internal
#' @noRd
#' @keywords internal

#' @examples
#' get_xf_pHcal(system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue"))
#' get_xf_pHcal(system.file("extdata", "20200110_SciRep_PBMCs_donor_B.xlsx", package = "seahtrue"))
#' get_xf_pHcal(system.file("extdata", "20200110_SciRep_PBMCs_donor_C.xlsx", package = "seahtrue"))
get_xf_pHcal <- function(filepath_seahorse){
  logger::log_info("Collecting pH calibration emission data")

  pH_calibration <- get_platelayout_data(filepath_seahorse,
                                         my_sheet = "Calibration",
                                         my_range = "P16:AB24",
                                         my_param = "pH_cal_em")
  
  logger::log_info("Finished collecting pH calibration emission data")
  
  
  return(pH_calibration)
}

# get_xf_O2cal ------------------------------------------------------------

#' Get O2 calibration emission.
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.
#'
#' @return List (tibble) that contains wells and the corresponding O2 calibration emission.
#' @keywords internal
#' @noRd
#' @keywords internal
#' @examples
#' get_xf_O2cal(system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue"))
#' get_xf_O2cal(system.file("extdata", "20200110_SciRep_PBMCs_donor_B.xlsx", package = "seahtrue"))
#' get_xf_O2cal(system.file("extdata", "20200110_SciRep_PBMCs_donor_C.xlsx", package = "seahtrue"))
get_xf_O2cal <- function(filepath_seahorse){

  logger::log_info("Collecting O2 calibration emission")

  O2_calibration <- get_platelayout_data(filepath_seahorse,
                                         my_sheet = "Calibration",
                                         my_range = "B7:N15",
                                         my_param = "O2_cal_em")

  logger::log_info("Finished collecting O2 calibration emission")
  
  
  return(O2_calibration)
}

# get_xf_inj --------------------------------------------------------------
#' Read the injection information from the "Operation log" sheet M-version or H-Version.
#'
#'@description The output of the injection information is different on different
#' XFe96 instruments. We distinguish injection information from our own "HAP" chair
#' group devices (H-version) and "Manual" injection information (M-version). The H-version
#' assumes the names of the injection names are listed in the "operation log file". The M-version
#' uses a manual assignment of the injection names. This function will read the number of
#' measurements per injection.
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.
#' @param injscheme Name of the injection scheme. Can be "HAP" or "manual".
#'
#' @return A list (tibble) that contains injection information.
#'
#' @noRd
#' @keywords internal
#'
#' @examples
#' get_xf_inj(system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue"))
#' get_xf_inj(system.file("extdata", "20200110_SciRep_PBMCs_donor_B.xlsx", package = "seahtrue"))
#' get_xf_inj(system.file("extdata", "20200110_SciRep_PBMCs_donor_C.xlsx", package = "seahtrue"))
get_xf_inj <- function(filepath_seahorse, injscheme = "HAP"){

  logger::log_info("Collecting injection information")

  #command_index in "Operation Log" sheet give numbers to the phases in a seahorse exp
  # each command (eg. "mix", "measure") gets the command_index for that phase
  # 0 = moving operation
  # 1 = calibration
  # 2 = baseline
  # 3 = injection 1
  # 4 = injection 2
  # 5 = injection 3
  # 6 = injection 4

  #read injection strategy and measurements from "Operation Log" sheet
  info_sh<-readxl::read_excel(filepath_seahorse, sheet = "Operation Log")
  colnames(info_sh) <- c("instruction_name","command_name",
                         "command_index","start_time","end_time",
                         "completion_status")

  if (injscheme == "HAP"){
    #assumes injection names are available in operation log file (this is the case for most experiments)
    measurement_info <- dplyr::filter(info_sh, command_name == "Measure")
    measurement_info$interval <- measurement_info$command_index -1
    measurement_info$measurement <- 1:nrow(measurement_info)
    measurement_info <- measurement_info %>% dplyr::select(measurement, interval, injection=instruction_name)
  }

  if (injscheme == "manual"){

    #in case there is no command index in "opertion log"
    command_names <- c("XF - PC_Measure", "XF - PC_Inject")
    measurement_info <- dplyr::filter(info_sh, command_name %in% command_names)

    # "PC - inject" has a number as command_index
    # "PC - measure" command_index == 0
    # I use that to set the command_index
    interval = 1
    for (i in 1:nrow(measurement_info)){
      if(measurement_info$command_index[i] == 0){
        measurement_info$command_index[i] <-  interval } else {
          interval <-  interval +1
          measurement_info$command_index[i] <-  interval}
    }
    colnames(measurement_info)[3] <- "interval"
    measurement_info <- dplyr::filter(measurement_info, command_name == "XF - PC_Measure")
    measurement_info$measurement <- 1:nrow(measurement_info)
    measurement_info <- measurement_info %>% dplyr::select(measurement, interval)

    #gives name of the injection manually
    # case mitostress
    injections <- c("basal", "OM", "FCCP", "AM/rot")
    injections_mitostress <- tibble::tibble(interval = 1:4, injection=c("basal", "OM", "FCCP", "AM/rot"))
    measurement_info <- dplyr::left_join(measurement_info, injections_mitostress, by = c("interval"))

    ## case glycostress
    #injections <- c("basal", "glucose", "OM", "2DG")
    #injections_glycostress <- tibble(interval = 1:4, injection=injections)
    #measurement_info <- left_join(measurement_info, injections_glycostress, by = c("interval"))
  }

  logger::log_info("Finished collecting injection information")
  
  logger::log_info("Finished collecting injection information  from 'Assay configuration' sheet.")
  
  return(measurement_info)

}


# get_xf_assayinfo ----------------------------------------------------
#' Get assay information.
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.
#' @param date_style The format of the date, can be "US" or "NL".
#' @param instrument The type of seahorse analyzer. Can be "XFe96" or XFHSmini".
#' @param norm_available requires xf_norm. Can be TRUE or FALSE.
#' @param xls_ocr_backgroundcorrected requires original rate table. Can be TRUE or FALSE.
#' @noRd
#' @keywords internal
#' @return List (tibble) with assay information.
#'
#' @examples
#' get_xf_assayinfo(system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue"), norm_available = "TRUE", xls_ocr_backgroundcorrected = "TRUE", date_style = "NL")
#' get_xf_assayinfo(system.file("extdata", "20200110_SciRep_PBMCs_donor_B.xlsx", package = "seahtrue"), norm_available = "TRUE", xls_ocr_backgroundcorrected = "TRUE", date_style = "US")
#' get_xf_assayinfo(system.file("extdata", "20200110_SciRep_PBMCs_donor_C.xlsx", package = "seahtrue"), norm_available = "TRUE", xls_ocr_backgroundcorrected = "TRUE", date_style = "NL")
get_xf_assayinfo <- function(filepath_seahorse,
                             date_style = "empty",
                             instrument = "XFe96",
                             norm_available,
                             xls_ocr_backgroundcorrected) {

  logger::log_info("Collecting assay information")

  if (instrument == "XFHSmini"){
    gain1_cell <-  "D68"
    gain2_cell <-  "E68"
  }

  if (instrument == "XFe96"){
    gain1_cell <-  "D70"
    gain2_cell <-  "E70"
  }

  # read Assay Configuration sheet
    meta_df <- readxl::read_excel(filepath_seahorse,
                          sheet = "Assay Configuration",
                          col_names = c("parameter", "value"),
                          range = "A1:B83"
    )


  pos_vector = c(4, 26, 32, 38, 58, 59, 60, 61, 62, 63, 65, 66, 67, 76)
  name_vector = c("Assay Name", "Cartridge Barcode", "Plate Barcode", "Instrument Serial",
                  "ksv", "Ksv Temp Correction", "Corrected Ksv", "Calculated FO",
                  "Pseudo Volume", "TAC", "TW", "TC", "TP", "Calibration pH")

  # Assertion to check if meta_df contains Seahorse constants on the right locations.
  tf <- check_excel_positions(meta_df, pos_vector, name_vector)

  meta_df <- meta_df[!is.na(meta_df$parameter), ]

  # read Assay Configuration sheet gain1
    gain1 <- readxl::read_excel(filepath_seahorse,
                        sheet = "Assay Configuration",
                        col_names = c("value"),
                        range = gain1_cell
    )

  # read Assay Configuration sheet gain2
    gain2 <- readxl::read_excel(filepath_seahorse,
                        sheet = "Assay Configuration",
                        col_names = c("value"),
                        range = gain2_cell
    )
  

  # read target emission cells
  O2_target_emission <- readxl::read_excel(filepath_seahorse,
                                   sheet = "Calibration",
                                   col_names = FALSE,
                                   range = "B4"
  )
  
  # read pH target emission cells
    pH_target_emission <- readxl::read_excel(filepath_seahorse,
                                     sheet = "Calibration",
                                     col_names = FALSE,
                                     range = "P4"
                                     
                                     
    )

  F0 <- as.double(meta_df$value[meta_df$parameter == "Calculated FO"])
  V_C <- as.double(meta_df$value[meta_df$parameter == "Pseudo Volume"])
  Tau_AC <- as.double(meta_df$value[meta_df$parameter == "TAC"])
  Tau_W <- as.double(meta_df$value[meta_df$parameter == "TW"])
  Tau_C <- as.double(meta_df$value[meta_df$parameter == "TC"])
  Tau_P <- as.double(meta_df$value[meta_df$parameter == "TP"])
  KSV_original <- as.double(meta_df$value[meta_df$parameter == "ksv"])
  KSV_corrected <- as.double(meta_df$value[meta_df$parameter == "Corrected Ksv"])
  KSV_tempCorrection <- as.logical(meta_df$value[meta_df$parameter == "Ksv Temp Correction"])
  KSV <- KSV_corrected

  pH_0 <- as.double(meta_df$value[meta_df$parameter == "Calibration pH"])
  pH_plateVolume <- as.double(meta_df$value[meta_df$parameter == "Plate Volume"])
  pH_kVol <- as.double(meta_df$value[meta_df$parameter == "kVol"])


  plate_id <- meta_df$value[meta_df$parameter == "Plate Barcode"]
  cartridge_barcode <- meta_df$value[meta_df$parameter == "Cartridge Barcode"]
  assay_name <- meta_df$value[meta_df$parameter == "Assay Name"]
  instrument_serial <- meta_df$value[meta_df$parameter == "Instrument Serial"]

  pH_targetEmission <- as.double(pH_target_emission[[1]])
  O2_targetEmission <- as.double(O2_target_emission[[1]])
  gain1 <- as.double(gain1[[1]])
  gain2 <- as.double(gain2[[1]])

  # other constants
  O2_0_mmHg <- 151.6900241
  O2_0_mM <- 0.214

  if (date_style == "US"){
    date_run <- lubridate::mdy_hm(meta_df$value[meta_df$parameter == "Last Run"])
    logger::log_info("Converted date to US format (US = mdy_hm, NL = dmy_hm).") # (Date-time column)
    #be carefull with the data format in excel! either mdy or dmy
  }

  if (date_style == "NL"){
    date_run <- lubridate::dmy_hm(meta_df$value[meta_df$parameter == "Last Run"])
    logger::log_info("Converted date to NL format (US = mdy_hm, NL = dmy_hm).") # (Date-time column)
    #be carefull with the data format in excel! either mdy or dmy
  }

  if (date_style == "empty"){
    date_run <- meta_df$value[meta_df$parameter == "Last Run"] # (Character instead of date-time column)
    logger::log_info("Date-style is empty, no date conversion was performed. Format is 'character' instead of 'date'.")
    #be carefull with the data format in excel! either mdy or dmy
  }

  if(instrument == "XFHSmini"){
    tibbler <- tibble::tibble(
      F0 = 4.63e04,
      V_C = 9.15,
      Tau_AC = 746,
      Tau_W = 296,
      Tau_C = 246,
      Tau_P = 60.9,
      KSV = 2.06e-02,
      KSV_corrected = 2.06e-02,
      KSV_original = 2.06e-02,
      KSV_tempCorrection = FALSE,
      gain1,
      gain2,
      pH_0,
      pH_plateVolume,
      pH_kVol,
      pH_targetEmission,
      O2_targetEmission,
      plate_id,
      cartridge_barcode,
      date_run,
      assay_name,
      instrument_serial,
      O2_0_mmHg,
      O2_0_mM
    )

  }
  if(instrument == "XFe96"){
    tibbler <- tibble::tibble(
      F0,
      V_C,
      Tau_AC, Tau_W,
      Tau_C, Tau_P,
      KSV,
      KSV_tempCorrection,
      KSV_original,
      gain1,
      gain2,
      pH_0,
      pH_plateVolume,
      pH_kVol,
      pH_targetEmission,
      O2_targetEmission,
      plate_id,
      cartridge_barcode,
      date_run,
      assay_name,
      instrument_serial,
      O2_0_mmHg,
      O2_0_mM
    )
  }
  

  tibbler$norm_available <- norm_available
  tibbler$xls_ocr_backgroundcorrected <- xls_ocr_backgroundcorrected

  cli::cli_alert_info("Finished collecting assay information.")

  return(tibbler)
}

# get_platelayout_data() -------------------------------------------------
#' Get plate layout data.
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.
#' @param my_sheet Sheet of the Seahorse Excel file
#' @param my_range Range of the cells in the Seahorse Excel file
#' @param my_param Summarised name of the parameter which will include the data that is collected.
#'
#' @return data frame with plate layout data.
#'
#' @noRd
#' @keywords internal
#'
#' @examples
#' get_platelayout_data(system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue"), "Assay Configuration", "B96:N104", "bufferfactor")
#' get_platelayout_data(system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue"), "Assay Configuration", "B84:N92", "cell_n")
#' get_platelayout_data(system.file("extdata", "20200110_SciRep_PBMCs_donor_B.xlsx", package = "seahtrue"), "Calibration", "P16:AB24", "pH_cal_em")
#' get_platelayout_data(system.file("extdata", "20200110_SciRep_PBMCs_donor_C.xlsx", package = "seahtrue"), "Calibration", "B7:N15", "O2_cal_em")

get_platelayout_data <- function(filepath_seahorse, my_sheet, my_range, my_param ){

      df <- readxl::read_excel(filepath_seahorse, sheet = my_sheet, range = my_range)

      colnames(df)[1] <- "firstCol"

      df <-  tidyr::gather(df, key = "key", value = "my_value", -firstCol) %>%
        dplyr::mutate(firstCol = paste0(firstCol, key) ) %>%
        dplyr::select(well = firstCol, my_value) %>%
        dplyr::arrange(gsub("\\d", "", well, as.numeric(gsub("\\D", "", well))))

      colnames(df)[2] <- my_param

      # add a zero between letter and number if wellname has 2 characters for normalization data
      for (i in 1:nrow(df)){
        if (nchar(df$well[i]) ==  2) {
          wellName <- sub("(.{1})(.*)", "\\10\\2", df$well[i])
        } else {
          wellName <- df$well[i]
        }
        df$well[i] <- wellName
      }

     return(df)

}



