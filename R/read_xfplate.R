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


read_xfplate <- function(filepath_seahorse) {

    cli::cli_alert(
      glue::glue("Start function to read seahorse plate data from 
                 Excel file: {filepath_seahorse}"))

    # read data
    xf_raw <- get_xf_raw(filepath_seahorse) %>% 
      verify_xf_raw()
    
    xf_assayinfo <- 
      get_xf_assayinfo(filepath_seahorse) %>% 
      verify_xf_assayinfo()

    xf_norm <- 
      get_xf_norm(filepath_seahorse) %>% 
      verify_xf_norm()
    
    
    
    xf_buffer <- get_xf_buffer(filepath_seahorse)
    xf_inj <- get_xf_inj(filepath_seahorse)
    xf_pHcal <- get_xf_pHcal(filepath_seahorse)
    xf_O2cal <- get_xf_O2cal(filepath_seahorse)
    xf_flagged <- get_xf_flagged(filepath_seahorse)
    
    xf_rate <- 
      get_xf_rate(filepath_seahorse) %>% 
      verify_xf_rate(., 
                     xf_flagged)
    
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

    return(xf)

}

# get_xf_raw() --------------------------------------------------------------
#' Get the data from the Seahorse 'Raw'-sheet and clean the names
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.
#'
#' @return xf_raw tibble (list) with 'Raw' Seahorse information and snake_case column names
#`
#' @noRd
#' @keywords internal
#' @import dplyr readxl janitor
#'
#' @examples
#' get_xf_raw(system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue"))
get_xf_raw <- function(filepath_seahorse){

    xf_raw <- readxl::read_excel(filepath_seahorse, 
                                 sheet = "Raw") %>%  
      dplyr::rename_all(., toupper) %>% 
      janitor::clean_names() %>% 
      dplyr::rename_with(~str_replace(., 'o2', 'O2')) %>% 
      dplyr::rename_with(~str_replace(., 'ph', 'pH')) %>% 
      dplyr::rename_with(~str_replace(., 'hg', 'Hg')) %>% 
      dplyr::rename(pH_em_corr = pH_corrected_em,
                    O2_em_corr = O2_corrected_em)
      
   
    return(xf_raw)
    }

#new function
verify_xf_raw <- function(xf_raw){
  
  # Check if required columns are available in input file
  columns_required <- c("measurement", "tick", "well", "group", "timestamp", 
                        "well_temperature", "env_temperature", "O2_is_valid", 
                        "O2_mmHg", "O2_light_emission", "O2_dark_emission", 
                        "O2_ref_light", "O2_ref_dark", "O2_em_corr", 
                        "pH_is_valid", "pH", "pH_light", "pH_dark", 
                        "pH_ref_light", "pH_ref_dark", "pH_em_corr")
  
  my_columns <- names(xf_raw)
  
  my_missing_columns <- missing_strings(my_columns, 
                                       columns_required)
  
  if (!is.null(my_missing_columns)){
    if (length(my_missing_columns)>1){
      cli::cli_abort(
        glue::glue("The following columns 
                  do not exist: {my_missing_columns}"))
    } else {
      cli::cli_abort(
        glue::glue("The following column 
                      does not exist: {my_missing_columns}"))
    }
  }
  
  return(xf_raw)
  
}
# get_xf_norm() -------------------------------------------------------------

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
#' @examples
#' get_xf_norm(system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue"))
get_xf_norm <- function(filepath_seahorse){

    norm_info <- 
      get_platelayout_data(
        filepath_seahorse,
        my_sheet = "Assay Configuration",
        my_range = "B84:N92",
        my_param = "cell_n")
    
    return(norm_info)
}

#new function
verify_xf_norm <- function(xf_norm){
  
  #typically a full plate is copied, thus a high number
  #of NAs is typically a sign that norm is not available
  
  #get the attribute using:
  # attributes(xf_norm) %>%  pluck("norm_available")
  
  if (sum(is.na(xf_norm$cell_n)) >90){
    xf_norm <- xf_norm %>% 
      structure(norm_available = FALSE)
  } else {
    xf_norm <- xf_norm %>% 
      structure(norm_available = TRUE)}
  
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
#' @import tidyxl readxl dplyr stringr tibble
#' @examples
#' get_xf_flagged(system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue"))
get_xf_flagged <- function(filepath_seahorse){

  
    
  
    x <- tidyxl::xlsx_cells(filepath_seahorse, "Assay Configuration")
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
    new_col_names_2 <-   unlist(stringr::str_split(new_col_names, "---"))

    new_row_names <- flagged_df %>%
      dplyr::pull(address) %>% substr(2,3) %>%
      stringr::str_c(collapse = "---") %>%
      stringr::str_replace_all(c("12" = "A", "13" = "B", "14" = "C", 
                                 "15" = "D", "16" = "E", "17" = "F",
                                 "18" = "G", "19" = "H"))
    
    new_row_names_2 <-   unlist(stringr::str_split(new_row_names, "---"))

    # output the wells that were "unselected" (flagged)
    flagged_vector <- paste0(new_row_names_2, new_col_names_2)

    flagged_tibble <- tibble::tibble(
      well = flagged_vector,
      flag = TRUE
    )
    
    return(flagged_tibble)
}

# get_xf_rate() -------------------------------------------------
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
#' get_xf_rate(system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue"))

get_xf_rate <- function(filepath_seahorse){

  xf_rate <- readxl::read_excel(
      filepath_seahorse, 
      sheet = "Rate") %>%  
    janitor::clean_names() 
  
  return(xf_rate)
  
}

#new function
verify_xf_rate <- function(xf_rate, xf_flagged){
  # because rate data can be either background corrected or
  # not in the WAVE software and the export depends on this
  # settings this should be checked first
  
  #if background wells are flagged, this could go wrong
  # therefore they should be removed if flagged, first
  groups <- xf_rate %>%  
    select(well, group) %>% 
    unique()
  
  xf_flagged <- xf_flagged %>% 
    left_join(groups, by = c("well"))
  
  if("Background" %in% xf_flagged$group) {
    
    flagged_bkgd_wells <- xf_flagged %>% 
      filter(group == "Background") %>% 
      pull(well)
    
    xf_rate <-  xf_rate %>% 
      filter(!well %in% flagged_bkgd_wells) 
    
  }
  
  #next do the check whether the data is bkgd corrected
  if (is.null(
    missing_strings(xf_rate %>% 
                      pull(group) %>% 
                      unique(), 
                      "Background") )) {
    
      check_background <- xf_rate %>%
        dplyr::filter(group == "Background") %>%
        dplyr::pull(ocr) %>%
        mean()

      if (check_background == 0) {
        corrected_allready <- TRUE
      } else {
        corrected_allready <-  FALSE
        }
  } else {
    # if Background  group is not present then:
      corrected_allready <- "no_background"
    }
  
  if (corrected_allready == TRUE){
    colnames(xf_rate) <-
      c("measurement","well", "group",
        "time_wave", "OCR_wave_bc",
        "ECAR_wave_bc", "PER_wave_bc")
    xf_rate <- xf_rate %>%
      dplyr::mutate(OCR_wave = 0, ECAR_wave = 0)

    xf_rate <- xf_rate %>%
      dplyr::select(measurement, well, group,
                    time_wave, OCR_wave, OCR_wave_bc,
                    ECAR_wave, ECAR_wave_bc)
    
    cli::cli_alert(
      glue::glue("Rate was exported WITH background correction"))
  } 
  
  if (corrected_allready == "no_background"){
    colnames(xf_rate) <-
      c("measurement","well", "group",
        "time_wave", "OCR_wave",
        "ECAR_wave", "PER_wave")
    xf_rate <- xf_rate %>%
      dplyr::mutate(OCR_wave_bc = 0, ECAR_wave_bc = 0)
    
    xf_rate <- xf_rate %>%
      dplyr::select(measurement, well, group,
                    time_wave, OCR_wave, OCR_wave_bc,
                    ECAR_wave, ECAR_wave_bc)
    
    cli::cli_alert(
      glue::glue("There was NO background group in the data"))
  } 
  
  if (corrected_allready == FALSE){
    colnames(xf_rate) <-
      c("measurement","well", "group",
        "time_wave", "OCR_wave",
        "ECAR_wave", "PER_wave")

    #do background substraction for wave table
    background <- xf_rate %>%
      dplyr::filter(group=="Background") %>%
      dplyr::summarize(bkg_OCR_wave = mean(OCR_wave),
                       bkg_ECAR_wave = mean(ECAR_wave),
                       .by = measurement)
  
    xf_rate <- dplyr::left_join(xf_rate,
                                background,
                                by = c("measurement"))

    xf_rate$OCR_wave_bc <- xf_rate$OCR_wave - xf_rate$bkg_OCR_wave
    xf_rate$ECAR_wave_bc <- xf_rate$ECAR_wave - xf_rate$bkg_ECAR_wave

    xf_rate <- xf_rate %>%
      dplyr::select(measurement, well, group,
                    time_wave, OCR_wave, OCR_wave_bc,
                    ECAR_wave, ECAR_wave_bc)
    
    cli::cli_alert(
      glue::glue("Rate was exported WITHOUT background correction,
                 Therefore, the background correction was added"))
  }

  xf_rate <- xf_rate %>% 
    structure(was_background_corrected = corrected_allready)
  
  
    
  return(xf_rate)

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

    bufferfactor_info <- get_platelayout_data(filepath_seahorse,
                                              my_sheet = "Assay Configuration",
                                              my_range = "B96:N104",
                                              my_param = "bufferfactor")
  

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

  pH_calibration <- get_platelayout_data(filepath_seahorse,
                                         my_sheet = "Calibration",
                                         my_range = "P16:AB24",
                                         my_param = "pH_cal_em")
  
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

  O2_calibration <- get_platelayout_data(filepath_seahorse,
                                         my_sheet = "Calibration",
                                         my_range = "B7:N15",
                                         my_param = "O2_cal_em")

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
get_xf_inj <- function(filepath_seahorse, injscheme = "HAP"){


  #command_index in "Operation Log" sheet give numbers to 
  # the phases in a seahorse exp
  # each command (eg. "mix", "measure") gets the 
  # command_index for that phase
  # 0 = moving operation
  # 1 = calibration
  # 2 = baseline
  # 3 = injection 1
  # 4 = injection 2
  # 5 = injection 3
  # 6 = injection 4

  #read injection strategy and measurements from "Operation Log" sheet
  info_sh<-readxl::read_excel(filepath_seahorse, 
                              sheet = "Operation Log") %>% 
    janitor::clean_names()

  if (injscheme == "HAP"){
    #assumes injection names are available in operation 
    # log file (this is the case for most experiments)
    measurement_info <- 
      dplyr::filter(info_sh, 
                    command_name == "Measure") %>% 
      dplyr::mutate(interval = command_index -1) %>% 
      dplyr::mutate(measurement = 1:n()) %>% 
      dplyr::select(measurement, 
                    interval, 
                    injection=instruction_name)
  }

  if (injscheme == "manual"){
    # this is quite an experimental feature
    # was needed for some old files
    
    #in case there is no command index in "operation log"
    command_names <- c("XF - PC_Measure", "XF - PC_Inject")
    measurement_info <- 
      dplyr::filter(info_sh, command_name %in% command_names)

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

get_xf_assayinfo <- function(filepath_seahorse,
                             date_style = "empty",
                             instrument = "XFe96") {

  if (instrument == "XFHSmini"){
    gain1_cell <-  "D68"
    gain2_cell <-  "E68"
  }

  if (instrument == "XFe96"){
    gain1_cell <-  "D70"
    gain2_cell <-  "E70"
  }

  # read Assay Configuration sheet
    meta_df <- readxl::read_excel(
      filepath_seahorse,
      sheet = "Assay Configuration",
      col_names = c("parameter", "value"),
      range = "A1:B83"
    )

  # pos_vector = c(4, 26, 32, 38, 58, 59, 
  #                60, 61, 62, 63, 65, 66, 67, 76)
  # name_vector = c("Assay Name", "Cartridge Barcode", 
  #                 "Plate Barcode", "Instrument Serial",
  #                 "ksv", "Ksv Temp Correction", 
  #                 "Corrected Ksv", "Calculated FO",
  #                 "Pseudo Volume", "TAC", "TW", 
  #                 "TC", "TP", "Calibration pH")

  meta_df <- meta_df %>% 
    dplyr::filter(!is.na(parameter))
  
  # read Assay Configuration sheet gain1
  gain1 <- 
    readxl::read_excel(
      filepath_seahorse,
      sheet = "Assay Configuration",
      col_names = c("value"),
      range = gain1_cell) %>% 
    dplyr::pull(value) %>%  as.numeric()

  # read Assay Configuration sheet gain2
  gain2 <- 
    readxl::read_excel(
      filepath_seahorse,
      sheet = "Assay Configuration",
      col_names = c("value"),
      range = gain2_cell) %>% 
    dplyr::pull(value) %>%  as.numeric()
  
  # read target emission cells
  O2_targetEmission <- 
    readxl::read_excel(
      filepath_seahorse,
      sheet = "Calibration",
      col_names = c("value"),
      range = "B4") %>% 
    dplyr::pull(value) %>%  as.numeric()
  
  # read pH target emission cells
  pH_targetEmission <- 
    readxl::read_excel(
      filepath_seahorse,
      sheet = "Calibration",
      col_names = c("value"),
      range = "P4") %>% 
    dplyr::pull(value) %>%  as.numeric()
  
  F0 <- meta_df %>% 
    dplyr::filter(parameter == "Calculated FO" ) %>% 
    dplyr::pull(value) %>%  as.numeric()
  
  V_C <- meta_df %>% 
    dplyr::filter(parameter == "Pseudo Volume" ) %>% 
    dplyr::pull(value) %>%  as.numeric()
  
  Tau_AC <- meta_df %>% 
    dplyr::filter(parameter == "TAC" ) %>% 
    dplyr::pull(value) %>%  as.numeric()
  
  Tau_W <- meta_df %>% 
    dplyr::filter(parameter == "TW" ) %>% 
    dplyr::pull(value) %>%  as.numeric()
  
  Tau_C <- meta_df %>% 
    dplyr::filter(parameter == "TC" ) %>% 
    dplyr::pull(value) %>%  as.numeric()
  
  Tau_P <- meta_df %>% 
    dplyr::filter(parameter == "TP" ) %>% 
    dplyr::pull(value) %>%  as.numeric()
  
  KSV_original <- meta_df %>% 
    dplyr::filter(parameter == "ksv" ) %>% 
    dplyr::pull(value) %>%  as.numeric()
  
  KSV_corrected <- meta_df %>% 
    dplyr::filter(parameter == "Corrected Ksv" ) %>% 
    dplyr::pull(value) %>%  as.numeric()
  
  KSV_tempCorrection <- meta_df %>% 
    dplyr::filter(parameter == "Ksv Temp Correction" ) %>% 
    dplyr::pull(value) %>%  as.logical()
  
  KSV <- KSV_corrected
  
  pH_0 <- meta_df %>% 
    dplyr::filter(parameter == "Calibration pH" ) %>% 
    dplyr::pull(value) %>%  as.numeric()
  
  pH_plateVolume <- meta_df %>% 
    dplyr::filter(parameter == "Plate Volume") %>% 
    dplyr::pull(value) %>%  as.numeric()
  
  pH_kVol <- meta_df %>% 
    dplyr::filter(parameter == "kVol") %>% 
    dplyr::pull(value)  %>%  as.numeric()
  
  plate_id <- meta_df %>% 
    dplyr::filter(parameter == "Plate Barcode" ) %>% 
    dplyr::pull(value)
  
  cartridge_barcode <- meta_df %>% 
    dplyr::filter(parameter == "Cartridge Barcode") %>% 
    dplyr::pull(value)
  
  assay_name <- meta_df %>% 
    dplyr::filter(parameter == "Assay Name" ) %>% 
    dplyr::pull(value)
  
  instrument_serial <- meta_df %>% 
    dplyr::filter(parameter == "Instrument Serial") %>% 
    dplyr::pull(value)

  # other constants
  O2_0_mmHg <- 151.6900241
  O2_0_mM <- 0.214

  #not used data_style conversion
  if (date_style == "US"){
    date_run <- lubridate::mdy_hm(
      meta_df$value[meta_df$parameter == "Last Run"])
    #be carefull with the data format in excel! either mdy or dmy
  }

  if (date_style == "NL"){
    date_run <- lubridate::dmy_hm(
      meta_df$value[meta_df$parameter == "Last Run"])
    #be carefull with the data format in excel! either mdy or dmy
  }

  if (date_style == "empty"){
    date_run <- meta_df$value[meta_df$parameter == "Last Run"] 
    # (Character instead of date-time column)
    #be carefull with the data format in excel! either mdy or dmy
  }

  if(instrument == "XFHSmini"){
    ids_constants <- tibble::tibble(
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
    ids_constants <- tibble::tibble(
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

  cli::cli_alert_info("Finished collecting assay information.")

  return(ids_constants)
}

#
#' Verify xf assay info
#'
#' @param xfassay_info tibble with collected data in get_xf_assayinfo()
#'
#'
#' @examples
#' xfassay_info <-get_xf_assayinfo(
#'   system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx", package = "seahtrue"))
#' verify_xf_assayinfo(xfassay_info)
verify_xf_assayinfo <- function(xfassay_info){
  
  plate_id <- xfassay_info %>%  
    purrr::pluck("plate_id", 1)
  
  if (is.null(plate_id)){
    xfassay_info <- xfassay_info %>% 
      dplyr::mutate(plate_id = "no_plate_id_found")
    
    cli::cli_alert(
      glue::glue("no plateid is found in input file"))
    
  } else{
    
    cli::cli_alert(
      glue::glue("plateid is identified as:{plate_id}"))
  }
  
  return(xfassay_info)
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

get_platelayout_data <- function(filepath_seahorse, 
                                 my_sheet, my_range, 
                                 my_param ){

      df <- 
        readxl::read_excel(
          filepath_seahorse, 
          sheet = my_sheet, 
          range = my_range,
          .name_repair = "unique_quiet") %>% 
        dplyr::rename(row = 1) %>% 
        tidyr::pivot_longer(c(-row), 
                            names_to = "col",
                            values_to = "my_values" ) %>% 
        dplyr::mutate(well = dplyr::case_when(
              nchar(col) == 1 ~ paste0(row, "0", col),
              .default = paste0(row, col))) %>% 
        dplyr::select(well,  my_values) %>% 
        dplyr::rename(!!my_param := my_values) #OMG the := :)
        
     return(df)

}

#new util function
missing_strings <- function(my_strings, strings_required){
  
  my_strings_df <- my_strings %>% 
    dplyr::as_tibble() 
  
  rule <- validate::validator(
    value %in% valid_codes)
  
  strings_available<- 
    validate::satisfying(my_strings_df, rule, 
                         ref = list(valid_codes = strings_required)) %>%  
    pull(value)
  
  my_missing_strings <-
    if (!identical(strings_available, strings_required)){
      dplyr::setdiff(strings_required,
              strings_available)}
  
  
  return(my_missing_strings)
  
}


