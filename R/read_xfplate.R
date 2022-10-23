# read_xfplate.r
# vincent de boer
# September 24th, 2022

# get_xf_raw() --------------------------------------------------------------
#' Get the data of the Seahorse 'Raw'-sheet
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.
#'
#' @return xf_raw tibble (list) with 'Raw' Seahorse information.
#'
#' @examples
#' get_xf_raw(file.path(working_directory, paste("/data-raw/seahorse_test_data.xlsx")))
get_xf_raw <- function(filepath_seahorse){

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

  return(xf_raw)
}

# get_xf_norm -------------------------------------------------------------

#' Get normalization info from the Assay Configuration sheet.
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.
#'
#' @return List consisting well names and the corresponding normalization values.
#'
#' @examples get_xf_norm(here::here("data-raw", "seahorse_test_data.xlsx")
get_xf_norm <- function(filepath_seahorse){
  norm_info <- get_platelayout_data(filepath_seahorse,
                                    my_sheet = "Assay Configuration",
                                    my_range = "B84:N92",
                                    my_param = "cell_n")
  return(norm_info)

  if (sum(is.na(norm_info$cell_n)) >90){
    norm_available <- FALSE
  } else {
    norm_available <- TRUE}

  xf_norm <- list(norm_info, norm_available)

  return(xf_norm)
}

# get_xf_flagged() -----------------------------------------------------

get_xf_flagged <- function(filepath_seahorse){

  #read excel file using tidyxl
  x <- xlsx_cells(filepath_seahorse, "Assay Configuration")
  formats <- xlsx_formats(filepath_seahorse, "Assay Configuration")

  # subset to only the platelayout with the cells that show the "unselected" wells by user
  subset_x <- x %>% filter(row %in% c(12:19)) %>% filter(col %in% c(3:14))

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
    pull(address) %>% substr(1,1) %>%
    str_c(collapse = "---") %>%
    str_replace_all(c("C" = "01", "D" = "02", "E" = "03", "F" = "04", "G" = "05", "H" = "06",
                      "I" = "07", "J" = "08", "K" = "09", "L" = "10", "M" = "11", "N" = "12"))
  new_col_names <-   unlist(str_split(new_col_names, "---"))

  new_row_names <- flagged_df %>%
    pull(address) %>% substr(2,3) %>%
    str_c(collapse = "---") %>%
    str_replace_all(c("12" = "A", "13" = "B", "14" = "C", "15" = "D", "16" = "E", "17" = "F",
                      "18" = "G", "18" = "H"))
  new_row_names <-   unlist(str_split(new_row_names, "---"))

  # output the wells that were "unselected" (flagged)
  flagged_vector <- paste0(new_row_names, new_col_names)

  return(flagged_vector)
}

# get_xf_rate -------------------------------------------------------------

get_xf_rate <- function(filepath_seahorse){
  #first item is table, second ite is background_corrected logical
  xf_rate_list <- get_originalRateTable(filepath_seahorse)
}

# get_xf_buffer -----------------------------------------------------------

get_xf_buffer <- function(filepath_seahorse){

  bufferfactor_info <- get_platelayout_data(filepath_seahorse,
                                            my_sheet = "Assay Configuration",
                                            my_range = "B96:N104",
                                            my_param = "bufferfactor")
}

# get_xf_pHcal ------------------------------------------------------------

get_xf_pHcal <- function(filepath_seahorse){

  pH_calibration <- get_platelayout_data(filepath_seahorse,
                                         my_sheet = "Calibration",
                                         my_range = "P16:AB24",
                                         my_param = "pH_cal_em")
}

# get_xf_O2cal ------------------------------------------------------------

get_xf_O2cal <- function(filepath_seahorse){
  pH_calibration <- get_platelayout_data(filepath_seahorse,
                                         my_sheet = "Calibration",
                                         my_range = "B7:N15",
                                         my_param = "O2_cal_em")
}

# get_xf_in()j --------------------------------------------------------------
get_xf_inj <- function(filepath_seahorse, injscheme = "HAP"){

  #command_index in "operation log" sheet give numbers to the phases in a seahorse exp
  # each command (eg. "mix", "measure") gets the command_index for that phase
  # 0 = moving operation
  # 1 = calibration
  # 2 = baseline
  # 3 = injection 1
  # 4 = injection 2
  # 5 = injection 3
  # 6 = injection 4

  #read injection strategy and measurements from "operation log" sheet
  info_sh<-read_excel(filepath_seahorse, sheet = "Operation Log")
  colnames(info_sh) <- c("instruction_name","command_name",
                         "command_index","start_time","end_time",
                         "completion_status")

  if (injscheme == "HAP"){
    #assumes injection names are available in operation log file (this is the case for most experiments)
    measurement_info <- filter(info_sh, command_name == "Measure")
    measurement_info$interval <- measurement_info$command_index -1
    measurement_info$measurement <- 1:nrow(measurement_info)
    measurement_info <- measurement_info %>% select(measurement, interval, injection=instruction_name)
  }

  if (injscheme == "manual"){

    #in case there is no command index in "opertion log"
    command_names <- c("XF - PC_Measure", "XF - PC_Inject")
    measurement_info <- filter(info_sh, command_name %in% command_names)

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
    measurement_info <- filter(measurement_info, command_name == "XF - PC_Measure")
    measurement_info$measurement <- 1:nrow(measurement_info)
    measurement_info <- measurement_info %>% select(measurement, interval)

    #gives name of the injection manually
    # case mitostress
    injections <- c("basal", "OM", "FCCP", "AM/rot")
    injections_mitostress <- tibble(interval = 1:4, injection=c("basal", "OM", "FCCP", "AM/rot"))
    measurement_info <- left_join(measurement_info, injections_mitostress, by = c("interval"))

    ## case glycostress
    #injections <- c("basal", "glucose", "OM", "2DG")
    #injections_glycostress <- tibble(interval = 1:4, injection=injections)
    #measurement_info <- left_join(measurement_info, injections_glycostress, by = c("interval"))
  }



  return(measurement_info)

}

# get_assay_info_new() ----------------------------------------------------
get_xf_assayinfo <- function(filepath_seahorse,
                             date_style = "US",
                             instrument = "XFe96",
                             norm_available,
                             xls_ocr_backgroundcorrected) {

  if (instrument == "XFHSmini"){
    gain1_cell <-  "D68"
    gain2_cell <-  "E68"
  }

  if (instrument == "XFe96"){
    gain1_cell <-  "D70"
    gain2_cell <-  "E70"
  }

  # read Assay Configuration sheet
  meta_df <- read_excel(filepath_seahorse,
                        sheet = "Assay Configuration",
                        col_names = c("parameter", "value"),
                        range = "A1:B83"
  )
  meta_df <- meta_df[!is.na(meta_df$parameter), ]


  # read Assay Configuration sheet gain1
  gain1 <- read_excel(filepath_seahorse,
                      sheet = "Assay Configuration",
                      col_names = c("value"),
                      range = gain1_cell
  )

  # read Assay Configuration sheet gain2
  gain2 <- read_excel(filepath_seahorse,
                      sheet = "Assay Configuration",
                      col_names = c("value"),
                      range = gain2_cell
  )

  # read target emission cells
  O2_target_emission <- read_excel(filepath_seahorse,
                                   sheet = "Calibration",
                                   col_names = FALSE,
                                   range = "B4"
  )

  pH_target_emission <- read_excel(filepath_seahorse,
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
    #be carefull with the data format in excel! either mdy or dmy
  }

  if (date_style == "NL"){
    date_run <- lubridate::dmy_hm(meta_df$value[meta_df$parameter == "Last Run"])
    #be carefull with the data format in excel! either mdy or dmy
  }

  if(instrument == "XFHSmini"){
    tibbler <- tibble(
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
    tibbler <- tibble(
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
#' @return dataframe with plate layout data.
#'
#' @examples
#' get_platelayout_data(here::here("data-raw", "seahorse_test_data.xlsx"), "Assay Configuration", "B84:N92", "cell_n")
#' get_platelayout_data(here::here("data-raw", "seahorse_test_data.xlsx"), "Assay Configuration", "B96:N104", "bufferfactor")
#' get_platelayout_data(here::here("data-raw", "seahorse_test_data.xlsx"), "Calibration", "P16:AB24", "pH_cal_em")
#' get_platelayout_data(here::here("data-raw", "seahorse_test_data.xlsx"), "Calibration", "B7:N15", "O2_cal_em")

get_platelayout_data <- function(filepath_seahorse, my_sheet,my_range, my_param ){

  df <- read_excel(filepath_seahorse, sheet = my_sheet, range = my_range)

  colnames(df)[1] <- "firstCol"

  df <-  gather(df, key = "key", value = "my_value", -firstCol) %>%
    mutate(firstCol = paste0(firstCol, key) ) %>%
    select(well = firstCol, my_value) %>%
    arrange(gsub("\\d", "", well, as.numeric(gsub("\\D", "", well))))

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

# get_originalRateTable() -------------------------------------------------
get_originalRateTable<- function(filepath_seahorse){

  original_rate_df <-read_excel(filepath_seahorse, sheet = "Rate")

  # because rate data can be either background corrected or not this should be checked first
  check_background <- original_rate_df %>% filter(Group == "Background") %>% select(OCR) %>%
    summarise(mean = mean(OCR)) %>% pull(mean)

  if (check_background == 0) {
    corrected_allready <- TRUE
  } else {
    corrected_allready <-  FALSE
  }

  if (corrected_allready == TRUE){
    colnames(original_rate_df) <- c("measurement","well", "group", "time_wave", "OCR_wave_bc", "ECAR_wave_bc", "PER_wave_bc")
    original_rate_df <- original_rate_df %>%
      mutate(OCR_wave = 0, ECAR_wave = 0)

    original_rate_df <- original_rate_df %>%
      select(measurement, well, group, time_wave, OCR_wave, OCR_wave_bc, ECAR_wave, ECAR_wave_bc)

  } else{
    colnames(original_rate_df) <- c("measurement","well", "group", "time_wave", "OCR_wave", "ECAR_wave", "PER_wave")

    #do background substraction for wave table
    background<-original_rate_df %>%
      filter(group=="Background") %>%
      group_by(measurement) %>%
      summarize(bkg_OCR_wave = mean(OCR_wave),
                bkg_ECAR_wave = mean(ECAR_wave)
      )
    original_rate_df<-left_join(original_rate_df, background, by = c("measurement"), copy = TRUE)

    original_rate_df$OCR_wave_bc <- original_rate_df$OCR_wave - original_rate_df$bkg_OCR_wave
    original_rate_df$ECAR_wave_bc <- original_rate_df$ECAR_wave - original_rate_df$bkg_ECAR_wave

    original_rate_df <- original_rate_df %>%
      select(measurement, well, group, time_wave, OCR_wave, OCR_wave_bc, ECAR_wave, ECAR_wave_bc)
  }

  original_rate_df_list <- list(original_rate_df, corrected_allready)

  return(original_rate_df_list)

}



# read_xfplate() -------------------------------------------------------
#' Read necessary Seahorse plate from Seahorse Excel file.
#'
#' @param filepath_seahorse Absolute path to the Seahorse Excel file.
#' This Excel file is converted from the assay result file (.asyr) downloaded from
#' the Agilent Seahorse XF Wave software.
#'
#' @return xf list with all necessary Seahorse data.
#' @export
#'
#' @examples
read_xfplate <- function(filepath_seahorse) {

  #read data
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
                                   xls_ocr_backgroundcorrected =xf_rate[[2]])
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
    buffer = xf_buffer,
    flagged = xf_flagged,
    filepath_seahorse = filepath_seahorse
  )

  return(xf)
}


