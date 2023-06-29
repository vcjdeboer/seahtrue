library(tidyverse)

# step 1 ------------------------------------------------------------------

## set path to file
path_to_fileName <-
  paste0(here::here("inst", "extdata", "20200110 SciRep PBMCs donor B.xlsx"))

# step 2 ------------------------------------------------------------------

## read and preprocess
preprocessed_xfplate <-
  path_to_fileName %>%
  read_xfplate() %>%
  preprocess_xfplate()

# step 3 ------------------------------------------------------------------

## do background qc and add info to "assay_info"
#load data (or the data can be calculated using calc_bkgd_qc_ref.R)
data("qc_well_ref_PBMC", envir = environment())
data("qc_plate_ref_PBMC", envir = environment())
my_qc_well_ref <- qc_well_ref_PBMC
my_qc_plate_ref <- qc_plate_ref_PBMC

preprocessed_xfplate <- set_bkgd_well_flags(preprocessed_xfplate,
                                            my_qc_well_ref,
                                            my_qc_plate_ref)

# step 4 ------------------------------------------------------------------

## remove flagged background wells and calculate mean of non-flagged bkgd wells
processed_xfplate <- preprocessed_xfplate %>%
  dplyr::mutate(O2_raw_data =
           purrr::map2(raw_data,
                assay_info,
                ~dplyr::mutate(.x,
                        O2_em_corr_bkg =
                          calc_background_O2_col(.x,
                                                 .y %>%
                                                   purrr::pluck(1, "O2_flagged_bkgd_wells")))))

# step 5 ------------------------------------------------------------------

## calculate OCR (convert to O2, substract background, calc OCR)

processed_xfplate <- processed_xfplate %>%
  dplyr::mutate(O2_raw_data = purrr::map2(O2_raw_data,
                                   assay_info,
                                   ~dplyr::mutate(.x,
                                           O2 = sternVolmer(.x$O2_em_corr, .y$KSV, .y$F0),
                                           O2_bkgd = sternVolmer(.x$O2_em_corr_bkg, .y$KSV, .y$F0),
                                           O2_M_mmHg = substract_O2_bkgd(O2, O2_bkgd, .y$O2_0_mmHg)))) %>%
  dplyr::mutate(ocr_without_meta = purrr::map2(O2_raw_data,
                                        assay_info,
                                        ~{.x %>%
                                            split(f = as.factor(.$well)) %>%
                                            purrr::map2(rep(list(.y), length(.)),
                                                        ~{add_extraTicks_new_2(.x, "O2_M_mmHg") %>% calculate_OCR_new(.y)}) %>%
                                            dplyr::bind_rows(.id = "well")})) %>%
  dplyr::mutate(ocr = purrr::map2(O2_raw_data,
                           ocr_without_meta,
                           ~{.x %>%
                               dplyr::distinct(well, measurement, .keep_all = TRUE) %>%
                               dplyr::select(well, measurement,
                                      group, interval, bufferfactor,
                                      cell_n, flagged_well) %>%
                               dplyr::left_join(.y, by= c("well", "measurement"))})) %>%
  dplyr::select(-ocr_without_meta)

# step 6 ------------------------------------------------------------------

# flag pH background wells and write to assay_info

pH_flagged_bkgd_wells <- c("")

#add flagged wells to assay info
processed_xfplate <- processed_xfplate %>%
  dplyr::mutate(assay_info = list(.$assay_info %>% purrr::pluck(1) %>%
                             purrr::list_merge(pH_flagged_bkgd_wells = pH_flagged_bkgd_wells)))


# step 7 ------------------------------------------------------------------

# remove pH background wells and calculate mean background pH

processed_xfplate <- processed_xfplate %>%
  dplyr::mutate(pH_raw_data =
           purrr::map2(raw_data,
                assay_info,
                ~dplyr::mutate(.x,
                        pH_bkg =
                          calc_background_pH_col(.x,
                                                 .y %>%
                                                   purrr::pluck(1, "pH_flagged_bkgd_wells")))))

