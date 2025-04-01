#' Read, Preprocess, and Validate Seahorse XF Excel File
#'
#' @description
#' This function reads a Seahorse XF assay result `.xlsx` file (exported from Wave),
#' and processes it through reading, preprocessing, and validation steps.
#' It performs QC validation using either default or user-defined quality control ranges.
#'
#' @param filepath_seahorse Character. Absolute path to the Seahorse Excel file.
#'   This file should be exported from Agilent Wave and include the expected sheets.
#' @param my_instrument Character. The type of Seahorse instrument used.
#'   Default is `"XFe96"`.
#' @param use_default_qc_ranges Logical. If `TRUE` (default), built-in QC ranges are used:
#'   O2 = [50-180] mmHg, pH = [6.8-7.6].
#' @param qc_ranges Optional list of custom QC ranges, created with [define_qc_ranges()].
#'   This is only used if `use_default_qc_ranges = FALSE`.
#'
#' @return A nested tibble with the processed Seahorse plate data, including:
#' \describe{
#'   \item{plate_id}{Barcode of the plate (if available)}
#'   \item{filepath_seahorse}{Path to the `.xlsx` file}
#'   \item{date_run}{Timestamp when the plate was run}
#'   \item{date_processed}{Timestamp when this object was created}
#'   \item{assay_info}{Metadata from the assay configuration and calibration sheets}
#'   \item{injection_info}{Data frame with injection (operation log) info}
#'   \item{raw_data}{Preprocessed raw data from the Raw sheet}
#'   \item{rate_data}{Preprocessed OCR/ECAR data from the Rate sheet}
#'   \item{validation_output}{List-column with validation outputs and QC flags}
#' }
#'
#' @details
#' This is the main entry point for working with Seahorse `.xlsx` files. It wraps
#' around lower-level functions like [read_xfplate()], [preprocess_xfplate()],
#' and [validate_preprocessed()]. It also performs basic file and sheet existence checks.
#'
#' @seealso [define_qc_ranges()], [validate_preprocessed()]
#'
#' @export
#'
#' @import logger rlang cli glue readxl
#'
#' @examples
#' # Example using internal test file:
#' revive_xfplate(
#'   system.file("extdata", "20191219_SciRep_PBMCs_donor_A.xlsx",
#'     package = "seahtrue"
#'   )
#' )
revive_xfplate <- function(filepath_seahorse,
                           my_instrument = "XFe96",
                           use_default_qc_ranges = TRUE,
                           qc_ranges = list()) {
    logger::log_info("Start reviving")

    # Check if argument is present
    rlang::check_required(filepath_seahorse)

    # Check if file exists.
    if (!file.exists(filepath_seahorse)) {
        cli::cli_abort(
            glue::glue("The following file 
                  does not exist: {basename(filepath_seahorse)}"),
            wrap = TRUE
        )
    }

    # Check if required sheets are available in input file
    sheets_required <- c(
        "Assay Configuration",
        "Rate",
        "Raw",
        "Calibration",
        "Operation Log"
    )
    my_excel_sheets <-
        readxl::excel_sheets(filepath_seahorse)

    my_missing_sheets <- missing_strings(
        my_excel_sheets,
        sheets_required
    )

    if (!is.null(my_missing_sheets)) {
        if (length(my_missing_sheets) > 1) {
            cli::cli_abort(
                glue::glue("The following sheets 
                  do not exist: {my_missing_sheets}"),
                wrap = TRUE
            )
        } else {
            cli::cli_abort(
                glue::glue("The following sheet 
                      does not exist: {my_missing_sheets}"),
                wrap = TRUE
            )
        }
    }

    #check the qc arguments
    if (use_default_qc_ranges) {
      cli::cli_alert("Using default QC ranges: O2 = [50-180] mmHg, pH = [6.8-7.6].")
      qc_ranges <- define_qc_ranges(O2_min = 50,
                                    O2_max = 180,
                                    pH_min = 6.8,
                                    pH_max = 7.6)
      
    }
    
    if (!use_default_qc_ranges && inherits(qc_ranges, "qc_ranges")){
      O2_min <- qc_ranges$O2$min
      O2_max <- qc_ranges$O2$max
      pH_min <- qc_ranges$pH$min
      pH_max <- qc_ranges$pH$max
      unit   <- qc_ranges$O2$unit
      cli::cli_inform(
        "Using the {.strong qc_ranges} provided: O2 = [{O2_min}-{O2_max}] {unit}, pH = [{pH_min}-{pH_max}]",
        O2_min = O2_min,
        O2_max = O2_max,
        pH_min = pH_min,
        pH_max = pH_max,
        unit   = unit
      )
      cat("\n")
      qc_ranges <- qc_ranges
    }
    
    if (!use_default_qc_ranges && !inherits(qc_ranges, "qc_ranges")){
      cli::cli_alert("Falling back to default QC ranges: O2 = [50-180] mmHg, pH = [6.8-7.6]. Because the qc_ranges was not generated using the define_qc_ranges function")
      
      qc_ranges <- define_qc_ranges(O2_min = 50,
                                    O2_max = 180,
                                    pH_min = 6.8,
                                    pH_max = 7.6)
    }
    # run the read/preprocess/validate function
    preprocessed_xf_plate <- filepath_seahorse %>%
        read_xfplate(my_instrument = my_instrument) %>%
        preprocess_xfplate() %>%
        validate_preprocessed(qc_ranges)

    logger::log_info("Finished reviving")


    return(preprocessed_xf_plate)
}

#' Glueing mulltiple plates from a folder
#'
#' @description
#' This function takes a folder path and on the available .xlsx files
#' the revive_xfplate() function is run and output in one nested tibble.
#'
#' @param folderpath_seahorse the path to a folder where the .xlsx files
#' are located or a vector of strings pointing to the path of each individual file
#' @param arg_is_folder either TRUE or FALSE. When the input is a vector of path strings
#' use FALSE, is it points to a folder use TRUE
#' @return a nested tibble with all files organized in a row
#' @export
#' @examples
#' c(
#'     system.file("extdata",
#'         "20191219_SciRep_PBMCs_donor_A.xlsx",
#'         package = "seahtrue"
#'     ),
#'     system.file("extdata",
#'         "20191219_SciRep_PBMCs_donor_A.xlsx",
#'         package = "seahtrue"
#'     )
#' ) |>
#'     glue_xfplates(arg_is_folder = FALSE)
glue_xfplates <- function(folderpath_seahorse, arg_is_folder) {
    logger::log_info("Start glueing")
    rlang::check_required(folderpath_seahorse)

    if (arg_is_folder) {
        if (!dir.exists(folderpath_seahorse)) {
            cli::cli_abort(
                glue::glue("The following folder 
                    does not exist: 
                   {basename(folderpath_seahorse)}"),
                wrap = TRUE
            )
        }

        file_list <- list.files(folderpath_seahorse,
            pattern = "*.xlsx",
            full.names = TRUE
        ) %>%
            stringr::str_subset(
              pattern = stringr::fixed("~$"),
              negate = TRUE
            )
    } else {
        file_list <- folderpath_seahorse
    }

    if (is.null(file_list)) {
        cli::cli_abort(
            glue::glue("There are zero .xlsx files in 
                 {(folderpath_seahorse)}"),
            wrap = TRUE
        )
    }

    df <- file_list %>%
        purrr::map(~ .x %>% revive_xfplate()) %>%
        purrr::list_rbind()

    logger::log_info("Finished glueing")

    return(df)
}


define_qc_ranges <- function(O2_min, O2_max, pH_min, pH_max) {
  structure(
    list(
      O2 = list(min = O2_min, max = O2_max, unit = "mmHg"),
      pH = list(min = pH_min, max = pH_max)
    ),
    class = "qc_ranges"
  )
}
