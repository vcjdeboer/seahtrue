#' Validate.R contains functions to validate the Seahorse (Excel) data.
#' For checking whether a data set meets presumptions or expectations.
#'
#' @description
#' validate_yaml_rules uses a dataframe and yaml rules to validate values.
#' @param rules_yaml: yaml file which contains the rules that are necessary for validation.
#' @param data_frame: dataframe with values to be validated.
#' @return: output of the validation.
#'
validate_yaml_rules <- function(rules_yaml, data_frame) {
  out <- tryCatch(
    {
      logger::log_info('Validating yaml file...')
      # create a list of data quality demands with the validator() function
      all_rules <- validate::validator(.file = rules_yaml)
      # Confront the data with those rules and save the output into a variable called out.
      out <- validate::confront(data_frame, all_rules)
      logger::log_info('Validation finished.')
      return(out)
    },
    warning = function(war) {
      cat("WARNING :", conditionMessage(war), "\n")
      logger::log_warn(conditionMessage(war), "\n")
    },
    error = function(err) {
      cat("ERROR :", conditionMessage(err), "\n")
      logger::log_error(conditionMessage(err), "\n")
    }
  )
}

#' plot_validation_output plots the output of a validation (using yaml file)
#'
#' @param validation_out: the output of the validation.
#' @param pdf_path: path to the output pdf file, which plots the validation.
#' @return None
#'
plot_validation_output <- function(validation_out, pdf_path) {
  out <- tryCatch(
    {
      logger::log_info('Opening a pdf file: {pdf_path}')
      grDevices::pdf(pdf_path)
      logger::log_info('Creating a plot inside pdf file.')
      plot(validation_out)
      grDevices::dev.off()
      logger::log_info('Closed the pdf file.')
    },
    warning = function(war) {
      cat("WARNING :", conditionMessage(war), "\n")
      logger::log_warn(conditionMessage(war), "\n")
    },
    error = function(err) {
      cat("ERROR :", conditionMessage(err), "\n")
      logger::log_error(conditionMessage(err), "\n")
    }
  )
}

#' Summarise validation output
#' @param validation_output: output of the validation.
#' @return validation_summary with summary of the validation.
#'

summarise_out <- function(validation_output) {
  out <- tryCatch(
    {
      logger::log_info('Summarising validation output')

      validation_summary <- tibble::as_tibble(validate::summary(validation_output))

      return(validation_summary)
    },
    warning = function(war) {
      cat("WARNING :", conditionMessage(war), "\n")
      logger::log_warn(conditionMessage(war), "\n")
    },
    error = function(err) {
      cat("ERROR :", conditionMessage(err), "\n")
      logger::log_error(conditionMessage(err), "\n")
    }
  )
}

### Add label and description to rules ####
rules_description <- function(rules, lbl, descr, id){
  validate::label(rules[id]) <- lbl
  validate::description(rules[id]) <- descr
}


#' Get name for yaml file and pdf validation file.
#'
#' @param xf_plate_pr A preprocessed seahorse dataframe.
#'
#' @return List with names for pdf and yaml files.

get_val_name <- function(xf_plate_pr){
    out <- tryCatch(
      {
    logger::log_info("Get name for yaml file and pdf validation file.")
    xf_pr = xf_plate_pr %>%
      substitute(.) %>%
      deparse(.)

    val_yaml_xf_pr_raw = gsub(" ", "",
                              paste("validate", "_", xf_pr, "_", "raw"))
    val_yaml_xf_pr_assay_info = gsub(" ", "", paste("validate", "_", xf_pr, "_", "assay_info"))

    return(list(val_yaml_xf_pr_raw, val_yaml_xf_pr_assay_info))
      },
  warning = function(war) {
    cat("WARNING :", conditionMessage(war), "\n")
    logger::log_warn(conditionMessage(war), "\n")
  },
  error = function(err) {
    cat("ERROR :", conditionMessage(err), "\n")
    logger::log_error(conditionMessage(err), "\n")
  }
  )
}

#' Export rules to yaml file.
#'
#' @param rules Validation criteria.
#'
#' @return None
export_val_yaml <- function(rules, yaml_rule_path){
  out <- tryCatch(
    {
  logger::log_info("Export rules to yaml file, using corresponding unique names.")
  validate::export_yaml(rules, yaml_rule_path)
    },
  warning = function(war) {
    cat("WARNING :", conditionMessage(war), "\n")
    logger::log_warn(conditionMessage(war), "\n")
  },
  error = function(err) {
    cat("ERROR :", conditionMessage(err), "\n")
    logger::log_error(conditionMessage(err), "\n")
  }
  )
}

#' Create path to validation file.
#'
#' @param df_col_name Name of column + dataframe ("df$col").
#'
#' @return Path to yaml file

val_yaml_path <- function(df_col_name){

  df_col_name = gsub("\\$", "_", df_col_name)

  val_yaml_path = here::here("assertions",
                             glue::glue("{df_col_name}.yaml"))

  return(val_yaml_path)

}

#' Provide validate summary and check on failures. Log failures to user.
#'
#' @param rule Name of rule set.
#' @param summary Summary related to rule set.
#'
#' @return "Passed" or "Failed" for a specific rule set.

check_fails <- function(rule, summary){

  failures <- summary %>%
    dplyr::select(name, fails) %>%
    dplyr::mutate(rule) %>%
    dplyr::filter(fails == 1)

  if (nrow(failures > 0)){

    validation <- "Failed"

     x <- purrr::map2(.x = as.list(failures$name),
                      .y = as.list(failures$rule),
                      .f = log_rule_fail)

    logger::log_info("The validation checks failed for the preprocessed seahorse dataset.
    To solve these failure(s), check the related column in your seahorse Excel file for the given rule.")

  } else {

    validation <- "Passed"

    logger::log_info(glue(
      "PASS: The preprocessed seahorse dataset pass validation checks for rule set {rule}."))
  }

  return(validation)
}

#' Log validation failures.
#'
#' @param name List with names of rules for validation.
#' @param rule List with names of for each rule set.

log_rule_fail <- function(name, rule){
  logger::log_error(glue("FAIL {name} at RULE '{rule}'
                        Validation for preprocessed seahorse dataset failed."))
}

#' Helper: Check if values of a given column are within a specified range.
#'
#' @param col_1 Column well which contains "A01, "A02" etc.
#' @param col_2 Column that will be validated on a specific range.
#' @param min min Minimum range
#' @param max Maximum range
#'
#' @return List with col_1, col_2 and a message which shows if value of col_2 is in range.
#'
#' @examples
between_min_max <- function(col_1, col_2, min, max){
  col_value_out_of_range <- list()
  true_false = dplyr::between(as.numeric(col_2), min, max)
  if(true_false == FALSE){
    col_value_out_of_range <- append(col_value_out_of_range, col_1)
    col_value_out_of_range <- append(col_value_out_of_range, col_2)
    col_value_out_of_range <- append(col_value_out_of_range, glue::glue(paste("Given column value for {col_1} {col_2} is not between {min} and {max}.")))
    return(col_value_out_of_range)
  }
  else{return(NULL)
    }
  }

#' Check if values of a given column are within a specified range.
#'
#' @param col_1 Column well which contains "A01, "A02" etc.
#' @param col_2 Column that will be validated on a specific range.
#' @param min Minimum range
#' @param max Maximum range
#'
#' @return Nested tibble which contains [[1]] well [[2]] col_2 value [[3]] out_of_range value (FALSE) [[4]] String with warning message.
#'
#' @examples check_range(data$raw_data[[1]]$well, data$raw_data[[1]]$O2_mmHg, min=70, max = 180)
check_range <- function(col_1, col_2, min, max){

  # loop over 2 columns, check if value between min and max.
  col_value_out_of_range <- purrr::map2(.x = col_1,
                                        .y = col_2,
                                        ~between_min_max(.x,
                                                         as.numeric(.y),
                                                         min,
                                                         max))

  # remove NULL values from nested list.
  col_value_out_of_range[sapply(col_value_out_of_range, is.null)] <- NULL


  for(value in col_value_out_of_range){
    cli::cli_alert_warning(value[3])
  }

  return(col_value_out_of_range)
}

