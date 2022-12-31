#' Validate.R contains functions to validate the Seahorse (Excel) data.
#' For checking whether a data set meets presumptions or expectations.
#'
#' @description
#' validate_yaml_rules uses a dataframe and yaml rules to validate values.
#' @param rules_yaml: yaml file which contains the rules that are nessecary for validation.
#' @param data_frame: dataframe with values to be validated.
#' @return: output of the validation.
#'
validate_yaml_rules <- function(rules_yaml, data_frame) {
  out <- tryCatch(
    {
      logger::log_info('Validating yaml file...')
      # create a list of data quality demands with the validator() function
      all_rules <- validate::validator(.file = rules_yaml)
      # Confront the data with those rules and save the output into a variable called out
      out <- validate::confront(data_frame, all_rules)
      logger::log_info('Validation finished.')
      return(out)
    },
    warning = function(war) {
      cat("WARNING :", conditionMessage(war), "\n")
      log_warn(conditionMessage(war), "\n")
    },
    error = function(err) {
      cat("ERROR :", conditionMessage(err), "\n")
      log_error(conditionMessage(err), "\n")
    }
  )
}

#' plot_validation_output plots the output of a validation (using yaml file)
#'
#' @param validation_out: the output of the validation.
#' @param pdf_path: path to the output pdf file, which plots the validation.
#' @export
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
      validation_summary <- validate::summary(validation_output)
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


# get name for yaml file and pdf validation file.
get_val_name <- function(xf_plate_pr){
    out <- tryCatch(
      {
    logger::log_info("Get name for yaml file and pdf validation file.")
    xf_pr = xf_plate_pr %>%
      substitute(.) %>%
      deparse(.)

    val_yaml_xf_pr_raw = gsub(" ", "", paste("validate", "_", xf_pr, "_", "raw"))
    val_yaml_xf_pr_assay_info = gsub(" ", "", paste("validate", "_", xf_pr, "_", "assay_info"))

    return(list(val_yaml_xf_pr_raw, val_yaml_xf_pr_assay_info))
      },
  warning = function(war) {
    cat("WARNING :", conditionMessage(war), "\n")
    log_warn(conditionMessage(war), "\n")
  },
  error = function(err) {
    cat("ERROR :", conditionMessage(err), "\n")
    log_error(conditionMessage(err), "\n")
  }
  )
}

# export rules to yaml file.
export_val_yaml <- function(rules, yaml_rule_name){
  out <- tryCatch(
    {
  logger::log_info("Export rules to yaml file, using corresponding unique names.")
  validate::export_yaml(rules, file=here::here("assertions", glue::glue("{yaml_rule_name}.yaml")))
    },
  warning = function(war) {
    cat("WARNING :", conditionMessage(war), "\n")
    log_warn(conditionMessage(war), "\n")
  },
  error = function(err) {
    cat("ERROR :", conditionMessage(err), "\n")
    log_error(conditionMessage(err), "\n")
  }
  )
}
