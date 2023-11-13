#' Validate dataframe
#'
#' @param df dataframe used for the validation
#' @param rules validation rules to check the dataframe.
#'
#' @return list with failed params and validation pass
#' @noRd
#' @keywords internal
validate_df <- function(df, rules){

  out <- validate::confront(df, rules)

  # this checks whether there are any rows where 'fails' is higher than 0
  # meaning that the validator rule did not pass for that row
  if (nrow(validate::summary(out) %>%
           dplyr::filter(fails > 0)) > 0){

    failed_params <-
      validate::summary(out) %>%
      dplyr::filter(fails > 0) %>%
      dplyr::select(name,expression)

    validation_passed <- FALSE

  } else {
    failed_params <- NULL
    validation_passed <- TRUE
  }

  return(list(failed_params = failed_params,
              validation_passed = validation_passed))

}

log_validation <- function(val_output, validated_df_name){

  name = val_output$name
  rule = val_output$expression

  cli::cli_alert_warning(glue::glue("Validation for '{name}' in
                         the preprocessed seahorse dataset FAILED.
                         The '{name}' parameter did not pass the following rule: '{rule}'"))
  
  input<-menu(c("Yes", "No"),title="Do you want to continue?")
  
  return(input)

}


#' Validate the preprocessed seahorse dataset.
#'
#' @param xf The preprocessed seahorse dataset.
#'
#' @return None
#' @importFrom magrittr %T>%
#' @noRd
#' @keywords internal
validate_preprocessed <- function(xf){
  
    df1 = xf$raw_data[[1]]
    df1_name = "raw_data"
    df2 = xf$assay_info[[1]]
    df2_name = "assay_info"

    cli::cli_alert_info("Validate the preprocessed data.")
    
    # Provide the validation rules.
    xf_plate_pr_raw_rules <- validate::validator(.file = raw_yaml_path)
    xf_assay_info_rules <- validate::validator(.file = assay_info_yaml_path)

    # Validate using the validation rules. Collect the failed validation rules for each provided rule group.
    # If any rules failed the analysis doesn't meet the validation criteria.
    val_output <- list(raw_val_output = validate_df(df1, xf_plate_pr_raw_rules), 
                        assay_info_val_output = validate_df(df2, xf_assay_info_rules))
    
    # Organise the failed validations.
    validation_failures <- organise_validation_failures(val_output)
    
    return(validation_failures)
}

organise_validation_failures <- function(val_output) {
  # Create an empty data tibble to store data failures.
  failed_validation_rules_tibble <- tibble()
  
  # For each rule group (for example from different yaml files) get the failed rules. 
  # Organise the failed rules to a new tibble.
  for (failed_rules_validation_output in names(val_output)) {
    
    # Extract failed rules from the validation output
    failed_params <- val_output[[failed_rules_validation_output]]$failed_params
    
    # Add the extracted failed rules to a tibble with failed rules.
    failure_tibble <- tibble(
      validation_group = rep(failed_rules_validation_output, length(failed_params$name)),
      failed_rules = failed_params$name,
      expression = failed_params$expression
    )
    
    # Check for duplicates. 
    # Only add non-duplicates values to the tibble with failed rules.
    failure_tibble <- failure_tibble[!duplicated(failure_tibble), ]
    
    # Failures from each validation yaml group are added to the eventual failed tibble that will be returned.
    failed_validation_rules_tibble <- bind_rows(failed_validation_rules_tibble, failure_tibble)
  }
  
  # Return the resulting tibble.
  return(failed_validation_rules_tibble)
}


