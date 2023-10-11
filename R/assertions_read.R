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

    logger::log_info("Validate the preprocessed data.")
    xf_plate_pr_raw_rules <- validate::validator(.file = raw_yaml_path)
    xf_assay_info_rules <- validate::validator(.file = assay_info_yaml_path)

    raw_val_output <- validate_df(df1, xf_plate_pr_raw_rules)
    assay_info_val_output <- validate_df(df2, xf_assay_info_rules)

    if (!raw_val_output$validation_passed){
      for (row in 1:nrow(raw_val_output$failed_params)){
        input <- log_validation(raw_val_output$failed_params[row,], df1_name)
        if (input == 1){
        }
        if (input == 2){
          cli::cli_abort("You stopped the Seahtrue analysis.")
        }
      }
    }

    if (!assay_info_val_output$validation_passed){
      for (row in 1:nrow(assay_info_val_output$failed_params)){
        input <- log_validation(assay_info_val_output$failed_params[row,], df2_name)
        if (input == 1){
        }
        if (input == 2){
          cli::cli_abort("You stopped the Seahtrue analysis.")
        }
        
      }
    }
    
    logger::log_info("Finsished validate the preprocessed data.")
}
