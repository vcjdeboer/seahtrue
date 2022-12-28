# Create a list of data quality demands with the validator() function.
# Rules for prepossessed seahorse data, "Raw" tibble.
xf_plate_pr_raw_rules <- validator(
    well_miss_values = all_complete(well),
    well_number = all(grepl("^[A-H](0[1-9]|1[0-1])\b$|$", well)),

    # Rules for measurement
    measurement_min_1 = all(measurement >= 1),
    measurement_miss_values = all_complete(measurement),

    # Rules for tick
    tick_min_0 = all(tick >= 0),
    tick_miss_values = all_complete(tick),

    # Rules for group
    group_miss_values = all_complete(group),
    group_contains_background = all(exists_any(group=="Background")),
    min_group = length(unique(group)) > 2,

    # # Rules for interval
    interval_numeric = is.numeric(interval),
    interval_miss_values = all_complete(interval),

    # # Rules for injection
    injection_character = is.character(injection),
    injection_miss_values = all_complete(injection),

    # Rules for O2_em_corr
    O2_em_corr_miss_values = all_complete(O2_em_corr),

    # Rules for pH_em_corr
    pH_em_corr_miss_values = all_complete(pH_em_corr),

    # Rules for O2_mmHg
    O2_mmHg_corr_miss_values = all_complete(O2_mmHg),

    # Rules for pH
    pH_numeric = is.numeric(pH),
    pH_miss_values = all_complete(pH),

    # # Rules for O2_em_corr_bkg
    O2_em_corr_bkg_miss_values = all_complete(O2_em_corr_bkg),

    # Rules for pH_em_corr_bkg
    pH_em_corr_bkg_miss_values = all_complete(pH_em_corr_bkg),

    # Rules for O2_mmHg_bkg
    O2_mmHg_bkg_miss_values = all_complete(O2_mmHg_bkg),

    # Rules for pH_bkgd
    ph_bkg_miss_values = all_complete(pH_bkgd),

    # Rules for pH_em_corr_corr_bkg
    pH_em_corr_corr_bkg_miss_values = all_complete(pH_em_corr_corr_bkg)
  )

# Rules for prepossessed seahorse data, "assay_info" tibble.
xf_plate_pr_assay_info_rules <- validator(
  # Rules for F0
  F0_miss_values = all_complete(F0),
  F0_min_1 = F0 >= 1,

  # Rules for V_C
  VC_miss_values = all_complete(V_C),

  # Rules for O2_targetEmission
  O2_targetEmission_miss_values =  all_complete(O2_targetEmission),

  # Rules for Tau_AC
  Tau_AC_miss_values = all_complete(Tau_AC),

  # Rules for Tau_W
  Tau_W_miss_values = all_complete(Tau_W),

  # Rules for Tau_C
  Tau_C_miss_values = all_complete(Tau_C),

  # Rules for Tau_P
  Tau_P_miss_values = all_complete(Tau_P),

  # Rules for KSV
  KSV_miss_values = all_complete(KSV),

  # Rules for gain1
  gain_miss_values = all_complete(gain1),

  # Rules for gain2
  gain_2_miss_values = all_complete(gain2),

  # Rules for pH_0
  pH_0_miss_values = all_complete(pH_0),

  # Rules for pH_targetEmission
  pH_targetEmission_miss_values = all_complete(pH_targetEmission),

  # Rules for plate_id
  plate_id_miss_values = all_complete(plate_id),

  # Rules for pH_targetEmission
  pH_targetEmission_miss_values = all_complete(pH_targetEmission),

  # Rules for O2_0_mmHg
  O2_0_mmHg_miss_values = all_complete(O2_0_mmHg),

)

  # Create a dataframe with rules
  xf_plate_pr_raw_df_rules <- as.data.frame(xf_plate_pr_raw_rules)
  xf_plate_pr_assay_info_df_rules <- as.data.frame(xf_plate_pr_assay_info_rules)

  # Give each rule dataframe an id to work with, required to add rule descriptions.
  rule_id_xf_plate_pr_raw <- xf_plate_pr_raw_df_rules %>%
    select(name) %>%
    mutate(id = row_number())

  # Create a list with key value pairs. Keys are column column values, Values are the corresponding ids.
  # These will be used to generate the description.
  rule_id_xf_plate_pr_raw <- setNames(as.list(rule_id_xf_plate_pr_raw$id), rule_id_xf_plate_pr_raw$name)

  ### Add label and description for well mssing values rule ####
rules_description(xf_plate_pr_raw_rules,
                  lbl="well_missing_values",
                  descr="The functions all_complete() test for missing values or combinations thereof in records.",
                  rule_id_xf_plate_pr_raw$well_miss_val)

  ### Add label and description for well numeric rule ####
rules_description(xf_plate_pr_raw_rules,
                  lbl="well number",
                  descr="Well must contain A-H and must be within range 1-12.",
                  rule_id_xf_plate_pr_raw$well_number)

#   ###measurement####
rules_description(xf_plate_pr_raw_rules,
                  lbl="measurement",
                  descr="Measurment has to be integer.",
                  rule_id_xf_plate_pr_raw$measurement_integer)

rules_description(xf_plate_pr_raw_rules,
                  lbl='measurement min 1',
                  descr="Check if measurement has minimum of 1.",
                  rule_id_xf_plate_pr_raw$measurement_min_1)

rules_description(xf_plate_pr_raw_rules,
                  lbl='measurment_missing_values',
                  descr="The functions all_complete() test for missing values or combinations thereof in records.",
                  rule_id_xf_plate_pr_raw$measurement_miss_val)

# ###tick####
rules_description(xf_plate_pr_raw_rules,
                  lbl="tick_min 0",
                  descr="Check if tick is minimum 0 (if it's below 0 give an error).",
                  rule_id_xf_plate_pr_raw$tick_min_0)

rules_description(xf_plate_pr_raw_rules,
                  lbl="tick missing values",
                  descr="The functions all_complete() test for missing values or combinations thereof in records.",
                  rule_id_xf_plate_pr_raw$tick_miss_val)

# ###group####

rules_description(xf_plate_pr_raw_rules,
                  lbl="group missing values",
                  descr="The functions all_complete() test for missing values or combinations thereof in records.",
                  rule_id_xf_plate_pr_raw$group_miss_values)

rules_description(xf_plate_pr_raw_rules,
                  lbl="group contains background",
                  descr="Checks if column contains Background values.",
                  rule_id_xf_plate_pr_raw$group_contains_background)

rules_description(xf_plate_pr_raw_rules,
                  lbl="min_group",
                  descr="Check if column contains minimum 2 groups.",
                  rule_id_xf_plate_pr_raw$min_group)

# ###interval####
rules_description(xf_plate_pr_raw_rules,
                  lbl="interval numeric",
                  descr="Interval has to be numeric.",
                  rule_id_xf_plate_pr_raw$interval_numeric)

rules_description(xf_plate_pr_raw_rules,
                  lbl="interval missing values",
                  descr="The functions all_complete() test for missing values or combinations thereof in records.",
                  rule_id_xf_plate_pr_raw$interval_miss_values)

# ###injection####
rules_description(xf_plate_pr_raw_rules,
                  lbl='injection_character',
                  descr="Injection has to be character.",
                  rule_id_xf_plate_pr_raw$injection_character)

rules_description(xf_plate_pr_raw_rules,
                  lbl='injection missing values',
                  descr="The functions all_complete() test for missing values or combinations thereof in records.",
                  rule_id_xf_plate_pr_raw$injection_miss_values)

# ###O2_em_corr####
rules_description(xf_plate_pr_raw_rules,
                  lbl='O2_em_corr miss values',
                  descr="The functions all_complete() test for missing values or combinations thereof in records.",
                  rule_id_xf_plate_pr_raw$O2_em_corr_miss_values)

# ###pH_em_corr####
rules_description(xf_plate_pr_raw_rules,
                  lbl='pH_em_corr missing values',
                  descr="The functions all_complete() test for missing values or combinations thereof in records.",
                  rule_id_xf_plate_pr_raw$pH_em_corr_miss_values)

# ###O2_mmHg####
rules_description(xf_plate_pr_raw_rules,
                  lbl='O2_mmHg missing values',
                  descr="The functions all_complete() test for missing values or combinations thereof in records.",
                  rule_id_xf_plate_pr_raw$O2_mmHg_corr_miss_values)

# ###pH####
rules_description(xf_plate_pr_raw_rules,
                  lbl='pH numeric',
                  descr="pH has to be numeric",
                  rule_id_xf_plate_pr_raw$pH_numeric)

rules_description(xf_plate_pr_raw_rules,
                  lbl='pH missing values',
                  descr="The functions all_complete() test for missing values or combinations thereof in records.",
                  rule_id_xf_plate_pr_raw$pH_em_corr_miss_values)

# ###O2_em_corr_bkg####
rules_description(xf_plate_pr_raw_rules,
                  lbl='O2_em_corr_bkg missing values',
                  descr="The functions all_complete() test for missing values or combinations thereof in records.",
                  rule_id_xf_plate_pr_raw$O2_em_corr_bkg_miss_values)

# ###pH_em_corr_bkg####
rules_description(xf_plate_pr_raw_rules,
                  lbl='pH_em_corr_bkg missing values',
                  descr="The functions all_complete() test for missing values or combinations thereof in records.",
                  rule_id_xf_plate_pr_raw$pH_em_corr_bkg_miss_values)

# ###O2_mmHg_bkg####
rules_description(xf_plate_pr_raw_rules,
                  lbl='O2_mmHg_bkg missing values',
                  descr="The functions all_complete() test for missing values or combinations thereof in records.",
                  rule_id_xf_plate_pr_raw$O2_mmHg_bkg_miss_values)

# ###pH_bkgd####
rules_description(xf_plate_pr_raw_rules,
                  lbl='pH_bkgd missing values',
                  descr="The functions all_complete() test for missing values or combinations thereof in records.",
                  rule_id_xf_plate_pr_raw$ph_bkg_miss_values)

# ###pH_em_corr_corr_bkg####
rules_description(xf_plate_pr_raw_rules,
                  lbl='pH_em_corr_corr_bkg missing values',
                  descr="The functions all_complete() test for missing values or combinations thereof in records.",
                  rule_id_xf_plate_pr_raw$pH_em_corr_corr_bkg_miss_values)

#' Validate preprocessed data.
#'
#' @param xf_plate_pr A preprocessed "Raw" data list (tibble).
#' @description Check the preprocessed "Raw" data list against validation rules.
#' @return TRUE when all validation steps pass. FALSE when there is a failure.
#'
#' @examples validate_xf_plate_pr(xf_plate_pr)

validate_xf_plate_pr <- function(xf_plate_pr){
  tryCatch({
      if (exists("xf_plate_pr") && is.data.frame(get("xf_plate_pr")) == TRUE) {

        source(rstudioapi::getSourceEditorContext()$path)

        logger::log_info("get name for yaml file and pdf validation file.")

        val_name_list <- get_val_name(xf_plate_pr)
        val_yaml_xf_pr_raw <- val_name_list[1]
        val_yaml_xf_pr_assay_info <- val_name_list[2]

        logger::log_info("export rules to yaml file.")
        export_yaml(xf_plate_pr_raw_rules, file=here::here("assertions", glue::glue("{val_yaml_xf_pr_raw}.yaml")))
        export_yaml(xf_plate_pr_assay_info_rules ,file=here::here("assertions", glue::glue("{val_yaml_xf_pr_assay_info}.yaml")))

        raw_validation_output <- validate_yaml_rules(here::here("assertions", glue::glue("{val_yaml_xf_pr_raw}.yaml")),
                                                     xf_plate_pr$raw_data[[1]])
        assay_info_validation_output <- validate_yaml_rules(here::here("assertions", glue::glue("{val_yaml_xf_pr_assay_info}.yaml")),
                                                     xf_plate_pr$assay_info[[1]])

        logger::log_info("check if plate_id is right format")
        plate_id_rules <- validator(grepl("^V[0-9]{10}V$", plate_id))
        plate_id_validation_output <- validate::confront(xf_plate_pr, plate_id_rules)

        validation_summary_raw <- summarise_out(raw_validation_output)
        validation_summary_assay_info <- summarise_out(assay_info_validation_output)
        validation_plate_id_rules <- summarise_out(plate_id_validation_output)

        plot_validation_output(raw_validation_output,
                               here::here("assertions",
                                          glue::glue("{val_yaml_xf_pr_raw}.pdf")))
        plot_validation_output(assay_info_validation_output,
                               here::here("assertions",
                                          glue::glue("{val_yaml_xf_pr_assay_info}.pdf")))


        # collect all information and put in list.
        validate_xf_plate_pr_list = list()
        validate_xf_plate_pr_list[[ "assay_info_pr_val" ]] <- list(meta(xf_plate_pr_assay_info_rules),
                                                                   summary(xf_plate_pr_assay_info_rules))

        validate_xf_plate_pr_list[[ "raw_pr_val" ]] <- list(meta(xf_plate_pr_raw_rules),
                                                            summary(xf_plate_pr_raw_rules))


        validation_summary_list <- list(validation_plate_id_rules,
                                        validation_summary_raw,
                                        validation_summary_assay_info)

        logger::log_info("Check if no validation failures occur")
        val_true_false <- c()
        for(summary in validation_summary_list){
          check_all_true <- check_all_true(summary)
          check_all_true <- all(check_all_true)
          val_true_false <- append(val_true_false, check_all_true)
        }

        # can be true or false, depending on validation failures.
        validation_pass <- as.logical(all(val_true_false))

        logger::log_info(glue::glue("Validation failures don't exist for sheet : {validation_pass}"))
        return(validation_pass)
      }
    },

    warning = function(war) {
      cat("WARNING :", conditionMessage(war), "\n")
      logger::log_warn(conditionMessage(war), "\n")
    },
    error = function(err) {
      cat("ERROR :", conditionMessage(err), "\n")
      logger::log_error(conditionMessage(err), "\n")
      logger::log_info(glue::glue("Quiting analysis with sheet: {filepath_seahorse}"))
      break
    }

)
}

check_all_true <- function(validate_summary){
  by(validate_summary[4], seq_len(nrow(validate_summary[4])),
     function(row){
       if(row[1] == 0){return(TRUE)}
       else if(row[1] == 1){
         return(FALSE)
       }
     }
  )
}



