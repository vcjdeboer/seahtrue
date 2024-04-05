

# set raw yaml rules
xf_plate_pr_raw_rules <- {validate::validator(

  well_miss_values = all_complete(well),
  well_number = all(grepl("^[A-H](0[1-9]|1[0-2])", well)),

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
  O2_mmHg_range = all(in_range(O2_mmHg, min=70, max=180)), #70 and 180
  # O2_mmHg_range = in_range(O2_mmHg, min=140, max=170) alternative each well
  # could confronted with rules as well instead of plate!
  # In that case a single well can be flagged instead of parameter of the whole plate

  # Rules for pH
  pH_numeric = is.numeric(pH),
  pH_miss_values = all_complete(pH),
  pH_range = all(in_range(pH, min=6.5, max=8)), #6.5 8

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
}
descriptions <- {tibble::tibble(rule_name = names(xf_plate_pr_raw_rules),
                       rule_desc =   c("The functions all_complete() test for missing values or combinations thereof in records.",
                                       "Well must contain A-H and must be within range 1-12.",
                                       "Check if measurement has minimum of 1.",
                                       "The functions all_complete() test for missing values or combinations thereof in records.",
                                       "Check if tick is minimum 0 (if it's below 0 give an error).",
                                       "The functions all_complete() test for missing values or combinations thereof in records.",
                                       "The functions all_complete() test for missing values or combinations thereof in records.",
                                       "Checks if column contains Background values.",
                                       "Check if column contains minimum 2 groups.",
                                       "Interval has to be numeric.",
                                       "The functions all_complete() test for missing values or combinations thereof in records.",
                                       "Injection has to be character.",
                                       "The functions all_complete() test for missing values or combinations thereof in records.",
                                       "The functions all_complete() test for missing values or combinations thereof in records.",
                                       "The functions all_complete() test for missing values or combinations thereof in records.",
                                       "The functions all_complete() test for missing values or combinations thereof in records.",
                                       "O2 should be between 70 and 80 mmHg",
                                       "pH has to be numeric",
                                       "The functions all_complete() test for missing values or combinations thereof in records.",
                                       "pH should be between 6.5 and 8",
                                       "The functions all_complete() test for missing values or combinations thereof in records.",
                                       "The functions all_complete() test for missing values or combinations thereof in records.",
                                       "The functions all_complete() test for missing values or combinations thereof in records.",
                                       "The functions all_complete() test for missing values or combinations thereof in records.",
                                       "The functions all_complete() test for missing values or combinations thereof in records."))
}
for (rule_index in 1:length(xf_plate_pr_raw_rules)){

  attr(xf_plate_pr_raw_rules[[rule_index]], "label") <- descriptions$rule_name[rule_index]
  attr(xf_plate_pr_raw_rules[[rule_index]], "description") <- descriptions$rule_desc[rule_index]
  
  #please note that the yaml file is assembled using attributes when calling the 
  #validator::export_yaml() function
}


#set assay_info yaml rules
xf_assay_info_rules <- {validate::validator(
  # the decriptions for the assay_info_yaml file have not been set yet
  
  # Rules for F0
  F0_miss_values = all_complete(F0),
  F0_min_1 = F0 >= 1, #>=1

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
  O2_0_mmHg_miss_values = all_complete(O2_0_mmHg)

)}

raw_yaml_path <- system.file("extdata", "rules_raw.yaml", package = "seahtrue")
assay_info_yaml_path <- system.file("extdata", "rules_assay_info.yaml", package = "seahtrue")

# export validation rules to yaml
create_raw_yaml <- function(xf_plate_pr_raw_rules, raw_yaml_path){
  validate::export_yaml(xf_plate_pr_raw_rules, raw_yaml_path)
}

create_assay_info_yaml <- function(xf_assay_info_rules, assay_info_yaml_path){
  validate::export_yaml(xf_assay_info_rules, assay_info_yaml_path)
}

create_raw_yaml(xf_plate_pr_raw_rules, raw_yaml_path)
create_assay_info_yaml(xf_assay_info_rules, assay_info_yaml_path)


