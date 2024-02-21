# Variables for baseline tables -----------------------------------------------

tabvars <- c(
  # demo
  "shf_indexyear_cat",
  "shf_sex",
  "shf_age",
  "shf_age_cat",

  # organizational
  "shf_location",
  "shf_followuphfunit",
  "shf_followuplocation_cat",

  # clinical factors and lab measurements
  "shf_durationhf",
  "shf_ef_cat",
  "shf_nyha",
  "shf_nyha_cat",
  "shf_bmi",
  "shf_bmi_cat",
  "shf_bpsys",
  "shf_bpsys_cat",
  "shf_bpdia",
  "shf_map",
  "shf_heartrate",
  "shf_heartrate_cat",
  "shf_gfrckdepi",
  "shf_gfrckdepi_cat",
  "shf_potassium",
  "shf_potassium_cat",
  "shf_hb",
  "shf_ntprobnp",
  "shf_ntprobnp_cat",

  # comorbs
  "shf_smoke_cat",
  "shf_sos_com_diabetes",
  "shf_sos_com_hypertension",
  "shf_sos_com_ihd",
  "sos_com_stroke",
  "shf_sos_com_af",
  "shf_anemia",
  "sos_com_valvular",
  "sos_com_liver",
  "sos_com_copd",
  "sos_com_cancer3y",
  "sos_com_charlsonci",
  "sos_com_charlsonci_cat",

  # treatments
  "shf_rasiarni",
  "shf_bbl",
  "shf_mra",
  "shf_sglt2",
  "shf_diuretic",
  "shf_nitrate",
  "shf_digoxin",
  "shf_anticoagulantia",
  "shf_asaantiplatelet",
  "shf_statin",
  "shf_device_cat",

  # socec
  "scb_famtype",
  "scb_child",
  "scb_education",
  "scb_dispincome_cat",
  "shf_qol",
  "shf_qol_cat",
  "shf_fatigue",
  "shf_outofbreath",
  "shf_mobility",
  "shf_hygiene",
  "shf_activities",
  "shf_pain"
)

# Variables for models (imputation, log, cox reg) ----------------------------

tabvars_not_in_mod <- c(
  "shf_age",
  "shf_nyha",
  "shf_revasc",
  "shf_bpsys",
  "shf_bpdia",
  "shf_map",
  "shf_map_cat",
  "shf_heartrate",
  "shf_gfrckdepi",
  "shf_hb",
  "shf_ntprobnp",
  "shf_potassium",
  "shf_potassium_cat",
  "shf_bmi",
  "sos_com_charlsonci",
  "sos_com_charlsonci_cat",
  "shf_qol",
  "shf_qol_cat",
  "shf_fatigue",
  "shf_outofbreath",
  "shf_mobility",
  "shf_hygiene",
  "shf_activities",
  "shf_pain",
  "shf_sglt2"
)

modvars <- tabvars[!(tabvars %in% tabvars_not_in_mod)]

stratavars <- c("shf_location")

outvars <- tibble(
  var = c("sos_out_death", "sos_out_deathcv", "sos_out_hosphf", "sos_out_hospany"),
  time = c("sos_outtime_death", "sos_outtime_death", "sos_outtime_hosphf", "sos_outtime_hospany"),
  shortname = c("Death", "CVD", "1st HFH", "1st Hospitalization"),
  name = c("All-cause death", "Cardiovascular death", "First heart failure hospitalization", "First all-cause hospitalization"),
  composite = c(F, F, F, F),
  rep = c(F, F, F, F),
  primary = c(T, F, F, F),
  order = c(1, 2, 3, 4)
) %>%
  arrange(order)
