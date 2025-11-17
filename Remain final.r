#REMAIN OBS1/2/3/4 - Enhanced Analysis with PMI Categories, Surgical Specialty Analysis, and T2MI Curves
# Set CRAN mirror (Netherlands)
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Load required packages
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("survival")) install.packages("survival")
if (!require("survminer")) install.packages("survminer")
if (!require("irr")) install.packages("irr")
if (!require("tableone")) install.packages("tableone")

library(tidyverse)
library(readxl)
library(dplyr)
library(survival)
library(survminer)
library(irr)
library(tableone)

# Print R and package versions
cat("\n=== SOFTWARE VERSIONS ===\n")
cat("R version:", R.version.string, "\n")
cat("RStudio version: Check Help > About RStudio\n")
cat("tableone version:", as.character(packageVersion("tableone")), "\n")
cat("survival version:", as.character(packageVersion("survival")), "\n")
cat("survminer version:", as.character(packageVersion("survminer")), "\n\n")

data <- read_csv2("Z:/REMAIN/Castor exports/REMAIN_ALLOBS_participant_data_csv_2025_06_02-10_55_02/REMAIN_export_20250602.csv")

#Coupling pseudonym_value and castor study nr
coupling <- read_excel("Z:/REMAIN/Mario Stark/Files gebruikt tijdens stage/Castor+pseudonym+date.xlsx")
demographics <- read_csv("Z:/REMAIN/Data export Datacapture/REMAIN - 2024-06-20 - demografie.csv")
medication <- read.csv("Z:/REMAIN/Data export Datacapture/REMAIN - 2024-06-20 - medicatie_voorschrift.csv")
admission <- read.csv("Z:/REMAIN/Data export Datacapture/REMAIN - 2024-06-20 - opname.csv")

#Exclude LOTx, centrale lijn, minor eg gastroscopy, tropo incorrect, recent cardiac surgery/intervention, organ donation and old/insufficient file
exclude_ids <- c(110014,110015,110083,110096,110279,110287,110300,110306,110308,110368,110376,110416,110470,110719,110930,110941,110951,110981,111009,110100,110148,110289,110318,110347,110349,110379,110567,110609,110694,110752,110908,111049,110162,110166,110210,110248,110398,110445,110487,110577,110614,110627,110635,110707,110743,110827,110864,110873,110891,110914,110995,110028,110033,110051,110067,110068,110099,110114,110117,110126,110131,110143,110160,110183,110187,110198,110205,110214,110221,110222,110225,110230,110238,110251,110261,110270,110330,110343,110357,110378,110382,110399,110408,110418,110431,110432,110443,110451,110455,110456,110458,110476,110491,110515,110521,110544,110550,110553,110573,110586,110588,110608,110644,110669,110686,110722,110726,110747,110753,110765,110774,110776,110783,110789,110841,110876,110878,110879,110884,110899,110911,110920,110922,110931,110933,110943,110948,110953,110970,110973,110983,110985,110987,110993,110998,111001,111005,111011,111012,111014,111026,111030,111032,111034,111038,111053,111055,111056,111064,111070,111075,111076,111082,111087,111097
)

cat("Total exclusion list:", length(exclude_ids), "\n")
cat("Unique exclusions:", length(unique(exclude_ids)), "\n")
cat("\nPatients before exclusion:", nrow(data), "\n")

data_included <- data %>%
  mutate(ID_numeric = as.numeric(sub("^OBS[1-4]-", "", `Participant Id`))) %>%
  filter(!ID_numeric %in% exclude_ids) %>%
  mutate(`Participant Id` = sub("^OBS[1-4]-","", `Participant Id`)) %>%
  select(-ID_numeric)

cat("Patients after exclusion:", nrow(data_included), "\n")
cat("Patients excluded:", nrow(data) - nrow(data_included), "\n")

remaining_excluded <- data_included %>%
  mutate(ID_check = as.numeric(`Participant Id`)) %>%
  filter(ID_check %in% exclude_ids) %>%
  nrow()
cat("Excluded IDs still in data (should be 0):", remaining_excluded, "\n\n")

# CRITICAL: Add Date (troponin reference date) from coupling file
# This Date will be used uniformly for ALL survival calculations in tables and KM curves
data_included <- data_included %>%
  mutate(Study_number = as.numeric(`Participant Id`)) %>%
  left_join(coupling %>% 
              mutate(Date = as.Date(as.character(Date))) %>%
              select(Study_number, Pseudonym, Date), 
            by = "Study_number") %>%
  left_join(demographics %>% select(pseudonym_value, leeftijd, gender_display),
            by = c("Pseudonym" = "pseudonym_value")) %>%
  left_join(admission %>% select(pseudonym_value, opname_bestemming),
            by = c("Pseudonym" = "pseudonym_value")) %>%
  # CRITICAL: Calculate in-hospital mortality based on discharge destination
  # "Overleden (zonder obductie)" and "Overleden (met obductie)" = died in hospital
  # All other discharge destinations = survived hospitalization
  mutate(
    death_in_hospital = if_else(
      opname_bestemming %in% c("Overleden (zonder obductie)", "Overleden (met obductie)"),
      1, 0, missing = 0
    )
  ) %>%
  mutate(
    RCRI_CAD = if_else(`history#Coronary Artery Disease` == 1, 1, 0, missing = 0),
    RCRI_Stroke = if_else(`history#Stroke / TIA` == 1, 1, 0, missing = 0),
    RCRI_CHF = if_else(`history#Chronic Heart Failure` == 1, 1, 0, missing = 0),
    RCRI_DM_insulin = if_else(`history#Diabetus Mellitus, insulin dependent` == 1, 1, 0, missing = 0),
    RCRI_CKD = if_else(`history#Chronic Kidney Disease` == 1, 1, 0, missing = 0),
    RCRI_high_risk_surg = if_else(surg_specialty %in% c("vascu", "neuro", "ge", "thora", "vascular"), 1, 0, missing = 0),
    RCRI_score = RCRI_CAD + RCRI_Stroke + RCRI_CHF + RCRI_DM_insulin + RCRI_CKD + RCRI_high_risk_surg
  )

#Fuse history variables across observers for each included patient
history_vars <- c("history#Coronary Artery Disease","history#Myocardial Infarction","history#Pheripheral Artery Disease","history#Stroke / TIA", "history#Chronic Heart Failure","history#Atrial Fibrilation","history#Moderate/Severe Valvular Disease","history#Diabetus Mellitus, non-insulin","history#Diabetus Mellitus, insulin dependent","history#Chronic Kidney Disease","history#Hypertension","history#Chronic Obstructive Pulmonary Disease")

#Fused history over OBS1/2 and 3/4 but separated extra/cardiac/T2MI
data_final <- data_included%>%
  group_by(`Participant Id`) %>%
  mutate(across(all_of(history_vars), ~if_else(any(. == 1, na.rm = TRUE), 1,0)),
         `history#None/Unknown` = if_else(any(`history#None/Unknown` == 1, na.rm = TRUE ), 1, 0)
  ) %>%
  mutate(
    `history#None/Unknown` = if_else(
      rowSums(across(all_of(history_vars)) == 1) > 0,
      0,
      `history#None/Unknown`
    ),
    no_cv_history = if_else(rowSums(across(all_of(history_vars)) == 1) == 0, 1,0)
  )%>%
  ungroup()%>%
  mutate(Observer_Group = if_else(`Site Abbreviation` %in% c("OBS1","OBS2"), "OBS12", "OBS34"))

#Compare OBS12 and OBS34
obs12 <- data_final %>% filter(Observer_Group == "OBS12")
obs34 <- data_final %>% filter(Observer_Group == "OBS34")                

comparison <- inner_join(
  obs12 %>% select(`Participant Id`,
                   extra_12 = cause_extra_car_yes,
                   cardiac_12 = Cause_cardiac_yes,
                   T2MI_12 = cause_T2MI),
  obs34 %>% select(`Participant Id`,
                   extra_34 = cause_extra_car_yes,
                   cardiac_34 = Cause_cardiac_yes,
                   T2MI_34 = cause_T2MI),
  by = "Participant Id"
)

# Identify agreed patients for K-M analysis
comparison_pmi <- comparison %>%
  mutate(
    PMI_type_12 = case_when(
      cardiac_12 == 1 | T2MI_12 == 1 | (T2MI_12 == 0 & extra_12 == 0 & cardiac_12 == 0) ~ "Cardiac",
      extra_12 == 1 ~ "Noncardiac",
      TRUE ~ NA_character_
    ),
    PMI_type_34 = case_when(
      cardiac_34 == 1 | T2MI_34 == 1 | (T2MI_34 == 0 & extra_34 == 0 & cardiac_34 == 0) ~ "Cardiac",
      extra_34 == 1 ~ "Noncardiac",
      TRUE ~ NA_character_
    ),
    agreed = PMI_type_12 == PMI_type_34 & !is.na(PMI_type_12) & !is.na(PMI_type_34)
  )

agreed_patients <- comparison_pmi %>%
  filter(agreed) %>%
  select(`Participant Id`, PMI_type = PMI_type_12)

# Create disagreed patients dataset
disagreed_patients <- comparison_pmi %>%
  filter(!agreed & !is.na(PMI_type_12) & !is.na(PMI_type_34)) %>%
  select(`Participant Id`, PMI_type_12, PMI_type_34)

# ========== INTER-RATER AGREEMENT (COHEN'S KAPPA) ==========

cat("\n=== INTER-RATER AGREEMENT: COHEN'S KAPPA ===\n")

cat("\nTotal patients with both OBS12 and OBS34 assessments:", nrow(comparison_pmi), "\n")
cat("Patients with NA in OBS12:", sum(is.na(comparison_pmi$PMI_type_12)), "\n")
cat("Patients with NA in OBS34:", sum(is.na(comparison_pmi$PMI_type_34)), "\n")
cat("Patients with NA in either OBS12 or OBS34:", 
    sum(is.na(comparison_pmi$PMI_type_12) | is.na(comparison_pmi$PMI_type_34)), "\n")

kappa_data <- comparison_pmi %>%
  filter(!is.na(PMI_type_12) & !is.na(PMI_type_34)) %>%
  select(PMI_type_12, PMI_type_34)

cat("Patients included in kappa analysis (both non-NA):", nrow(kappa_data), "\n")

kappa_result <- kappa2(kappa_data, weight = "unweighted")

n <- nrow(kappa_data)
po <- sum(kappa_data$PMI_type_12 == kappa_data$PMI_type_34) / n
pe <- (sum(kappa_data$PMI_type_12 == "Cardiac") * sum(kappa_data$PMI_type_34 == "Cardiac") + 
       sum(kappa_data$PMI_type_12 == "Noncardiac") * sum(kappa_data$PMI_type_34 == "Noncardiac")) / n^2
kappa <- (po - pe) / (1 - pe)
se_kappa <- sqrt(po * (1 - po) / (n * (1 - pe)^2))

cat("\nCohen's Kappa for PMI Classification (Cardiac vs Noncardiac):\n")
cat("Kappa:", round(kappa_result$value, 3), "\n")
cat("95% CI: [", round(kappa - 1.96*se_kappa, 3), ", ", round(kappa + 1.96*se_kappa, 3), "]\n", sep="")
cat("p-value:", format.pval(kappa_result$p.value, digits = 3), "\n")

cat("\nAgreement table:\n")
agreement_table <- table(OBS12 = kappa_data$PMI_type_12, OBS34 = kappa_data$PMI_type_34)
print(agreement_table)

pct_agreement <- sum(kappa_data$PMI_type_12 == kappa_data$PMI_type_34) / nrow(kappa_data) * 100
cat("\nPercentage agreement:", round(pct_agreement, 1), "%\n")

# ========== PREPARE DATASETS ==========
# NOTE: obs12_with_pmi and agreed_survival inherit Date, death_30d, death_365d from data_included
# This ensures uniform survival calculations across tables and KM curves

# OBS12 with PMI classification - one row per patient
obs12_with_pmi <- obs12 %>%
  mutate(
    PMI_type = case_when(
      Cause_cardiac_yes == 1 | cause_T2MI == 1 | 
        (cause_T2MI == 0 & cause_extra_car_yes == 0 & Cause_cardiac_yes == 0) ~ "Cardiac",
      cause_extra_car_yes == 1 ~ "Noncardiac",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(PMI_type)) %>%
  distinct(Pseudonym, .keep_all = TRUE)

# Create agreed_with_pmi dataset
agreed_with_pmi <- obs12 %>%
  inner_join(agreed_patients, by = "Participant Id") %>%
  filter(!is.na(PMI_type))

# Agreed survival - one row per patient
agreed_survival <- obs12 %>%
  inner_join(agreed_patients, by = "Participant Id") %>%
  filter(!is.na(PMI_type)) %>%
  distinct(Pseudonym, .keep_all = TRUE)

# ========== MEDICATION ANALYSIS ==========

cat("\n\n=== MEDICATION ANALYSIS ===\n\n")

# Target medications for analysis
target_medications <- c("CLOPIDOGREL", "ACETYLSALICYLZUUR", "DABIGATRANETEXILAAT", "TICAGRELOR")

# Filter for target medications and join with OBS12 data
medication_filtered <- medication %>%
  filter(code5_ATC_display_nl %in% target_medications) %>%
  distinct(pseudonym_value, code5_ATC_display_nl)

# Join medication data with obs12_with_pmi
obs12_with_medication <- obs12_with_pmi %>%
  left_join(medication_filtered, by = c("Pseudonym" = "pseudonym_value"))

# Count patients on each medication by PMI type
cat("--- Medication Usage by PMI Type (OBS12) ---\n\n")

medication_summary <- obs12_with_medication %>%
  filter(!is.na(code5_ATC_display_nl)) %>%
  group_by(PMI_type, code5_ATC_display_nl) %>%
  summarise(N = n(), .groups = 'drop') %>%
  pivot_wider(names_from = PMI_type, values_from = N, values_fill = 0)

# Add Total column (handle case where Cardiac or Noncardiac column might not exist)
if("Cardiac" %in% names(medication_summary) && "Noncardiac" %in% names(medication_summary)) {
  medication_summary <- medication_summary %>%
    mutate(Total = Cardiac + Noncardiac) %>%
    arrange(desc(Total))
} else if("Cardiac" %in% names(medication_summary)) {
  medication_summary <- medication_summary %>%
    mutate(Noncardiac = 0, Total = Cardiac) %>%
    arrange(desc(Total))
} else if("Noncardiac" %in% names(medication_summary)) {
  medication_summary <- medication_summary %>%
    mutate(Cardiac = 0, Total = Noncardiac) %>%
    arrange(desc(Total))
}

print(medication_summary)

# Overall medication usage by PMI type
cat("\n--- Overall Medication Usage Summary (OBS12) ---\n\n")

overall_med_summary <- obs12_with_pmi %>%
  left_join(medication_filtered %>%
              filter(code5_ATC_display_nl %in% target_medications) %>%
              mutate(on_medication = 1) %>%
              distinct(pseudonym_value, on_medication),
            by = c("Pseudonym" = "pseudonym_value")) %>%
  mutate(on_medication = replace_na(on_medication, 0)) %>%
  group_by(PMI_type) %>%
  summarise(
    Total_patients = n(),
    On_target_medications = sum(on_medication),
    Percentage = round(On_target_medications / Total_patients * 100, 1)
  )

print(overall_med_summary)

# Count unique patients on each medication
cat("\n--- Patients on Each Medication (OBS12) ---\n\n")

for(med in target_medications) {
  cardiac_count <- obs12_with_medication %>%
    filter(PMI_type == "Cardiac" & code5_ATC_display_nl == med) %>%
    nrow()

  noncardiac_count <- obs12_with_medication %>%
    filter(PMI_type == "Noncardiac" & code5_ATC_display_nl == med) %>%
    nrow()

  total_cardiac <- sum(obs12_with_pmi$PMI_type == "Cardiac")
  total_noncardiac <- sum(obs12_with_pmi$PMI_type == "Noncardiac")

  cat(med, ":\n")
  cat("  Cardiac PMI:", cardiac_count, "/", total_cardiac,
      "(", round(cardiac_count/total_cardiac*100, 1), "%)\n")
  cat("  Noncardiac PMI:", noncardiac_count, "/", total_noncardiac,
      "(", round(noncardiac_count/total_noncardiac*100, 1), "%)\n\n")
}

# ========== ECG ANALYSIS ==========

cat("\n\n=== ECG ANALYSIS ===\n\n")

# Note: ECG variable is expected to be in the main dataset
# If ECG is not present, check variable names in the data

# Add ECG analysis to obs12_with_pmi
# Assuming ECG variable exists in the data with values 0 and 1
if("ECG" %in% names(obs12_with_pmi)) {
  cat("--- ECG Distribution by PMI Type (OBS12) ---\n\n")

  ecg_summary <- obs12_with_pmi %>%
    filter(!is.na(ECG)) %>%
    group_by(PMI_type, ECG) %>%
    summarise(N = n(), .groups = 'drop') %>%
    pivot_wider(names_from = PMI_type, values_from = N, values_fill = 0)

  # Add Total column (handle case where Cardiac or Noncardiac column might not exist)
  if("Cardiac" %in% names(ecg_summary) && "Noncardiac" %in% names(ecg_summary)) {
    ecg_summary <- ecg_summary %>%
      mutate(Total = Cardiac + Noncardiac)
  } else if("Cardiac" %in% names(ecg_summary)) {
    ecg_summary <- ecg_summary %>%
      mutate(Noncardiac = 0, Total = Cardiac)
  } else if("Noncardiac" %in% names(ecg_summary)) {
    ecg_summary <- ecg_summary %>%
      mutate(Cardiac = 0, Total = Noncardiac)
  }

  ecg_summary <- ecg_summary %>%
    mutate(ECG = factor(ECG, levels = c(0, 1), labels = c("No ECG (0)", "ECG Done (1)")))

  print(ecg_summary)

  # ECG percentages by PMI type
  cat("\n--- ECG Percentages by PMI Type (OBS12) ---\n\n")

  ecg_pct_summary <- obs12_with_pmi %>%
    filter(!is.na(ECG)) %>%
    group_by(PMI_type) %>%
    summarise(
      Total = n(),
      ECG_Done = sum(ECG == 1, na.rm = TRUE),
      ECG_Not_Done = sum(ECG == 0, na.rm = TRUE),
      Pct_ECG_Done = round(ECG_Done / Total * 100, 1),
      Pct_ECG_Not_Done = round(ECG_Not_Done / Total * 100, 1)
    )

  print(ecg_pct_summary)

  # Chi-square test for ECG vs PMI type
  cat("\n--- Statistical Test: ECG by PMI Type ---\n\n")

  ecg_table <- table(obs12_with_pmi$PMI_type, obs12_with_pmi$ECG)
  if(nrow(ecg_table) > 1 && ncol(ecg_table) > 1) {
    chisq_result <- chisq.test(ecg_table)
    cat("Chi-square test:\n")
    cat("  X-squared =", round(chisq_result$statistic, 3), "\n")
    cat("  p-value =", format.pval(chisq_result$p.value, digits = 3), "\n\n")
    print(ecg_table)
  }

} else {
  cat("WARNING: ECG variable not found in obs12_with_pmi dataset.\n")
  cat("Available variables containing 'ECG':\n")
  ecg_vars <- grep("ECG|ecg|Ecg", names(obs12_with_pmi), value = TRUE, ignore.case = TRUE)
  if(length(ecg_vars) > 0) {
    print(ecg_vars)
  } else {
    cat("  No ECG-related variables found.\n")
    cat("\nPlease check the variable name in the dataset.\n")
    cat("First few variable names:\n")
    print(head(names(obs12_with_pmi), 20))
  }
}

# ========== PMI CATEGORY BREAKDOWN - OBS12 ==========

cat("\n\n=== PMI CATEGORY BREAKDOWN - OBS12 ===\n\n")

# Create PMI category variable
obs12_with_pmi <- obs12_with_pmi %>%
  mutate(
    PMI_category = case_when(
      cause_extra_car_yes == 1 ~ cause_extra_car,
      Cause_cardiac_yes == 1 ~ cause_cardiac,
      cause_T2MI == 1 ~ "T2MI_with_cause",
      cause_T2MI == 0 & cause_extra_car_yes == 0 & Cause_cardiac_yes == 0 ~ "T2MI_without_cause",
      TRUE ~ "Unknown"
    )
  )

cat("--- Overview of PMI Categories (OBS12) ---\n")
obs12_category_overview <- obs12_with_pmi %>%
  count(PMI_category, sort = TRUE) %>%
  mutate(Percentage = round(n/sum(n)*100, 1))
print(obs12_category_overview, n = Inf)

cat("\n--- Noncardiac Causes (OBS12) ---\n")
obs12_with_pmi %>%
  filter(cause_extra_car_yes == 1) %>%
  count(cause_extra_car, sort = TRUE) %>%
  mutate(Percentage = round(n/sum(n)*100, 1)) %>%
  print(n = Inf)

cat("\n--- Cardiac Causes (OBS12) ---\n")
obs12_with_pmi %>%
  filter(Cause_cardiac_yes == 1) %>%
  count(cause_cardiac, sort = TRUE) %>%
  mutate(Percentage = round(n/sum(n)*100, 1)) %>%
  print(n = Inf)

cat("\n--- T2MI Categories (OBS12) ---\n")
obs12_t2mi <- obs12_with_pmi %>%
  mutate(T2MI_status = case_when(
    cause_T2MI == 1 ~ "T2MI with cause",
    cause_T2MI == 0 & cause_extra_car_yes == 0 & Cause_cardiac_yes == 0 ~ "T2MI without cause",
    TRUE ~ "Not T2MI"
  )) %>%
  count(T2MI_status, sort = TRUE) %>%
  mutate(Percentage = round(n/sum(n)*100, 1))
print(obs12_t2mi)

# ========== PMI CATEGORY BREAKDOWN - AGREED CASES ==========

cat("\n\n=== PMI CATEGORY BREAKDOWN - AGREED CASES ===\n\n")

# Create PMI category variable for agreed cases
agreed_survival <- agreed_survival %>%
  mutate(
    PMI_category = case_when(
      cause_extra_car_yes == 1 ~ cause_extra_car,
      Cause_cardiac_yes == 1 ~ cause_cardiac,
      cause_T2MI == 1 ~ "T2MI_with_cause",
      cause_T2MI == 0 & cause_extra_car_yes == 0 & Cause_cardiac_yes == 0 ~ "T2MI_without_cause",
      TRUE ~ "Unknown"
    )
  )

cat("--- Overview of PMI Categories (Agreed Cases) ---\n")
agreed_category_overview <- agreed_survival %>%
  count(PMI_category, sort = TRUE) %>%
  mutate(Percentage = round(n/sum(n)*100, 1))
print(agreed_category_overview, n = Inf)

cat("\n--- Noncardiac Causes (Agreed Cases) ---\n")
agreed_survival %>%
  filter(cause_extra_car_yes == 1) %>%
  count(cause_extra_car, sort = TRUE) %>%
  mutate(Percentage = round(n/sum(n)*100, 1)) %>%
  print(n = Inf)

cat("\n--- Cardiac Causes (Agreed Cases) ---\n")
agreed_survival %>%
  filter(Cause_cardiac_yes == 1) %>%
  count(cause_cardiac, sort = TRUE) %>%
  mutate(Percentage = round(n/sum(n)*100, 1)) %>%
  print(n = Inf)

cat("\n--- T2MI Categories (Agreed Cases) ---\n")
agreed_t2mi <- agreed_survival %>%
  mutate(T2MI_status = case_when(
    cause_T2MI == 1 ~ "T2MI with cause",
    cause_T2MI == 0 & cause_extra_car_yes == 0 & Cause_cardiac_yes == 0 ~ "T2MI without cause",
    TRUE ~ "Not T2MI"
  )) %>%
  count(T2MI_status, sort = TRUE) %>%
  mutate(Percentage = round(n/sum(n)*100, 1))
print(agreed_t2mi)

# ========== COMPARISON TABLE: OBS12 vs AGREED PMI CATEGORIES ==========

cat("\n\n=== COMPARISON: PMI CATEGORIES IN OBS12 vs AGREED CASES ===\n\n")

# Create comparison table
comparison_categories <- full_join(
  obs12_category_overview %>% 
    select(PMI_category, n_OBS12 = n, Pct_OBS12 = Percentage),
  agreed_category_overview %>% 
    select(PMI_category, n_Agreed = n, Pct_Agreed = Percentage),
  by = "PMI_category"
) %>%
  mutate(
    n_OBS12 = replace_na(n_OBS12, 0),
    n_Agreed = replace_na(n_Agreed, 0),
    Pct_OBS12 = replace_na(Pct_OBS12, 0),
    Pct_Agreed = replace_na(Pct_Agreed, 0)
  ) %>%
  arrange(desc(n_OBS12))

cat("PMI Categories: OBS12 vs Agreed Cases\n")
print(comparison_categories, n = Inf)

# ========== NEW: SURGICAL SPECIALTY ANALYSIS WITH P-VALUES ==========

cat("\n\n=== SURGICAL SPECIALTY ANALYSIS: CARDIAC vs NONCARDIAC (OBS12) ===\n\n")

# Function to compute p-value for cardiac vs noncardiac by specialty
compute_specialty_pvalue <- function(data, specialty_col = "surg_specialty", pmi_col = "PMI_type") {
  # Get unique specialties
  specialties <- unique(data[[specialty_col]])
  specialties <- specialties[!is.na(specialties)]
  
  results <- data.frame()
  
  for(spec in specialties) {
    spec_data <- data %>% filter(!!sym(specialty_col) == spec)
    
    # Count cardiac and noncardiac
    n_cardiac <- sum(spec_data[[pmi_col]] == "Cardiac", na.rm = TRUE)
    n_noncardiac <- sum(spec_data[[pmi_col]] == "Noncardiac", na.rm = TRUE)
    n_total <- n_cardiac + n_noncardiac
    
    # Compute p-value using chi-square test if sufficient numbers
    if(n_total >= 5 && n_cardiac > 0 && n_noncardiac > 0) {
      # Create contingency table
      spec_table <- table(spec_data[[pmi_col]])
      
      # If we have both cardiac and noncardiac, perform chi-square test
      # Compare to overall proportions
      overall_prop_cardiac <- sum(data[[pmi_col]] == "Cardiac", na.rm = TRUE) / 
                              (sum(data[[pmi_col]] == "Cardiac", na.rm = TRUE) + 
                               sum(data[[pmi_col]] == "Noncardiac", na.rm = TRUE))
      
      # Expected counts
      expected_cardiac <- n_total * overall_prop_cardiac
      expected_noncardiac <- n_total * (1 - overall_prop_cardiac)
      
      # Chi-square statistic
      chi_sq <- ((n_cardiac - expected_cardiac)^2 / expected_cardiac) + 
                ((n_noncardiac - expected_noncardiac)^2 / expected_noncardiac)
      p_value <- pchisq(chi_sq, df = 1, lower.tail = FALSE)
      
      # Fisher's exact test as alternative for small samples
      if(n_total < 20) {
        # Create 2x2 contingency table
        other_cardiac <- sum(data[[pmi_col]] == "Cardiac", na.rm = TRUE) - n_cardiac
        other_noncardiac <- sum(data[[pmi_col]] == "Noncardiac", na.rm = TRUE) - n_noncardiac
        
        fisher_table <- matrix(c(n_cardiac, n_noncardiac, 
                                 other_cardiac, other_noncardiac), 
                               nrow = 2, byrow = TRUE)
        fisher_test <- fisher.test(fisher_table)
        p_value_fisher <- fisher_test$p.value
      } else {
        p_value_fisher <- NA
      }
    } else {
      p_value <- NA
      p_value_fisher <- NA
    }
    
    pct_cardiac <- round(n_cardiac / n_total * 100, 1)
    pct_noncardiac <- round(n_noncardiac / n_total * 100, 1)
    
    results <- rbind(results, data.frame(
      Specialty = spec,
      N_Total = n_total,
      N_Cardiac = n_cardiac,
      Pct_Cardiac = pct_cardiac,
      N_Noncardiac = n_noncardiac,
      Pct_Noncardiac = pct_noncardiac,
      P_value_ChiSq = ifelse(is.na(p_value), NA, format.pval(p_value, digits = 3)),
      P_value_Fisher = ifelse(is.na(p_value_fisher), NA, format.pval(p_value_fisher, digits = 3))
    ))
  }
  
  return(results)
}

# OBS12 analysis by surgical specialty
cat("--- Cardiac vs Noncardiac Distribution by Surgical Specialty (OBS12) ---\n")
obs12_specialty_results <- compute_specialty_pvalue(obs12_with_pmi)
print(obs12_specialty_results)

cat("\n--- Cardiac vs Noncardiac Distribution by Surgical Specialty (Agreed Cases) ---\n")
agreed_specialty_results <- compute_specialty_pvalue(agreed_survival)
print(agreed_specialty_results)

# ========== SHAPIRO-WILK NORMALITY TESTS ==========

cat("\n\n=== SHAPIRO-WILK NORMALITY TESTS ===\n\n")

# Prepare dataset for all included patients (unique by pseudonym)
all_patients <- data_included %>%
  distinct(Pseudonym, .keep_all = TRUE)

# Test continuous variables
continuous_vars <- c("leeftijd", "RCRI_score")

cat("Testing normality for continuous variables in ALL INCLUDED PATIENTS:\n\n")
normality_results_all <- data.frame()

for(var in continuous_vars) {
  if(var %in% names(all_patients)) {
    test_data <- all_patients[[var]][!is.na(all_patients[[var]])]
    if(length(test_data) > 3 & length(test_data) < 5000) {
      shapiro_test <- shapiro.test(test_data)
      cat(var, ":\n")
      cat("  W =", round(shapiro_test$statistic, 4), "\n")
      cat("  p-value =", format.pval(shapiro_test$p.value, digits = 3), "\n")
      cat("  Distribution:", ifelse(shapiro_test$p.value < 0.05, "Non-normal", "Normal"), "\n\n")
      
      normality_results_all <- rbind(normality_results_all, 
                                     data.frame(Variable = var, 
                                               W = shapiro_test$statistic,
                                               p_value = shapiro_test$p.value,
                                               Distribution = ifelse(shapiro_test$p.value < 0.05, "Non-normal", "Normal")))
    }
  }
}

nonnormal_vars <- normality_results_all %>%
  filter(Distribution == "Non-normal") %>%
  pull(Variable)

if(length(nonnormal_vars) > 0) {
  cat("Variables to be treated as non-normal in tables:", paste(nonnormal_vars, collapse = ", "), "\n\n")
} else {
  cat("All tested variables appear normally distributed\n\n")
}

# ========== BASELINE CHARACTERISTICS TABLES ==========

cat("\n\n=== BASELINE CHARACTERISTICS TABLES ===\n\n")

# Define variables for baseline tables (NO mortality - that's for KM curves)
vars_baseline <- c("leeftijd", "gender_display", "emergency_surg", "surg_specialty",
                   "history#Coronary Artery Disease", "history#Myocardial Infarction",
                   "history#Pheripheral Artery Disease", "history#Stroke / TIA",
                   "history#Chronic Heart Failure", "history#Atrial Fibrilation",
                   "history#Moderate/Severe Valvular Disease", "history#Diabetus Mellitus, non-insulin",
                   "history#Diabetus Mellitus, insulin dependent", "history#Chronic Kidney Disease",
                   "history#Hypertension", "history#Chronic Obstructive Pulmonary Disease",
                   "RCRI_score")

cat_vars_baseline <- c("gender_display", "emergency_surg", "surg_specialty",
                       "history#Coronary Artery Disease", "history#Myocardial Infarction",
                       "history#Pheripheral Artery Disease", "history#Stroke / TIA",
                       "history#Chronic Heart Failure", "history#Atrial Fibrilation",
                       "history#Moderate/Severe Valvular Disease", "history#Diabetus Mellitus, non-insulin",
                       "history#Diabetus Mellitus, insulin dependent", "history#Chronic Kidney Disease",
                       "history#Hypertension", "history#Chronic Obstructive Pulmonary Disease")

# Table 1: All included patients
cat("\n=== TABLE 1: ALL INCLUDED PATIENTS (BASELINE CHARACTERISTICS) ===\n")
table1_all <- CreateTableOne(vars = vars_baseline,
                              data = all_patients,
                              factorVars = cat_vars_baseline,
                              test = FALSE)
print(table1_all, nonnormal = nonnormal_vars, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# Table 2: OBS12 - Cardiac vs Noncardiac (baseline characteristics)
cat("\n\n=== TABLE 2: OBS12 - CARDIAC vs NONCARDIAC PMI (BASELINE CHARACTERISTICS) ===\n")
table2_obs12 <- CreateTableOne(vars = vars_baseline,
                                strata = "PMI_type",
                                data = obs12_with_pmi %>% mutate(PMI_type = factor(PMI_type, levels = c("Cardiac", "Noncardiac"))),
                                factorVars = cat_vars_baseline,
                                test = TRUE)
print(table2_obs12, nonnormal = nonnormal_vars, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# **NEW: Add surgical specialty p-values for OBS12**
cat("\n\n--- SURGICAL SPECIALTY P-VALUES FOR OBS12 ---\n")
cat("P-values comparing Cardiac vs Noncardiac proportions within each specialty:\n\n")
obs12_specialty_pval_table <- obs12_specialty_results %>%
  select(Specialty, N_Total, N_Cardiac, N_Noncardiac, P_value_ChiSq, P_value_Fisher) %>%
  rename(
    "Total N" = N_Total,
    "Cardiac N" = N_Cardiac,
    "Noncardiac N" = N_Noncardiac,
    "P (Chi-square)" = P_value_ChiSq,
    "P (Fisher)" = P_value_Fisher
  )
print(obs12_specialty_pval_table, row.names = FALSE)
cat("\nNote: Fisher's exact test is used when N < 20; Chi-square test otherwise.\n")

# Table 3: Agreed cases - Cardiac vs Noncardiac (baseline characteristics)
cat("\n\n=== TABLE 3: AGREED CASES - CARDIAC vs NONCARDIAC PMI (BASELINE CHARACTERISTICS) ===\n")
table3_agreed <- CreateTableOne(vars = vars_baseline,
                                 strata = "PMI_type",
                                 data = agreed_survival %>% mutate(PMI_type = factor(PMI_type, levels = c("Cardiac", "Noncardiac"))),
                                 factorVars = cat_vars_baseline,
                                 test = TRUE)
print(table3_agreed, nonnormal = nonnormal_vars, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# **NEW: Add surgical specialty p-values for Agreed**
cat("\n\n--- SURGICAL SPECIALTY P-VALUES FOR AGREED CASES ---\n")
cat("P-values comparing Cardiac vs Noncardiac proportions within each specialty:\n\n")
agreed_specialty_pval_table <- agreed_specialty_results %>%
  select(Specialty, N_Total, N_Cardiac, N_Noncardiac, P_value_ChiSq, P_value_Fisher) %>%
  rename(
    "Total N" = N_Total,
    "Cardiac N" = N_Cardiac,
    "Noncardiac N" = N_Noncardiac,
    "P (Chi-square)" = P_value_ChiSq,
    "P (Fisher)" = P_value_Fisher
  )
print(agreed_specialty_pval_table, row.names = FALSE)
cat("\nNote: Fisher's exact test is used when N < 20; Chi-square test otherwise.\n")

# ========== IN-HOSPITAL MORTALITY ANALYSIS ==========

cat("\n\n=== IN-HOSPITAL MORTALITY ANALYSIS ===\n\n")
cat("NOTE: In-hospital mortality based on discharge destination:\n")
cat("  - 'Overleden (zonder obductie)' and 'Overleden (met obductie)' = in-hospital death\n")
cat("  - All other discharge destinations = survived hospitalization\n\n")

# ========== MORTALITY BY PMI TYPE (CARDIAC VS NONCARDIAC) ==========

cat("--- In-Hospital Mortality: Cardiac vs Noncardiac (OBS12) ---\n\n")
obs12_mortality_pmi <- obs12_with_pmi %>%
  group_by(PMI_type) %>%
  summarise(
    N = n(),
    Deaths = sum(death_in_hospital == 1, na.rm = TRUE),
    Mortality_pct = round(Deaths / N * 100, 1),
    .groups = 'drop'
  )
print(obs12_mortality_pmi)

# Chi-square test for mortality difference
cat("\nChi-square test (Cardiac vs Noncardiac in OBS12):\n")
mort_table_obs12 <- table(obs12_with_pmi$PMI_type, obs12_with_pmi$death_in_hospital)
if(nrow(mort_table_obs12) > 1 && ncol(mort_table_obs12) > 1) {
  chisq_mort_obs12 <- chisq.test(mort_table_obs12)
  cat("  X-squared =", round(chisq_mort_obs12$statistic, 3), "\n")
  cat("  p-value =", format.pval(chisq_mort_obs12$p.value, digits = 3), "\n\n")
}

# Logistic regression: adjusted for age and emergency surgery
cat("\n--- Logistic Regression Analysis (OBS12) ---\n\n")

# Prepare data for logistic regression
obs12_logistic <- obs12_with_pmi %>%
  mutate(
    PMI_type_numeric = if_else(PMI_type == "Noncardiac", 1, 0),  # Noncardiac vs Cardiac (reference)
    age_continuous = leeftijd,
    emergency_binary = emergency_surg
  ) %>%
  filter(!is.na(age_continuous) & !is.na(emergency_binary) & !is.na(death_in_hospital))

# Unadjusted model
model_unadjusted_obs12 <- glm(death_in_hospital ~ PMI_type_numeric,
                               data = obs12_logistic,
                               family = binomial(link = "logit"))

# Adjusted model (age + emergency surgery)
model_adjusted_obs12 <- glm(death_in_hospital ~ PMI_type_numeric + age_continuous + emergency_binary,
                             data = obs12_logistic,
                             family = binomial(link = "logit"))

# Extract results
or_unadj <- exp(coef(model_unadjusted_obs12)["PMI_type_numeric"])
ci_unadj <- exp(confint(model_unadjusted_obs12)["PMI_type_numeric",])
p_unadj <- summary(model_unadjusted_obs12)$coefficients["PMI_type_numeric", "Pr(>|z|)"]

or_adj <- exp(coef(model_adjusted_obs12)["PMI_type_numeric"])
ci_adj <- exp(confint(model_adjusted_obs12)["PMI_type_numeric",])
p_adj <- summary(model_adjusted_obs12)$coefficients["PMI_type_numeric", "Pr(>|z|)"]

cat("Odds Ratios for In-Hospital Mortality (Noncardiac vs Cardiac):\n\n")
cat("Unadjusted OR:", round(or_unadj, 2),
    "(95% CI:", round(ci_unadj[1], 2), "-", round(ci_unadj[2], 2), ")\n")
cat("  p-value:", format.pval(p_unadj, digits = 3), "\n\n")

cat("Adjusted OR (age + emergency surgery):", round(or_adj, 2),
    "(95% CI:", round(ci_adj[1], 2), "-", round(ci_adj[2], 2), ")\n")
cat("  p-value:", format.pval(p_adj, digits = 3), "\n\n")

cat("Interpretation: OR > 1 indicates higher mortality in Noncardiac PMI\n")
cat("                OR < 1 indicates lower mortality in Noncardiac PMI\n\n")

# ========== MORTALITY: PMI TYPE VS OVERALL POPULATION ==========

cat("\n--- In-Hospital Mortality: Each PMI Type vs Overall Population (OBS12) ---\n\n")

# Calculate overall mortality rate
overall_mortality_rate <- sum(obs12_with_pmi$death_in_hospital == 1, na.rm = TRUE) / nrow(obs12_with_pmi)
cat("Overall in-hospital mortality rate (OBS12):", round(overall_mortality_rate * 100, 1), "%\n\n")

# Cardiac PMI vs Overall
cat("CARDIAC PMI vs Overall Population:\n")
obs12_cardiac_vs_overall <- obs12_with_pmi %>%
  mutate(
    is_cardiac = if_else(PMI_type == "Cardiac", 1, 0),
    age_continuous = leeftijd,
    emergency_binary = emergency_surg
  ) %>%
  filter(!is.na(age_continuous) & !is.na(emergency_binary) & !is.na(death_in_hospital))

# Unadjusted
model_cardiac_unadj <- glm(death_in_hospital ~ is_cardiac,
                           data = obs12_cardiac_vs_overall,
                           family = binomial(link = "logit"))

# Adjusted
model_cardiac_adj <- glm(death_in_hospital ~ is_cardiac + age_continuous + emergency_binary,
                         data = obs12_cardiac_vs_overall,
                         family = binomial(link = "logit"))

or_cardiac_unadj <- exp(coef(model_cardiac_unadj)["is_cardiac"])
ci_cardiac_unadj <- exp(confint(model_cardiac_unadj)["is_cardiac",])
p_cardiac_unadj <- summary(model_cardiac_unadj)$coefficients["is_cardiac", "Pr(>|z|)"]

or_cardiac_adj <- exp(coef(model_cardiac_adj)["is_cardiac"])
ci_cardiac_adj <- exp(confint(model_cardiac_adj)["is_cardiac",])
p_cardiac_adj <- summary(model_cardiac_adj)$coefficients["is_cardiac", "Pr(>|z|)"]

cat("  Unadjusted OR:", round(or_cardiac_unadj, 2),
    "(95% CI:", round(ci_cardiac_unadj[1], 2), "-", round(ci_cardiac_unadj[2], 2), ")\n")
cat("  p-value:", format.pval(p_cardiac_unadj, digits = 3), "\n\n")

cat("  Adjusted OR (age + emergency surgery):", round(or_cardiac_adj, 2),
    "(95% CI:", round(ci_cardiac_adj[1], 2), "-", round(ci_cardiac_adj[2], 2), ")\n")
cat("  p-value:", format.pval(p_cardiac_adj, digits = 3), "\n\n")

# Noncardiac PMI vs Overall
cat("NONCARDIAC PMI vs Overall Population:\n")
obs12_noncardiac_vs_overall <- obs12_with_pmi %>%
  mutate(
    is_noncardiac = if_else(PMI_type == "Noncardiac", 1, 0),
    age_continuous = leeftijd,
    emergency_binary = emergency_surg
  ) %>%
  filter(!is.na(age_continuous) & !is.na(emergency_binary) & !is.na(death_in_hospital))

# Unadjusted
model_noncardiac_unadj <- glm(death_in_hospital ~ is_noncardiac,
                              data = obs12_noncardiac_vs_overall,
                              family = binomial(link = "logit"))

# Adjusted
model_noncardiac_adj <- glm(death_in_hospital ~ is_noncardiac + age_continuous + emergency_binary,
                            data = obs12_noncardiac_vs_overall,
                            family = binomial(link = "logit"))

or_noncardiac_unadj <- exp(coef(model_noncardiac_unadj)["is_noncardiac"])
ci_noncardiac_unadj <- exp(confint(model_noncardiac_unadj)["is_noncardiac",])
p_noncardiac_unadj <- summary(model_noncardiac_unadj)$coefficients["is_noncardiac", "Pr(>|z|)"]

or_noncardiac_adj <- exp(coef(model_noncardiac_adj)["is_noncardiac"])
ci_noncardiac_adj <- exp(confint(model_noncardiac_adj)["is_noncardiac",])
p_noncardiac_adj <- summary(model_noncardiac_adj)$coefficients["is_noncardiac", "Pr(>|z|)"]

cat("  Unadjusted OR:", round(or_noncardiac_unadj, 2),
    "(95% CI:", round(ci_noncardiac_unadj[1], 2), "-", round(ci_noncardiac_unadj[2], 2), ")\n")
cat("  p-value:", format.pval(p_noncardiac_unadj, digits = 3), "\n\n")

cat("  Adjusted OR (age + emergency surgery):", round(or_noncardiac_adj, 2),
    "(95% CI:", round(ci_noncardiac_adj[1], 2), "-", round(ci_noncardiac_adj[2], 2), ")\n")
cat("  p-value:", format.pval(p_noncardiac_adj, digits = 3), "\n\n")

cat("Interpretation:\n")
cat("  OR > 1: PMI type has higher mortality than overall population\n")
cat("  OR < 1: PMI type has lower mortality than overall population\n")
cat("  OR = 1: PMI type has same mortality as overall population\n\n")

# ========== RCRI AND IN-HOSPITAL MORTALITY ANALYSIS ==========

cat("\n\n========== RCRI AND IN-HOSPITAL MORTALITY ANALYSIS ==========\n\n")

# 1. Overall OBS12 Population
cat("--- RCRI vs In-Hospital Mortality (Overall OBS12 Population) ---\n\n")

rcri_mortality_overall <- obs12_with_pmi %>%
  group_by(RCRI_score) %>%
  summarise(
    N = n(),
    Deaths = sum(death_in_hospital == 1, na.rm = TRUE),
    Mortality_pct = round(Deaths / N * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(RCRI_score)

cat("Mortality by RCRI Score:\n")
print(rcri_mortality_overall, n = Inf)

# Trend test using logistic regression (RCRI as continuous predictor)
rcri_logistic_overall <- obs12_with_pmi %>%
  filter(!is.na(RCRI_score) & !is.na(death_in_hospital))

if(nrow(rcri_logistic_overall) > 0) {
  model_rcri_overall <- glm(death_in_hospital ~ RCRI_score,
                            data = rcri_logistic_overall,
                            family = binomial(link = "logit"))

  or_rcri_overall <- exp(coef(model_rcri_overall)["RCRI_score"])
  ci_rcri_overall <- exp(confint(model_rcri_overall)["RCRI_score",])
  p_rcri_overall <- summary(model_rcri_overall)$coefficients["RCRI_score", "Pr(>|z|)"]

  cat("\nOdds Ratio per 1-point increase in RCRI:\n")
  cat("  OR:", round(or_rcri_overall, 2),
      "(95% CI:", round(ci_rcri_overall[1], 2), "-", round(ci_rcri_overall[2], 2), ")\n")
  cat("  p-value:", format.pval(p_rcri_overall, digits = 3), "\n")

  # Cochran-Armitage trend test (using proportional increase)
  cat("\nInterpretation: OR > 1 indicates increasing mortality with higher RCRI\n\n")
}

# 2. Cardiac PMI Group
cat("\n--- RCRI vs In-Hospital Mortality (Cardiac PMI Group) ---\n\n")

cardiac_group <- obs12_with_pmi %>%
  filter(PMI_type == "Cardiac")

rcri_mortality_cardiac <- cardiac_group %>%
  group_by(RCRI_score) %>%
  summarise(
    N = n(),
    Deaths = sum(death_in_hospital == 1, na.rm = TRUE),
    Mortality_pct = round(Deaths / N * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(RCRI_score)

cat("Mortality by RCRI Score (Cardiac PMI):\n")
print(rcri_mortality_cardiac, n = Inf)

rcri_logistic_cardiac <- cardiac_group %>%
  filter(!is.na(RCRI_score) & !is.na(death_in_hospital))

if(nrow(rcri_logistic_cardiac) > 0) {
  model_rcri_cardiac <- glm(death_in_hospital ~ RCRI_score,
                            data = rcri_logistic_cardiac,
                            family = binomial(link = "logit"))

  or_rcri_cardiac <- exp(coef(model_rcri_cardiac)["RCRI_score"])
  ci_rcri_cardiac <- exp(confint(model_rcri_cardiac)["RCRI_score",])
  p_rcri_cardiac <- summary(model_rcri_cardiac)$coefficients["RCRI_score", "Pr(>|z|)"]

  cat("\nOdds Ratio per 1-point increase in RCRI (Cardiac PMI):\n")
  cat("  OR:", round(or_rcri_cardiac, 2),
      "(95% CI:", round(ci_rcri_cardiac[1], 2), "-", round(ci_rcri_cardiac[2], 2), ")\n")
  cat("  p-value:", format.pval(p_rcri_cardiac, digits = 3), "\n")
  cat("\nInterpretation: OR > 1 indicates increasing mortality with higher RCRI in Cardiac PMI\n\n")
}

# 3. Noncardiac (Extra-cardiac) PMI Group
cat("\n--- RCRI vs In-Hospital Mortality (Noncardiac/Extra-cardiac PMI Group) ---\n\n")

noncardiac_group <- obs12_with_pmi %>%
  filter(PMI_type == "Noncardiac")

rcri_mortality_noncardiac <- noncardiac_group %>%
  group_by(RCRI_score) %>%
  summarise(
    N = n(),
    Deaths = sum(death_in_hospital == 1, na.rm = TRUE),
    Mortality_pct = round(Deaths / N * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(RCRI_score)

cat("Mortality by RCRI Score (Noncardiac PMI):\n")
print(rcri_mortality_noncardiac, n = Inf)

rcri_logistic_noncardiac <- noncardiac_group %>%
  filter(!is.na(RCRI_score) & !is.na(death_in_hospital))

if(nrow(rcri_logistic_noncardiac) > 0) {
  model_rcri_noncardiac <- glm(death_in_hospital ~ RCRI_score,
                                data = rcri_logistic_noncardiac,
                                family = binomial(link = "logit"))

  or_rcri_noncardiac <- exp(coef(model_rcri_noncardiac)["RCRI_score"])
  ci_rcri_noncardiac <- exp(confint(model_rcri_noncardiac)["RCRI_score",])
  p_rcri_noncardiac <- summary(model_rcri_noncardiac)$coefficients["RCRI_score", "Pr(>|z|)"]

  cat("\nOdds Ratio per 1-point increase in RCRI (Noncardiac PMI):\n")
  cat("  OR:", round(or_rcri_noncardiac, 2),
      "(95% CI:", round(ci_rcri_noncardiac[1], 2), "-", round(ci_rcri_noncardiac[2], 2), ")\n")
  cat("  p-value:", format.pval(p_rcri_noncardiac, digits = 3), "\n")
  cat("\nInterpretation: OR > 1 indicates increasing mortality with higher RCRI in Noncardiac PMI\n\n")
}

# Summary comparison
cat("\n--- Summary: RCRI Effect on In-Hospital Mortality ---\n\n")
cat("This analysis examines whether in-hospital mortality increases with each 1-point increase in RCRI score.\n")
cat("An OR > 1 with p < 0.05 indicates a significant increase in mortality risk per RCRI point.\n\n")

cat("\n--- In-Hospital Mortality: Cardiac vs Noncardiac (Agreed Cases) ---\n\n")
agreed_mortality_pmi <- agreed_survival %>%
  group_by(PMI_type) %>%
  summarise(
    N = n(),
    Deaths = sum(death_in_hospital == 1, na.rm = TRUE),
    Mortality_pct = round(Deaths / N * 100, 1),
    .groups = 'drop'
  )
print(agreed_mortality_pmi)

# Chi-square test for mortality difference
cat("\nChi-square test (Cardiac vs Noncardiac in Agreed Cases):\n")
mort_table_agreed <- table(agreed_survival$PMI_type, agreed_survival$death_in_hospital)
if(nrow(mort_table_agreed) > 1 && ncol(mort_table_agreed) > 1) {
  chisq_mort_agreed <- chisq.test(mort_table_agreed)
  cat("  X-squared =", round(chisq_mort_agreed$statistic, 3), "\n")
  cat("  p-value =", format.pval(chisq_mort_agreed$p.value, digits = 3), "\n\n")
}

# Logistic regression: adjusted for age and emergency surgery
cat("\n--- Logistic Regression Analysis (Agreed Cases) ---\n\n")

# Prepare data for logistic regression
agreed_logistic <- agreed_survival %>%
  mutate(
    PMI_type_numeric = if_else(PMI_type == "Noncardiac", 1, 0),  # Noncardiac vs Cardiac (reference)
    age_continuous = leeftijd,
    emergency_binary = emergency_surg
  ) %>%
  filter(!is.na(age_continuous) & !is.na(emergency_binary) & !is.na(death_in_hospital))

# Unadjusted model
model_unadjusted_agreed <- glm(death_in_hospital ~ PMI_type_numeric,
                                data = agreed_logistic,
                                family = binomial(link = "logit"))

# Adjusted model (age + emergency surgery)
model_adjusted_agreed <- glm(death_in_hospital ~ PMI_type_numeric + age_continuous + emergency_binary,
                              data = agreed_logistic,
                              family = binomial(link = "logit"))

# Extract results
or_unadj_agreed <- exp(coef(model_unadjusted_agreed)["PMI_type_numeric"])
ci_unadj_agreed <- exp(confint(model_unadjusted_agreed)["PMI_type_numeric",])
p_unadj_agreed <- summary(model_unadjusted_agreed)$coefficients["PMI_type_numeric", "Pr(>|z|)"]

or_adj_agreed <- exp(coef(model_adjusted_agreed)["PMI_type_numeric"])
ci_adj_agreed <- exp(confint(model_adjusted_agreed)["PMI_type_numeric",])
p_adj_agreed <- summary(model_adjusted_agreed)$coefficients["PMI_type_numeric", "Pr(>|z|)"]

cat("Odds Ratios for In-Hospital Mortality (Noncardiac vs Cardiac):\n\n")
cat("Unadjusted OR:", round(or_unadj_agreed, 2),
    "(95% CI:", round(ci_unadj_agreed[1], 2), "-", round(ci_unadj_agreed[2], 2), ")\n")
cat("  p-value:", format.pval(p_unadj_agreed, digits = 3), "\n\n")

cat("Adjusted OR (age + emergency surgery):", round(or_adj_agreed, 2),
    "(95% CI:", round(ci_adj_agreed[1], 2), "-", round(ci_adj_agreed[2], 2), ")\n")
cat("  p-value:", format.pval(p_adj_agreed, digits = 3), "\n\n")

cat("Interpretation: OR > 1 indicates higher mortality in Noncardiac PMI\n")
cat("                OR < 1 indicates lower mortality in Noncardiac PMI\n\n")

# ========== MORTALITY BY PMI CATEGORY ==========

cat("\n\n--- In-Hospital Mortality by PMI Category (OBS12) ---\n\n")
obs12_mortality_category <- obs12_with_pmi %>%
  group_by(PMI_category) %>%
  summarise(
    N = n(),
    Deaths = sum(death_in_hospital == 1, na.rm = TRUE),
    Mortality_pct = round(Deaths / N * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(N))
print(obs12_mortality_category, n = Inf)

cat("\n--- In-Hospital Mortality by PMI Category (Agreed Cases) ---\n\n")
agreed_mortality_category <- agreed_survival %>%
  group_by(PMI_category) %>%
  summarise(
    N = n(),
    Deaths = sum(death_in_hospital == 1, na.rm = TRUE),
    Mortality_pct = round(Deaths / N * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(N))
print(agreed_mortality_category, n = Inf)

# ========== T2MI MORTALITY (WITH VS WITHOUT PRECIPITANT) ==========

cat("\n\n--- In-Hospital Mortality: T2MI with vs without Precipitant (OBS12) ---\n\n")
obs12_t2mi_mortality <- obs12_with_pmi %>%
  filter(PMI_category %in% c("T2MI_with_cause", "T2MI_without_cause")) %>%
  mutate(T2MI_group = if_else(PMI_category == "T2MI_with_cause",
                               "T2MI with precipitant",
                               "T2MI without precipitant")) %>%
  group_by(T2MI_group) %>%
  summarise(
    N = n(),
    Deaths = sum(death_in_hospital == 1, na.rm = TRUE),
    Mortality_pct = round(Deaths / N * 100, 1),
    .groups = 'drop'
  )
print(obs12_t2mi_mortality)

if(nrow(obs12_t2mi_mortality) == 2) {
  cat("\nFisher's exact test (T2MI with vs without precipitant in OBS12):\n")
  t2mi_table_obs12 <- obs12_with_pmi %>%
    filter(PMI_category %in% c("T2MI_with_cause", "T2MI_without_cause")) %>%
    mutate(T2MI_group = if_else(PMI_category == "T2MI_with_cause",
                                 "T2MI with precipitant",
                                 "T2MI without precipitant"))
  fisher_t2mi_obs12 <- fisher.test(table(t2mi_table_obs12$T2MI_group, t2mi_table_obs12$death_in_hospital))
  cat("  p-value =", format.pval(fisher_t2mi_obs12$p.value, digits = 3), "\n\n")
}

cat("\n--- In-Hospital Mortality: T2MI with vs without Precipitant (Agreed Cases) ---\n\n")
agreed_t2mi_mortality <- agreed_survival %>%
  filter(PMI_category %in% c("T2MI_with_cause", "T2MI_without_cause")) %>%
  mutate(T2MI_group = if_else(PMI_category == "T2MI_with_cause",
                               "T2MI with precipitant",
                               "T2MI without precipitant")) %>%
  group_by(T2MI_group) %>%
  summarise(
    N = n(),
    Deaths = sum(death_in_hospital == 1, na.rm = TRUE),
    Mortality_pct = round(Deaths / N * 100, 1),
    .groups = 'drop'
  )
print(agreed_t2mi_mortality)

if(nrow(agreed_t2mi_mortality) == 2) {
  cat("\nFisher's exact test (T2MI with vs without precipitant in Agreed Cases):\n")
  t2mi_table_agreed <- agreed_survival %>%
    filter(PMI_category %in% c("T2MI_with_cause", "T2MI_without_cause")) %>%
    mutate(T2MI_group = if_else(PMI_category == "T2MI_with_cause",
                                 "T2MI with precipitant",
                                 "T2MI without precipitant"))
  fisher_t2mi_agreed <- fisher.test(table(t2mi_table_agreed$T2MI_group, t2mi_table_agreed$death_in_hospital))
  cat("  p-value =", format.pval(fisher_t2mi_agreed$p.value, digits = 3), "\n\n")
}

# ========== AGREED VS DISAGREED MORTALITY ANALYSIS ==========

cat("\n\n=== IN-HOSPITAL MORTALITY: AGREED vs DISAGREED CASES ===\n\n")

# Create agreed and disagreed datasets with mortality
agreed_mortality_data <- obs12 %>%
  inner_join(agreed_patients, by = "Participant Id") %>%
  distinct(Pseudonym, .keep_all = TRUE) %>%
  mutate(Agreement_status = "Agreed")

disagreed_mortality_data <- obs12 %>%
  inner_join(disagreed_patients, by = "Participant Id") %>%
  distinct(Pseudonym, .keep_all = TRUE) %>%
  mutate(Agreement_status = "Disagreed")

# Combine for comparison
agreement_comparison <- bind_rows(agreed_mortality_data, disagreed_mortality_data)

# Summary table
cat("--- In-Hospital Mortality by Agreement Status ---\n\n")
agreement_mort_summary <- agreement_comparison %>%
  group_by(Agreement_status) %>%
  summarise(
    N = n(),
    Deaths = sum(death_in_hospital == 1, na.rm = TRUE),
    Mortality_pct = round(Deaths / N * 100, 1),
    .groups = 'drop'
  )
print(agreement_mort_summary)

# Chi-square test
cat("\nChi-square test (Agreed vs Disagreed):\n")
agreement_mort_table <- table(agreement_comparison$Agreement_status, agreement_comparison$death_in_hospital)
if(nrow(agreement_mort_table) > 1 && ncol(agreement_mort_table) > 1) {
  chisq_agreement <- chisq.test(agreement_mort_table)
  cat("  X-squared =", round(chisq_agreement$statistic, 3), "\n")
  cat("  p-value =", format.pval(chisq_agreement$p.value, digits = 3), "\n\n")
  print(agreement_mort_table)
}

# Logistic regression: disagreed vs agreed (adjusted for age and emergency surgery)
cat("\n\n--- Logistic Regression: Disagreed vs Agreed (adjusted) ---\n\n")

agreement_logistic <- agreement_comparison %>%
  mutate(
    disagreed_binary = if_else(Agreement_status == "Disagreed", 1, 0),  # Disagreed vs Agreed (reference)
    age_continuous = leeftijd,
    emergency_binary = emergency_surg
  ) %>%
  filter(!is.na(age_continuous) & !is.na(emergency_binary) & !is.na(death_in_hospital))

# Unadjusted model
model_unadj_agreement <- glm(death_in_hospital ~ disagreed_binary,
                              data = agreement_logistic,
                              family = binomial(link = "logit"))

# Adjusted model
model_adj_agreement <- glm(death_in_hospital ~ disagreed_binary + age_continuous + emergency_binary,
                            data = agreement_logistic,
                            family = binomial(link = "logit"))

# Extract results
or_unadj_agr <- exp(coef(model_unadj_agreement)["disagreed_binary"])
ci_unadj_agr <- exp(confint(model_unadj_agreement)["disagreed_binary",])
p_unadj_agr <- summary(model_unadj_agreement)$coefficients["disagreed_binary", "Pr(>|z|)"]

or_adj_agr <- exp(coef(model_adj_agreement)["disagreed_binary"])
ci_adj_agr <- exp(confint(model_adj_agreement)["disagreed_binary",])
p_adj_agr <- summary(model_adj_agreement)$coefficients["disagreed_binary", "Pr(>|z|)"]

cat("Odds Ratios for In-Hospital Mortality (Disagreed vs Agreed):\n\n")
cat("Unadjusted OR:", round(or_unadj_agr, 2),
    "(95% CI:", round(ci_unadj_agr[1], 2), "-", round(ci_unadj_agr[2], 2), ")\n")
cat("  p-value:", format.pval(p_unadj_agr, digits = 3), "\n\n")

cat("Adjusted OR (age + emergency surgery):", round(or_adj_agr, 2),
    "(95% CI:", round(ci_adj_agr[1], 2), "-", round(ci_adj_agr[2], 2), ")\n")
cat("  p-value:", format.pval(p_adj_agr, digits = 3), "\n\n")

cat("Interpretation: OR > 1 indicates higher mortality in Disagreed cases\n")
cat("                OR < 1 indicates lower mortality in Disagreed cases\n\n")

cat("Number of agreed cases:", nrow(agreed_mortality_data), "\n")
cat("Number of disagreed cases:", nrow(disagreed_mortality_data), "\n")

cat("\n\n=== ANALYSIS COMPLETE ===\n")
cat("\n✓ Inter-rater agreement (Cohen's Kappa)\n")
cat("✓ PMI category breakdown (OBS12 vs Agreed)\n")
cat("✓ Surgical specialty analysis (Chi-square/Fisher p-values)\n")
cat("✓ Medication analysis (CLOPIDOGREL, ACETYLSALICYLZUUR, DABIGATRANETEXILAAT, TICAGRELOR)\n")
cat("✓ ECG analysis by PMI type\n")
cat("✓ Baseline characteristics tables (NO mortality - baseline only)\n")
cat("✓ In-hospital mortality analysis:\n")
cat("  - Cardiac vs Noncardiac (OBS12 & Agreed) with Chi-square tests\n")
cat("  - Logistic regression with unadjusted and adjusted OR (age + emergency surgery)\n")
cat("  - All PMI categories mortality breakdown\n")
cat("  - T2MI with vs without precipitant (OBS12 & Agreed) with Fisher's exact test\n")
cat("  - Agreed vs Disagreed cases mortality comparison\n")
cat("\nKey points:\n")
cat("  • Baseline tables: Chi-square test for group comparisons\n")
cat("  • Mortality analysis:\n")
cat("    - Chi-square test for Cardiac vs Noncardiac comparisons\n")
cat("    - Logistic regression for adjusted odds ratios (age + emergency surgery)\n")
cat("    - Fisher's exact test for T2MI comparisons\n")
cat("  • NO Cox regression, hazard ratios, or Kaplan-Meier curves\n")
cat("  • In-hospital mortality based on discharge destination (opname_bestemming)\n")
cat("\nMORTALITY DATA SOURCE:\n")
cat("  • In-hospital mortality from discharge destination field:\n")
cat("    - 'Overleden (zonder obductie)' = deceased without autopsy\n")
cat("    - 'Overleden (met obductie)' = deceased with autopsy\n")
cat("    - All other discharge destinations = survived hospitalization\n")
cat("  • No time-to-event data available (only binary outcome: died vs survived)\n")
cat("  • Out-of-hospital mortality NOT captured\n")
cat("\nADJUSTMENT VARIABLES:\n")
cat("  • Logistic regression models adjusted for:\n")
cat("    - Age (continuous variable)\n")
cat("    - Emergency surgery (binary: elective vs emergency)\n")
cat("  • Reference group: Cardiac PMI\n")
cat("  • OR > 1: Higher mortality in Noncardiac PMI\n")
cat("  • OR < 1: Lower mortality in Noncardiac PMI\n")
