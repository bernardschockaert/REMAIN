#REMAIN OBS1/2/3/4 - MASTER CODE: Enhanced Analysis with PMI Categories, Surgical Specialty Analysis, T2MI Curves, and Postoperative Vitals
# Integrated Sessions:
# 1. In-hospital mortality by PMI aetiology
# 2. Postoperative vitals analysis (hypotension & tachycardia)
# 3. Expanded exclusion list with old/insufficient data entries
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
postoperativevitals <- read.csv("Z:/REMAIN/Data export Datacapture/REMAIN - metingen_postoperatief_20241010.csv")
opname <- read_csv("Z:/REMAIN/Data export Datacapture/REMAIN - 2024-06-20 - opname")
lab <- read_csv("Z:/REMAIN/Data export Datacapture/REMAIN - 2024-06-20 - lab")

#Exclude LOTx, centrale lijn, minor eg gastroscopy, tropo incorrect, recent cardiac surgery/intervention, organ donation and old/insufficient file
exclude_ids <- c(110014,110015,110083,110096,110279,110287,110300,110306,110308,110368,110376,110416,110470,110719,110930,110941,110951,110981,111009,110100,110148,110159,110289,110318,110347,110349,110352,110379,110567,110609,110694,110752,110908,111049,110162,110166,110210,110248,110398,110445,110487,110577,110614,110627,110635,110707,110743,110827,110864,110873,110891,110914,110995,110028,110033,110051,110067,110068,110099,110114,110117,110126,110131,110143,110160,110183,110187,110198,110205,110214,110221,110222,110225,110230,110238,110251,110261,110270,110330,110343,110357,110378,110382,110399,110408,110418,110431,110432,110443,110451,110455,110456,110458,110476,110491,110515,110521,110544,110550,110553,110573,110586,110588,110608,110644,110669,110686,110722,110726,110747,110753,110765,110774,110776,110783,110789,110841,110876,110878,110879,110884,110899,110911,110920,110922,110931,110933,110943,110948,110953,110970,110973,110983,110985,110987,110993,110998,111001,111005,111011,111012,111014,111026,111030,111032,111034,111038,111053,111055,111056,111064,111070,111075,111076,111082,111087,111097
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
  left_join(demographics %>% select(pseudonym_value, leeftijd, gender_display, deceasedDateTime), 
            by = c("Pseudonym" = "pseudonym_value")) %>%
  mutate(
    deceasedDateTime = as.Date(as.POSIXct(deceasedDateTime, format = "%Y-%m-%d %H:%M:%S")),
    time_to_death = as.numeric(difftime(deceasedDateTime, Date, units = "days"))
  ) %>%
  # CRITICAL: Calculate survival outcomes ONCE using Date from coupling
  # These variables will be inherited by obs12_with_pmi and agreed_survival
  mutate(
    death_30d = if_else(!is.na(time_to_death) & time_to_death <= 30, 1, 0),
    survival_time_30d = if_else(is.na(time_to_death), 30, pmin(time_to_death, 30)),
    death_365d = if_else(!is.na(time_to_death) & time_to_death <= 365, 1, 0),
    survival_time_365d = if_else(is.na(time_to_death), 365, pmin(time_to_death, 365))
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

# ========== POSTOPERATIVE VITALS PROCESSING ==========

cat("\n\n=== PROCESSING POSTOPERATIVE VITALS ===\n\n")

# Load hemodynamics data from postoperative vitals
hemodynamics <- postoperativevitals %>%
  filter(code_display_original %in% c("ABP", "NIBP", "HR", "SpO2")) %>%
  mutate(valueQuantity_value = as.numeric(valueQuantity_value))  # Ensure numeric values

# Extract vital signs - Prioritize ABP over NIBP for blood pressure
abp_data <- hemodynamics %>%
  filter(code_display_original == "ABP") %>%
  group_by(pseudonym_value, effectiveDateTime) %>%
  summarise(MAP = mean(valueQuantity_value, na.rm = TRUE), source = "ABP", .groups = "drop")

nibp_data <- hemodynamics %>%
  filter(code_display_original == "NIBP") %>%
  group_by(pseudonym_value, effectiveDateTime) %>%
  summarise(MAP = mean(valueQuantity_value, na.rm = TRUE), source = "NIBP", .groups = "drop")

# Combine: use ABP when available, NIBP only when ABP missing
bp_data <- abp_data %>%
  bind_rows(nibp_data) %>%
  group_by(pseudonym_value, effectiveDateTime) %>%
  arrange(desc(source)) %>%  # ABP comes before NIBP alphabetically
  slice(1) %>%  # Take first (ABP if present, else NIBP)
  ungroup() %>%
  select(pseudonym_value, effectiveDateTime, MAP)

# Extract heart rate and SpO2
hr_data <- hemodynamics %>%
  filter(code_display_original == "HR") %>%
  select(pseudonym_value, effectiveDateTime, HR = valueQuantity_value)

spo2_data <- hemodynamics %>%
  filter(code_display_original == "SpO2") %>%
  select(pseudonym_value, effectiveDateTime, SpO2 = valueQuantity_value)

# Combine all vital signs
vital_signs <- bp_data %>%
  full_join(hr_data, by = c("pseudonym_value", "effectiveDateTime")) %>%
  full_join(spo2_data, by = c("pseudonym_value", "effectiveDateTime"))

# Patient-level summary with TWA hypotension and threshold violations
patient_hemodynamics <- vital_signs %>%
  group_by(pseudonym_value) %>%
  summarise(
    any_MAP_below_65 = any(MAP < 65, na.rm = TRUE),
    any_HR_above_120 = any(HR > 120, na.rm = TRUE),
    any_SpO2_below_90 = any(SpO2 < 90, na.rm = TRUE),
    TWA_hypotension = sum(pmax(65 - MAP[!is.na(MAP)], 0), na.rm = TRUE),  # Time-weighted average below 65
    .groups = "drop"
  )

# Add in-hospital mortality variable based on time_to_death
data_included <- data_included %>%
  mutate(death_in_hospital = if_else(!is.na(time_to_death) & time_to_death <= 30, 1, 0))

# Merge vital signs with obs12_with_pmi and agreed_survival
obs12_with_pmi <- obs12_with_pmi %>%
  left_join(patient_hemodynamics, by = c("Pseudonym" = "pseudonym_value"))

agreed_survival <- agreed_survival %>%
  left_join(patient_hemodynamics, by = c("Pseudonym" = "pseudonym_value"))

# Add in-hospital mortality to both datasets
obs12_with_pmi <- obs12_with_pmi %>%
  mutate(death_in_hospital = if_else(!is.na(time_to_death) & time_to_death <= 30, 1, 0))

agreed_survival <- agreed_survival %>%
  mutate(death_in_hospital = if_else(!is.na(time_to_death) & time_to_death <= 30, 1, 0))

# Summary statistics for vital sign thresholds
vitals_summary <- patient_hemodynamics %>%
  summarise(
    N = n(),
    MAP_below_65 = sum(any_MAP_below_65, na.rm = TRUE) / N * 100,
    HR_above_120 = sum(any_HR_above_120, na.rm = TRUE) / N * 100,
    SpO2_below_90 = sum(any_SpO2_below_90, na.rm = TRUE) / N * 100
  )

cat("Postoperative vital sign threshold violations (% of total cohort):\n")
cat("  MAP < 65 mmHg:", round(vitals_summary$MAP_below_65, 1), "%\n")
cat("  HR > 120 bpm:", round(vitals_summary$HR_above_120, 1), "%\n")
cat("  SpO2 < 90%:", round(vitals_summary$SpO2_below_90, 1), "%\n\n")

# ========== FIRST hsTnT VALUE AND LOCATION COUPLING ==========

cat("\n\n=== COUPLING FIRST hsTnT VALUES WITH LOCATION ===\n\n")

# Get first hsTnT measurement per patient
first_hstnt <- lab %>%
  filter(!is.na(valueQuantity_value)) %>%
  arrange(pseudonym_value, collection_collectedDateTime) %>%
  group_by(pseudonym_value) %>%
  slice(1) %>%
  ungroup() %>%
  select(pseudonym_value,
         first_hstnt_value = valueQuantity_value,
         first_hstnt_datetime = collection_collectedDateTime)

# Get admission location information
admission_location <- opname %>%
  select(pseudonym_value, specialty_display_original, opnamedeel_afdeling)

# Couple first hsTnT with location and merge with obs12_with_pmi
hstnt_location <- first_hstnt %>%
  left_join(admission_location, by = "pseudonym_value")

obs12_with_pmi <- obs12_with_pmi %>%
  left_join(hstnt_location, by = c("Pseudonym" = "pseudonym_value"))

agreed_survival <- agreed_survival %>%
  left_join(hstnt_location, by = c("Pseudonym" = "pseudonym_value"))

cat("First hsTnT values coupled with location data\n")
cat("Patients with hsTnT data:", sum(!is.na(obs12_with_pmi$first_hstnt_value)), "\n")
cat("Patients with specialty data:", sum(!is.na(obs12_with_pmi$specialty_display_original)), "\n")
cat("Patients with ward data:", sum(!is.na(obs12_with_pmi$opnamedeel_afdeling)), "\n\n")

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

vars_for_table <- c("leeftijd", "gender_display", "emergency_surg", "surg_specialty",
                    "history#Coronary Artery Disease", "history#Myocardial Infarction",
                    "history#Pheripheral Artery Disease", "history#Stroke / TIA",
                    "history#Chronic Heart Failure", "history#Atrial Fibrilation",
                    "history#Moderate/Severe Valvular Disease", "history#Diabetus Mellitus, non-insulin",
                    "history#Diabetus Mellitus, insulin dependent", "history#Chronic Kidney Disease",
                    "history#Hypertension", "history#Chronic Obstructive Pulmonary Disease",
                    "RCRI_score", "death_30d", "death_365d")

cat_vars <- c("gender_display", "emergency_surg", "surg_specialty",
              "history#Coronary Artery Disease", "history#Myocardial Infarction",
              "history#Pheripheral Artery Disease", "history#Stroke / TIA",
              "history#Chronic Heart Failure", "history#Atrial Fibrilation",
              "history#Moderate/Severe Valvular Disease", "history#Diabetus Mellitus, non-insulin",
              "history#Diabetus Mellitus, insulin dependent", "history#Chronic Kidney Disease",
              "history#Hypertension", "history#Chronic Obstructive Pulmonary Disease",
              "death_30d", "death_365d")

# Table 1: All included patients
cat("\n=== TABLE 1: ALL INCLUDED PATIENTS ===\n")
table1_all <- CreateTableOne(vars = vars_for_table,
                              data = all_patients,
                              factorVars = cat_vars,
                              test = FALSE)
print(table1_all, nonnormal = nonnormal_vars, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# Table 2: OBS12 - Cardiac vs Noncardiac
cat("\n\n=== TABLE 2: OBS12 - CARDIAC vs NONCARDIAC PMI ===\n")
obs12_with_pmi_grouped <- obs12_with_pmi %>%
  mutate(PMI_type = factor(PMI_type, levels = c("Cardiac", "Noncardiac")))

table2_obs12 <- CreateTableOne(vars = vars_for_table,
                                strata = "PMI_type",
                                data = obs12_with_pmi_grouped,
                                factorVars = cat_vars,
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

# Table 3: Agreed cases - Cardiac vs Noncardiac
cat("\n\n=== TABLE 3: AGREED CASES - CARDIAC vs NONCARDIAC PMI ===\n")
agreed_survival_grouped <- agreed_survival %>%
  mutate(PMI_type = factor(PMI_type, levels = c("Cardiac", "Noncardiac")))

table3_agreed <- CreateTableOne(vars = vars_for_table,
                                 strata = "PMI_type",
                                 data = agreed_survival_grouped,
                                 factorVars = cat_vars,
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

# ========== COX REGRESSION MODELS ==========

cat("\n\n=== COX REGRESSION MODELS - OBS12 ===\n")

obs12_cox <- obs12_with_pmi %>%
  mutate(
    RCRI_high = if_else(RCRI_score > 1, 1, 0),
    emergency_surg_factor = factor(emergency_surg, levels = c(0, 1), labels = c("Elective", "Emergency")),
    RCRI_high_factor = factor(RCRI_high, levels = c(0, 1), labels = c("RCRI≤1", "RCRI>1")),
    sex_factor = factor(gender_display, levels = c("Male", "Female")),
    age_continuous = leeftijd
  )

cox_30d_obs12_unadj <- coxph(Surv(survival_time_30d, death_30d) ~ PMI_type, data = obs12_cox)
cox_30d_obs12_full <- coxph(Surv(survival_time_30d, death_30d) ~ PMI_type + age_continuous + sex_factor + emergency_surg_factor + RCRI_high_factor, data = obs12_cox)
cox_365d_obs12_unadj <- coxph(Surv(survival_time_365d, death_365d) ~ PMI_type, data = obs12_cox)
cox_365d_obs12_full <- coxph(Surv(survival_time_365d, death_365d) ~ PMI_type + age_continuous + sex_factor + emergency_surg_factor + RCRI_high_factor, data = obs12_cox)

# Extract adjusted HRs for display on plots
hr_30d_obs12_adj <- exp(coef(cox_30d_obs12_full)["PMI_typeNoncardiac"])
ci_30d_obs12_adj <- exp(confint(cox_30d_obs12_full)["PMI_typeNoncardiac",])
p_30d_obs12_adj <- summary(cox_30d_obs12_full)$coefficients["PMI_typeNoncardiac", "Pr(>|z|)"]

hr_365d_obs12_adj <- exp(coef(cox_365d_obs12_full)["PMI_typeNoncardiac"])
ci_365d_obs12_adj <- exp(confint(cox_365d_obs12_full)["PMI_typeNoncardiac",])
p_365d_obs12_adj <- summary(cox_365d_obs12_full)$coefficients["PMI_typeNoncardiac", "Pr(>|z|)"]

cat("\n=== COX REGRESSION MODELS - AGREED CASES ===\n")

agreed_survival_cox <- agreed_survival %>%
  mutate(
    RCRI_high = if_else(RCRI_score > 1, 1, 0),
    emergency_surg_factor = factor(emergency_surg, levels = c(0, 1), labels = c("Elective", "Emergency")),
    RCRI_high_factor = factor(RCRI_high, levels = c(0, 1), labels = c("RCRI≤1", "RCRI>1")),
    sex_factor = factor(gender_display, levels = c("Male", "Female")),
    age_continuous = leeftijd
  )

cox_30d_agreed_unadj <- coxph(Surv(survival_time_30d, death_30d) ~ PMI_type, data = agreed_survival_cox)
cox_30d_agreed_full <- coxph(Surv(survival_time_30d, death_30d) ~ PMI_type + age_continuous + sex_factor + emergency_surg_factor + RCRI_high_factor, data = agreed_survival_cox)
cox_365d_agreed_unadj <- coxph(Surv(survival_time_365d, death_365d) ~ PMI_type, data = agreed_survival_cox)
cox_365d_agreed_full <- coxph(Surv(survival_time_365d, death_365d) ~ PMI_type + age_continuous + sex_factor + emergency_surg_factor + RCRI_high_factor, data = agreed_survival_cox)

# Extract adjusted HRs for display on plots
hr_30d_agreed_adj <- exp(coef(cox_30d_agreed_full)["PMI_typeNoncardiac"])
ci_30d_agreed_adj <- exp(confint(cox_30d_agreed_full)["PMI_typeNoncardiac",])
p_30d_agreed_adj <- summary(cox_30d_agreed_full)$coefficients["PMI_typeNoncardiac", "Pr(>|z|)"]

hr_365d_agreed_adj <- exp(coef(cox_365d_agreed_full)["PMI_typeNoncardiac"])
ci_365d_agreed_adj <- exp(confint(cox_365d_agreed_full)["PMI_typeNoncardiac",])
p_365d_agreed_adj <- summary(cox_365d_agreed_full)$coefficients["PMI_typeNoncardiac", "Pr(>|z|)"]

# Extract HRs
extract_hr <- function(model, model_name, outcome, dataset) {
  coef_name <- "PMI_typeNoncardiac"
  if(!coef_name %in% names(coef(model))) return(NULL)
  
  hr <- exp(coef(model)[coef_name])
  ci <- exp(confint(model)[coef_name,])
  p <- summary(model)$coefficients[coef_name, "Pr(>|z|)"]
  
  data.frame(
    Dataset = dataset,
    Outcome = outcome,
    Model = model_name,
    HR = round(hr, 2),
    CI = paste0(round(hr, 2), " (", round(ci[1], 2), "-", round(ci[2], 2), ")"),
    p_value = format.pval(p, digits = 3, eps = 0.001)
  )
}

hr_table <- bind_rows(
  extract_hr(cox_30d_obs12_unadj, "Unadjusted", "30-day", "OBS12"),
  extract_hr(cox_30d_obs12_full, "Fully adjusted", "30-day", "OBS12"),
  extract_hr(cox_365d_obs12_unadj, "Unadjusted", "365-day", "OBS12"),
  extract_hr(cox_365d_obs12_full, "Fully adjusted", "365-day", "OBS12"),
  extract_hr(cox_30d_agreed_unadj, "Unadjusted", "30-day", "Agreed"),
  extract_hr(cox_30d_agreed_full, "Fully adjusted", "30-day", "Agreed"),
  extract_hr(cox_365d_agreed_unadj, "Unadjusted", "365-day", "Agreed"),
  extract_hr(cox_365d_agreed_full, "Fully adjusted", "365-day", "Agreed")
)

cat("\n=== HAZARD RATIOS: Noncardiac vs Cardiac ===\n")
print(hr_table)

# ========== POSTOPERATIVE VITALS: MORTALITY ANALYSIS ==========

cat("\n\n=== POSTOPERATIVE VITALS AND IN-HOSPITAL MORTALITY ===\n\n")

# Helper function to create vitals mortality tables
create_vitals_mortality_table <- function(data, threshold_var, threshold_name, cohort_name) {
  cat("\n--- ", cohort_name, ": ", threshold_name, " ---\n\n", sep="")

  # Use base R filtering instead of tidy evaluation
  data_clean <- data[!is.na(data[[threshold_var]]) & !is.na(data$death_in_hospital), ]

  if(nrow(data_clean) == 0) {
    cat("No data available\n")
    return(NULL)
  }

  ct <- table(data_clean[[threshold_var]], data_clean$death_in_hospital)

  below <- sum(data_clean[[threshold_var]] == TRUE, na.rm = TRUE)
  above <- sum(data_clean[[threshold_var]] == FALSE, na.rm = TRUE)
  mort_below <- sum(data_clean[[threshold_var]] == TRUE & data_clean$death_in_hospital == 1, na.rm = TRUE)
  mort_above <- sum(data_clean[[threshold_var]] == FALSE & data_clean$death_in_hospital == 1, na.rm = TRUE)

  pct_mort_below <- if(below > 0) mort_below / below * 100 else NA
  pct_mort_above <- if(above > 0) mort_above / above * 100 else NA
  se_below <- if(below > 0) sqrt(pct_mort_below * (100 - pct_mort_below) / below) else NA
  se_above <- if(above > 0) sqrt(pct_mort_above * (100 - pct_mort_above) / above) else NA

  if(min(ct) >= 5) {
    test <- chisq.test(ct)
    test_name <- "Chi-square"
    p_value <- test$p.value
  } else {
    test <- fisher.test(ct)
    test_name <- "Fisher's exact"
    p_value <- test$p.value
  }

  cat("Threshold violation present (n=", below, "):\n", sep="")
  cat("  Deaths: ", mort_below, " (", round(pct_mort_below, 1), "% ± ", round(se_below, 1), " SE)\n", sep="")
  cat("No threshold violation (n=", above, "):\n", sep="")
  cat("  Deaths: ", mort_above, " (", round(pct_mort_above, 1), "% ± ", round(se_above, 1), " SE)\n\n", sep="")
  cat(test_name, " test: p=", format.pval(p_value, digits=3), "\n", sep="")

  # Build formula using as.formula for base R compatibility
  formula_str <- paste("death_in_hospital ~", threshold_var)
  model <- glm(as.formula(formula_str), data = data_clean, family = binomial)
  or <- exp(coef(model)[2])
  ci <- exp(confint(model)[2,])
  p_glm <- summary(model)$coefficients[2,4]
  cat("Odds Ratio: ", round(or, 2), " (95%CI: ", round(ci[1], 2), "-", round(ci[2], 2),
      "), p=", format.pval(p_glm, digits=3), "\n", sep="")
}

# SUMMARY TABLE - TOTAL COHORT
cat("\n\n=== SUMMARY TABLE: TOTAL COHORT ===\n\n")
cat("Postoperative Vital Sign Thresholds and In-Hospital Mortality\n\n")
cat(sprintf("%-20s | %-15s | %-15s | %-12s | %-20s | %-10s\n",
            "Threshold", "With Violation", "No Violation", "p-value", "Odds Ratio (95%CI)", ""))
cat(sprintf("%s\n", paste(rep("-", 100), collapse="")))

# MAP < 65
map_data <- obs12_with_pmi[!is.na(obs12_with_pmi$any_MAP_below_65) & !is.na(obs12_with_pmi$death_in_hospital), ]
if(nrow(map_data) > 0 && sum(map_data$any_MAP_below_65 == TRUE) > 0 && sum(map_data$any_MAP_below_65 == FALSE) > 0) {
  map_with <- sum(map_data$any_MAP_below_65 == TRUE & map_data$death_in_hospital == 1) / sum(map_data$any_MAP_below_65 == TRUE) * 100
  map_without <- sum(map_data$any_MAP_below_65 == FALSE & map_data$death_in_hospital == 1) / sum(map_data$any_MAP_below_65 == FALSE) * 100
  map_test <- if(min(table(map_data$any_MAP_below_65, map_data$death_in_hospital)) >= 5) chisq.test(table(map_data$any_MAP_below_65, map_data$death_in_hospital))$p.value else fisher.test(table(map_data$any_MAP_below_65, map_data$death_in_hospital))$p.value
  map_model <- glm(death_in_hospital ~ any_MAP_below_65, data = map_data, family = binomial)
  map_or <- exp(coef(map_model)[2])
  map_ci <- exp(confint(map_model)[2,])

  cat(sprintf("%-20s | %6.1f%% (n=%3d) | %6.1f%% (n=%3d) | %-12s | %4.2f (%4.2f-%4.2f)\n",
              "MAP < 65 mmHg",
              map_with, sum(map_data$any_MAP_below_65 == TRUE),
              map_without, sum(map_data$any_MAP_below_65 == FALSE),
              format.pval(map_test, digits=3),
              map_or, map_ci[1], map_ci[2]))
}

# HR > 120
hr_data <- obs12_with_pmi[!is.na(obs12_with_pmi$any_HR_above_120) & !is.na(obs12_with_pmi$death_in_hospital), ]
if(nrow(hr_data) > 0 && sum(hr_data$any_HR_above_120 == TRUE) > 0 && sum(hr_data$any_HR_above_120 == FALSE) > 0) {
  hr_with <- sum(hr_data$any_HR_above_120 == TRUE & hr_data$death_in_hospital == 1) / sum(hr_data$any_HR_above_120 == TRUE) * 100
  hr_without <- sum(hr_data$any_HR_above_120 == FALSE & hr_data$death_in_hospital == 1) / sum(hr_data$any_HR_above_120 == FALSE) * 100
  hr_test <- if(min(table(hr_data$any_HR_above_120, hr_data$death_in_hospital)) >= 5) chisq.test(table(hr_data$any_HR_above_120, hr_data$death_in_hospital))$p.value else fisher.test(table(hr_data$any_HR_above_120, hr_data$death_in_hospital))$p.value
  hr_model <- glm(death_in_hospital ~ any_HR_above_120, data = hr_data, family = binomial)
  hr_or <- exp(coef(hr_model)[2])
  hr_ci <- exp(confint(hr_model)[2,])

  cat(sprintf("%-20s | %6.1f%% (n=%3d) | %6.1f%% (n=%3d) | %-12s | %4.2f (%4.2f-%4.2f)\n",
              "HR > 120 bpm",
              hr_with, sum(hr_data$any_HR_above_120 == TRUE),
              hr_without, sum(hr_data$any_HR_above_120 == FALSE),
              format.pval(hr_test, digits=3),
              hr_or, hr_ci[1], hr_ci[2]))
}

# SpO2 < 90
spo2_data <- obs12_with_pmi[!is.na(obs12_with_pmi$any_SpO2_below_90) & !is.na(obs12_with_pmi$death_in_hospital), ]
if(nrow(spo2_data) > 0 && sum(spo2_data$any_SpO2_below_90 == TRUE) > 0 && sum(spo2_data$any_SpO2_below_90 == FALSE) > 0) {
  spo2_with <- sum(spo2_data$any_SpO2_below_90 == TRUE & spo2_data$death_in_hospital == 1) / sum(spo2_data$any_SpO2_below_90 == TRUE) * 100
  spo2_without <- sum(spo2_data$any_SpO2_below_90 == FALSE & spo2_data$death_in_hospital == 1) / sum(spo2_data$any_SpO2_below_90 == FALSE) * 100
  spo2_test <- if(min(table(spo2_data$any_SpO2_below_90, spo2_data$death_in_hospital)) >= 5) chisq.test(table(spo2_data$any_SpO2_below_90, spo2_data$death_in_hospital))$p.value else fisher.test(table(spo2_data$any_SpO2_below_90, spo2_data$death_in_hospital))$p.value
  spo2_model <- glm(death_in_hospital ~ any_SpO2_below_90, data = spo2_data, family = binomial)
  spo2_or <- exp(coef(spo2_model)[2])
  spo2_ci <- exp(confint(spo2_model)[2,])

  cat(sprintf("%-20s | %6.1f%% (n=%3d) | %6.1f%% (n=%3d) | %-12s | %4.2f (%4.2f-%4.2f)\n",
              "SpO2 < 90%",
              spo2_with, sum(spo2_data$any_SpO2_below_90 == TRUE),
              spo2_without, sum(spo2_data$any_SpO2_below_90 == FALSE),
              format.pval(spo2_test, digits=3),
              spo2_or, spo2_ci[1], spo2_ci[2]))
}

# SUMMARY TABLE - CARDIAC PMI SUBGROUP
cat("\n\n=== SUMMARY TABLE: CARDIAC PMI ===\n\n")
cat("Postoperative Vital Sign Thresholds and In-Hospital Mortality (Cardiac PMI Only)\n\n")

cardiac_data <- obs12_with_pmi %>% filter(PMI_type == "Cardiac")

cat(sprintf("%-20s | %-15s | %-15s | %-12s | %-20s | %-10s\n",
            "Threshold", "With Violation", "No Violation", "p-value", "Odds Ratio (95%CI)", ""))
cat(sprintf("%s\n", paste(rep("-", 100), collapse="")))

# MAP < 65 (Cardiac)
map_cardiac <- cardiac_data[!is.na(cardiac_data$any_MAP_below_65) & !is.na(cardiac_data$death_in_hospital), ]
if(nrow(map_cardiac) > 0 && sum(map_cardiac$any_MAP_below_65 == TRUE) > 0 && sum(map_cardiac$any_MAP_below_65 == FALSE) > 0) {
  map_c_with <- sum(map_cardiac$any_MAP_below_65 == TRUE & map_cardiac$death_in_hospital == 1) / sum(map_cardiac$any_MAP_below_65 == TRUE) * 100
  map_c_without <- sum(map_cardiac$any_MAP_below_65 == FALSE & map_cardiac$death_in_hospital == 1) / sum(map_cardiac$any_MAP_below_65 == FALSE) * 100
  map_c_test <- if(min(table(map_cardiac$any_MAP_below_65, map_cardiac$death_in_hospital)) >= 5) chisq.test(table(map_cardiac$any_MAP_below_65, map_cardiac$death_in_hospital))$p.value else fisher.test(table(map_cardiac$any_MAP_below_65, map_cardiac$death_in_hospital))$p.value
  map_c_model <- glm(death_in_hospital ~ any_MAP_below_65, data = map_cardiac, family = binomial)
  map_c_or <- exp(coef(map_c_model)[2])
  map_c_ci <- exp(confint(map_c_model)[2,])

  cat(sprintf("%-20s | %6.1f%% (n=%3d) | %6.1f%% (n=%3d) | %-12s | %4.2f (%4.2f-%4.2f)\n",
              "MAP < 65 mmHg",
              map_c_with, sum(map_cardiac$any_MAP_below_65 == TRUE),
              map_c_without, sum(map_cardiac$any_MAP_below_65 == FALSE),
              format.pval(map_c_test, digits=3),
              map_c_or, map_c_ci[1], map_c_ci[2]))
}

# HR > 120 (Cardiac)
hr_cardiac <- cardiac_data[!is.na(cardiac_data$any_HR_above_120) & !is.na(cardiac_data$death_in_hospital), ]
if(nrow(hr_cardiac) > 0 && sum(hr_cardiac$any_HR_above_120 == TRUE) > 0 && sum(hr_cardiac$any_HR_above_120 == FALSE) > 0) {
  hr_c_with <- sum(hr_cardiac$any_HR_above_120 == TRUE & hr_cardiac$death_in_hospital == 1) / sum(hr_cardiac$any_HR_above_120 == TRUE) * 100
  hr_c_without <- sum(hr_cardiac$any_HR_above_120 == FALSE & hr_cardiac$death_in_hospital == 1) / sum(hr_cardiac$any_HR_above_120 == FALSE) * 100
  hr_c_test <- if(min(table(hr_cardiac$any_HR_above_120, hr_cardiac$death_in_hospital)) >= 5) chisq.test(table(hr_cardiac$any_HR_above_120, hr_cardiac$death_in_hospital))$p.value else fisher.test(table(hr_cardiac$any_HR_above_120, hr_cardiac$death_in_hospital))$p.value
  hr_c_model <- glm(death_in_hospital ~ any_HR_above_120, data = hr_cardiac, family = binomial)
  hr_c_or <- exp(coef(hr_c_model)[2])
  hr_c_ci <- exp(confint(hr_c_model)[2,])

  cat(sprintf("%-20s | %6.1f%% (n=%3d) | %6.1f%% (n=%3d) | %-12s | %4.2f (%4.2f-%4.2f)\n",
              "HR > 120 bpm",
              hr_c_with, sum(hr_cardiac$any_HR_above_120 == TRUE),
              hr_c_without, sum(hr_cardiac$any_HR_above_120 == FALSE),
              format.pval(hr_c_test, digits=3),
              hr_c_or, hr_c_ci[1], hr_c_ci[2]))
}

# SpO2 < 90 (Cardiac)
spo2_cardiac <- cardiac_data[!is.na(cardiac_data$any_SpO2_below_90) & !is.na(cardiac_data$death_in_hospital), ]
if(nrow(spo2_cardiac) > 0 && sum(spo2_cardiac$any_SpO2_below_90 == TRUE) > 0 && sum(spo2_cardiac$any_SpO2_below_90 == FALSE) > 0) {
  spo2_c_with <- sum(spo2_cardiac$any_SpO2_below_90 == TRUE & spo2_cardiac$death_in_hospital == 1) / sum(spo2_cardiac$any_SpO2_below_90 == TRUE) * 100
  spo2_c_without <- sum(spo2_cardiac$any_SpO2_below_90 == FALSE & spo2_cardiac$death_in_hospital == 1) / sum(spo2_cardiac$any_SpO2_below_90 == FALSE) * 100
  spo2_c_test <- if(min(table(spo2_cardiac$any_SpO2_below_90, spo2_cardiac$death_in_hospital)) >= 5) chisq.test(table(spo2_cardiac$any_SpO2_below_90, spo2_cardiac$death_in_hospital))$p.value else fisher.test(table(spo2_cardiac$any_SpO2_below_90, spo2_cardiac$death_in_hospital))$p.value
  spo2_c_model <- glm(death_in_hospital ~ any_SpO2_below_90, data = spo2_cardiac, family = binomial)
  spo2_c_or <- exp(coef(spo2_c_model)[2])
  spo2_c_ci <- exp(confint(spo2_c_model)[2,])

  cat(sprintf("%-20s | %6.1f%% (n=%3d) | %6.1f%% (n=%3d) | %-12s | %4.2f (%4.2f-%4.2f)\n",
              "SpO2 < 90%",
              spo2_c_with, sum(spo2_cardiac$any_SpO2_below_90 == TRUE),
              spo2_c_without, sum(spo2_cardiac$any_SpO2_below_90 == FALSE),
              format.pval(spo2_c_test, digits=3),
              spo2_c_or, spo2_c_ci[1], spo2_c_ci[2]))
}

# ========== BAR CHARTS FOR VITAL SIGNS ==========

cat("\n\n=== CREATING BAR CHARTS FOR VITAL SIGNS ===\n\n")

# Bar chart 1: Overall threshold violations
threshold_data <- data.frame(
  Threshold = c("MAP < 65 mmHg", "HR > 120 bpm", "SpO2 < 90%"),
  Percentage = c(vitals_summary$MAP_below_65, vitals_summary$HR_above_120, vitals_summary$SpO2_below_90)
)

p1 <- ggplot(threshold_data, aes(x = reorder(Threshold, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "#4292c6", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), hjust = -0.2, size = 4) +
  coord_flip() +
  ylim(0, max(threshold_data$Percentage) * 1.15) +
  labs(title = "Postoperative Vital Sign Threshold Violations",
       subtitle = "Percentage of Total Cases",
       x = "Threshold",
       y = "Percentage (%)") +
  theme_minimal()

print(p1)

# Bar chart 2: Threshold violations by PMI type (Cardiac vs Noncardiac)
obs12_pmi_vitals <- obs12_with_pmi %>%
  group_by(PMI_type) %>%
  summarise(
    MAP_below_65 = sum(any_MAP_below_65 == TRUE, na.rm = TRUE) / n() * 100,
    HR_above_120 = sum(any_HR_above_120 == TRUE, na.rm = TRUE) / n() * 100,
    SpO2_below_90 = sum(any_SpO2_below_90 == TRUE, na.rm = TRUE) / n() * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(MAP_below_65, HR_above_120, SpO2_below_90),
               names_to = "Threshold", values_to = "Percentage") %>%
  mutate(Threshold = case_when(
    Threshold == "MAP_below_65" ~ "MAP < 65 mmHg",
    Threshold == "HR_above_120" ~ "HR > 120 bpm",
    Threshold == "SpO2_below_90" ~ "SpO2 < 90%"
  ))

p2 <- ggplot(obs12_pmi_vitals, aes(x = Threshold, y = Percentage, fill = PMI_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5) +
  labs(title = "Threshold Violations by PMI Type",
       subtitle = "Cardiac vs Noncardiac",
       x = "Threshold",
       y = "Percentage (%)",
       fill = "PMI Type") +
  theme_minimal() +
  scale_fill_manual(values = c("#E7B800", "#2E9FDF"))

print(p2)

# Bar chart 3: Mortality rates by threshold violations
mortality_data <- data.frame(
  Threshold = c("MAP < 65 mmHg", "HR > 120 bpm", "SpO2 < 90%"),
  With_Violation = c(
    if(exists("map_with")) map_with else NA,
    if(exists("hr_with")) hr_with else NA,
    if(exists("spo2_with")) spo2_with else NA
  ),
  Without_Violation = c(
    if(exists("map_without")) map_without else NA,
    if(exists("hr_without")) hr_without else NA,
    if(exists("spo2_without")) spo2_without else NA
  )
) %>%
  pivot_longer(cols = c(With_Violation, Without_Violation),
               names_to = "Group", values_to = "Mortality") %>%
  mutate(Group = if_else(Group == "With_Violation", "With Violation", "Without Violation"))

p3 <- ggplot(mortality_data %>% filter(!is.na(Mortality)),
             aes(x = Threshold, y = Mortality, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Mortality, 1), "%")),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5) +
  labs(title = "In-Hospital Mortality by Vital Sign Threshold Violations",
       x = "Threshold",
       y = "Mortality (%)",
       fill = "Threshold Status") +
  theme_minimal() +
  scale_fill_manual(values = c("#d73027", "#4575b4"))

print(p3)

# Bar chart 4: Cardiac vs Noncardiac PMI with In-Hospital Mortality
cat("\n--- Creating Cardiac vs Noncardiac PMI Mortality Bar Charts ---\n")

# Calculate mortality by PMI type
pmi_mortality_summary <- obs12_with_pmi %>%
  group_by(PMI_type) %>%
  summarise(
    N = n(),
    Deaths = sum(death_in_hospital, na.rm = TRUE),
    Mortality_pct = round(Deaths / N * 100, 1),
    .groups = "drop"
  )

# Bar chart for mortality by PMI type
p4 <- ggplot(pmi_mortality_summary, aes(x = PMI_type, y = Mortality_pct, fill = PMI_type)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(Mortality_pct, 1), "%\n(", Deaths, "/", N, ")")),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Cardiac" = "#E64B35", "Noncardiac" = "#4DBBD5")) +
  labs(title = "In-Hospital Mortality by PMI Aetiology",
       subtitle = "Cardiac vs Noncardiac PMI",
       x = "PMI Type",
       y = "Mortality (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, max(pmi_mortality_summary$Mortality_pct) * 1.2)

print(p4)

# Bar chart 5: Combined threshold violations and mortality by PMI type
# Prepare data for combined visualization
pmi_threshold_mortality <- obs12_with_pmi %>%
  group_by(PMI_type) %>%
  summarise(
    MAP_below_65_pct = sum(any_MAP_below_65 == TRUE, na.rm = TRUE) / n() * 100,
    HR_above_120_pct = sum(any_HR_above_120 == TRUE, na.rm = TRUE) / n() * 100,
    Mortality_pct = sum(death_in_hospital == 1, na.rm = TRUE) / n() * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(MAP_below_65_pct, HR_above_120_pct, Mortality_pct),
               names_to = "Metric", values_to = "Percentage") %>%
  mutate(Metric = case_when(
    Metric == "MAP_below_65_pct" ~ "Hypotension (MAP<65)",
    Metric == "HR_above_120_pct" ~ "Tachycardia (HR>120)",
    Metric == "Mortality_pct" ~ "In-Hospital Mortality"
  ))

p5 <- ggplot(pmi_threshold_mortality, aes(x = Metric, y = Percentage, fill = PMI_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Cardiac" = "#E64B35", "Noncardiac" = "#4DBBD5")) +
  labs(title = "Vital Sign Thresholds and Mortality by PMI Aetiology",
       subtitle = "Cardiac vs Noncardiac PMI",
       x = "",
       y = "Percentage (%)",
       fill = "PMI Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

print(p5)

# Bar chart 6: Mortality stratified by PMI type AND threshold violations
cardiac_mort_by_threshold <- obs12_with_pmi %>%
  filter(PMI_type == "Cardiac") %>%
  summarise(
    MAP_with = sum(any_MAP_below_65 == TRUE & death_in_hospital == 1, na.rm = TRUE) /
               sum(any_MAP_below_65 == TRUE, na.rm = TRUE) * 100,
    MAP_without = sum(any_MAP_below_65 == FALSE & death_in_hospital == 1, na.rm = TRUE) /
                  sum(any_MAP_below_65 == FALSE, na.rm = TRUE) * 100,
    HR_with = sum(any_HR_above_120 == TRUE & death_in_hospital == 1, na.rm = TRUE) /
              sum(any_HR_above_120 == TRUE, na.rm = TRUE) * 100,
    HR_without = sum(any_HR_above_120 == FALSE & death_in_hospital == 1, na.rm = TRUE) /
                 sum(any_HR_above_120 == FALSE, na.rm = TRUE) * 100
  )

noncardiac_mort_by_threshold <- obs12_with_pmi %>%
  filter(PMI_type == "Noncardiac") %>%
  summarise(
    MAP_with = sum(any_MAP_below_65 == TRUE & death_in_hospital == 1, na.rm = TRUE) /
               sum(any_MAP_below_65 == TRUE, na.rm = TRUE) * 100,
    MAP_without = sum(any_MAP_below_65 == FALSE & death_in_hospital == 1, na.rm = TRUE) /
                  sum(any_MAP_below_65 == FALSE, na.rm = TRUE) * 100,
    HR_with = sum(any_HR_above_120 == TRUE & death_in_hospital == 1, na.rm = TRUE) /
              sum(any_HR_above_120 == TRUE, na.rm = TRUE) * 100,
    HR_without = sum(any_HR_above_120 == FALSE & death_in_hospital == 1, na.rm = TRUE) /
                 sum(any_HR_above_120 == FALSE, na.rm = TRUE) * 100
  )

stratified_mort_data <- data.frame(
  PMI_type = rep(c("Cardiac", "Noncardiac"), each = 4),
  Threshold = rep(c("MAP<65", "MAP≥65", "HR>120", "HR≤120"), 2),
  Mortality = c(
    cardiac_mort_by_threshold$MAP_with,
    cardiac_mort_by_threshold$MAP_without,
    cardiac_mort_by_threshold$HR_with,
    cardiac_mort_by_threshold$HR_without,
    noncardiac_mort_by_threshold$MAP_with,
    noncardiac_mort_by_threshold$MAP_without,
    noncardiac_mort_by_threshold$HR_with,
    noncardiac_mort_by_threshold$HR_without
  )
) %>%
  filter(!is.na(Mortality) & !is.infinite(Mortality))

p6 <- ggplot(stratified_mort_data, aes(x = Threshold, y = Mortality, fill = PMI_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(Mortality, 1), "%")),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Cardiac" = "#E64B35", "Noncardiac" = "#4DBBD5")) +
  labs(title = "Mortality by PMI Type and Vital Sign Thresholds",
       subtitle = "Stratified Analysis",
       x = "Threshold Category",
       y = "Mortality (%)",
       fill = "PMI Type") +
  theme_minimal()

print(p6)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("\n✓ Comparison table: OBS12 vs Agreed PMI categories\n")
cat("✓ PMI category overviews (noncardiac, cardiac, T2MI)\n")
cat("✓ Surgical specialty analysis with p-values for cardiac vs noncardiac\n")
cat("✓ Baseline characteristics tables\n")
cat("✓ Surgical specialty p-values integrated into OBS12 and Agreed tables\n")
cat("✓ Cox regression with adjusted HR\n")
cat("✓ UNIFORM Date from data_included used for all survival calculations\n")
cat("✓ Postoperative vitals analysis with hypotension/tachycardia detection\n")
cat("✓ In-hospital mortality by vital sign threshold violations\n")
cat("✓ Mortality tables for total cohort and cardiac PMI subgroup\n")
cat("✓ **NEW: First hsTnT value coupled with admission location (specialty & ward)**\n")
cat("✓ **NEW: Enhanced bar charts with cardiac vs noncardiac colors (#E64B35 & #4DBBD5)**\n")
cat("✓ **NEW: Mortality by PMI aetiology bar charts**\n")
cat("✓ **NEW: Combined vitals thresholds and mortality visualization**\n")
cat("✓ **NEW: Stratified mortality analysis by PMI type and thresholds**\n")
cat("✓ Kaplan-Meier curves removed as requested\n")
