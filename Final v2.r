#REMAIN OBS1/2/3/4 - MASTER CODE: Enhanced Analysis with PMI Categories, In-Hospital Mortality, and Emergency Surgery Adjustment
# Key Features:
# 1. In-hospital mortality by PMI aetiology (from opname_bestemming only)
# 2. Postoperative vitals analysis (hypotension & tachycardia)
# 3. Logistic regression adjusted for emergency surgery, age, and sex
# 4. Subgroup analysis in elective surgeries
# 5. VAS pain score analysis
# Set CRAN mirror (HTTP instead of HTTPS for network compatibility)
options(repos = c(CRAN = "http://cran.r-project.org"))

# Load required packages
library(tidyverse)
library(readxl)
library(dplyr)
library(irr)
library(tableone)
library(survival)
library(survminer)
if (!require("cmprsk")) install.packages("cmprsk")
library(cmprsk)

# Print R and package versions
cat("\n=== SOFTWARE VERSIONS ===\n")
cat("R version:", R.version.string, "\n")
cat("RStudio version: Check Help > About RStudio\n")
cat("tableone version:", as.character(packageVersion("tableone")), "\n")
cat("dplyr version:", as.character(packageVersion("dplyr")), "\n")
cat("ggplot2 version:", as.character(packageVersion("ggplot2")), "\n\n")

data <- read_csv2("Z:/REMAIN/Castor exports/REMAIN_ALLOBS_participant_data_csv_2025_06_02-10_55_02/REMAIN_export_20250602.csv")

#Coupling pseudonym_value and castor study nr
coupling <- read_excel("Z:/REMAIN/Mario Stark/Files gebruikt tijdens stage/Castor+pseudonym+date.xlsx")
demographics <- read_csv("Z:/REMAIN/Data export Datacapture/REMAIN - 2024-06-20 - demografie.csv")
postoperativevitals <- read.csv("Z:/REMAIN/Data export Datacapture/REMAIN - metingen_postoperatief_20241010.csv")
opname <- read_csv("Z:/REMAIN/Data export Datacapture/REMAIN - 2024-06-20 - opname.csv")
lab <- read_csv("Z:/REMAIN/Data export Datacapture/REMAIN - 2024-06-20 - lab.csv")
verrichtingen <- read.csv("###") # TODO: fill in the correct path to verrichtingen CSV

#Exclude LOTx, centrale lijn, minor eg gastroscopy, tropo incorrect, recent cardiac surgery/intervention, organ donation and old/insufficient file
exclude_ids <- c(110014,110015,110028,110033,110051,110067,110068,110083,110096,110099,110100,110114,110117,110126,110131,110143,110148,110159,110160,110162,110166,110183,110187,110198,110205,110210,110214,110221,110222,110225,110230,110238,110251,110261,110270,110279,110287,110289,110300,110306,110308,110318,110330,110343,110347,110349,110352,110357,110368,110376,110378,110379,110382,110398,110399,110408,110416,110418,110431,110432,110443,110445,110451,110455,110456,110458,110470,110476,110487,110491,110515,110521,110544,110550,110553,110567,110573,110577,110586,110588,110608,110609,110614,110627,110635,110644,110669,110686,110694,110707,110719,110722,110726,110743,110747,110752,110753,110765,110774,110776,110783,110789,110827,110841,110864,110873,110876,110878,110879,110884,110891,110899,110908,110911,110914,110920,110922,110930,110931,110933,110941,110943,110948,110951,110953,110970,110973,110981,110983,110985,110987,110993,110995,110998,111001,111005,111009,111011,111012,111014,111026,111030,111032,111034,111038,111049,111053,111055,111056,111064,111070,111075,111076,111082,111087,111097,110248
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
data_included <- data_included %>%
  mutate(Study_number = as.numeric(`Participant Id`)) %>%
  left_join(coupling %>% 
              mutate(Date = as.Date(as.character(Date))) %>%
              select(Study_number, Pseudonym, Date), 
            by = "Study_number") %>%
  left_join(demographics %>% select(pseudonym_value, leeftijd, gender_display), 
            by = c("Pseudonym" = "pseudonym_value")) %>%
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

cat("\n=== PATIENT COUNT TRACKING ===\n")
cat("Total data_final patients:", nrow(data_final), "\n")
cat("OBS12 patients:", nrow(obs12), "\n")
cat("OBS34 patients:", nrow(obs34), "\n")                

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
obs12_before_filter <- obs12 %>%
  mutate(
    PMI_type = case_when(
      Cause_cardiac_yes == 1 | cause_T2MI == 1 |
        (cause_T2MI == 0 & cause_extra_car_yes == 0 & Cause_cardiac_yes == 0) ~ "Cardiac",
      cause_extra_car_yes == 1 ~ "Noncardiac",
      TRUE ~ NA_character_
    )
  )

cat("OBS12 before PMI_type filter:", nrow(obs12_before_filter), "\n")
cat("OBS12 with NA PMI_type:", sum(is.na(obs12_before_filter$PMI_type)), "\n")

obs12_with_pmi <- obs12_before_filter %>%
  filter(!is.na(PMI_type)) %>%
  filter(!is.na(Pseudonym)) %>%
  distinct(Pseudonym, .keep_all = TRUE)

cat("OBS12_with_PMI after filter:", nrow(obs12_with_pmi), "\n")
cat("Patients lost due to NA PMI_type:", nrow(obs12_before_filter) - nrow(obs12_with_pmi), "\n")
cat("Patients with NA Pseudonym excluded\n\n")

# Create agreed_with_pmi dataset
agreed_with_pmi <- obs12 %>%
  inner_join(agreed_patients, by = "Participant Id") %>%
  filter(!is.na(PMI_type))

# Agreed survival - one row per patient
agreed_survival <- obs12 %>%
  inner_join(agreed_patients, by = "Participant Id") %>%
  filter(!is.na(PMI_type)) %>%
  filter(!is.na(Pseudonym)) %>%
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

# Extract death from opname dataset based on discharge destination
# IN-HOSPITAL MORTALITY is defined ONLY by opname_bestemming
death_from_opname <- opname %>%
  mutate(
    death_in_hospital = if_else(
      opname_bestemming %in% c("Overleden (zonder obductie)", "Overleden (met obductie)"),
      1, 0
    )
  ) %>%
  group_by(pseudonym_value) %>%
  summarise(
    death_in_hospital = max(death_in_hospital, na.rm = TRUE),  # If any admission resulted in death
    .groups = "drop"
  )

cat("\n=== IN-HOSPITAL MORTALITY FROM OPNAME ===\n")
cat("In-hospital deaths (from opname_bestemming):", sum(death_from_opname$death_in_hospital == 1, na.rm = TRUE), "\n\n")

# Merge vital signs and death indicator with obs12_with_pmi and agreed_survival
obs12_with_pmi <- obs12_with_pmi %>%
  left_join(patient_hemodynamics, by = c("Pseudonym" = "pseudonym_value")) %>%
  left_join(death_from_opname, by = c("Pseudonym" = "pseudonym_value")) %>%
  mutate(death_in_hospital = if_else(is.na(death_in_hospital), 0, death_in_hospital))

agreed_survival <- agreed_survival %>%
  left_join(patient_hemodynamics, by = c("Pseudonym" = "pseudonym_value")) %>%
  left_join(death_from_opname, by = c("Pseudonym" = "pseudonym_value")) %>%
  mutate(death_in_hospital = if_else(is.na(death_in_hospital), 0, death_in_hospital))

cat("=== IN-HOSPITAL MORTALITY SUMMARY (OBS12) ===\n")
cat("Total in-hospital deaths:", sum(obs12_with_pmi$death_in_hospital == 1, na.rm = TRUE), "\n")
cat("In-hospital mortality rate:", round(sum(obs12_with_pmi$death_in_hospital == 1, na.rm = TRUE) / nrow(obs12_with_pmi) * 100, 1), "%\n\n")

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

# Get first hsTnT measurement per patient from lab data based on earliest collection_collectedDateTime
first_hstnt <- lab %>%
  mutate(
    valueQuantity_value = as.numeric(gsub(",", ".", valueQuantity_value)),  # Replace comma with period, then convert
    collection_collectedDateTime = as.POSIXct(collection_collectedDateTime)  # Convert to datetime
  ) %>%
  filter(!is.na(valueQuantity_value)) %>%
  group_by(pseudonym_value) %>%
  arrange(collection_collectedDateTime) %>%
  slice(1) %>%  # Take the first row (earliest datetime) per patient
  ungroup() %>%
  select(pseudonym_value,
         first_hstnt_value = valueQuantity_value,
         first_hstnt_datetime = collection_collectedDateTime)

# Couple with ward location from opname based on pseudonym_value
hstnt_location <- first_hstnt %>%
  left_join(opname %>% select(pseudonym_value, specialty_display_original, opnamedeel_afdeling),
            by = "pseudonym_value")

obs12_with_pmi <- obs12_with_pmi %>%
  left_join(hstnt_location, by = c("Pseudonym" = "pseudonym_value"))

agreed_survival <- agreed_survival %>%
  left_join(hstnt_location, by = c("Pseudonym" = "pseudonym_value"))

cat("First hsTnT values coupled with ward location (opnamedeel_afdeling)\n")
cat("NOTE: Using earliest collection_collectedDateTime per pseudonym_value from lab\n")
cat("      Coupled with opnamedeel_afdeling from opname by pseudonym_value\n\n")
cat("Patients with hsTnT data:", sum(!is.na(obs12_with_pmi$first_hstnt_value)), "\n")
cat("Patients with specialty data:", sum(!is.na(obs12_with_pmi$specialty_display_original)), "\n")
cat("Patients with ward data:", sum(!is.na(obs12_with_pmi$opnamedeel_afdeling)), "\n")
cat("Patients with collection datetime:", sum(!is.na(obs12_with_pmi$first_hstnt_datetime)), "\n\n")

# Show sample of first hsTnT coupling with datetime and location
cat("--- Sample of First hsTnT Coupling (first 10 patients) ---\n")
sample_coupling <- obs12_with_pmi %>%
  filter(!is.na(first_hstnt_value)) %>%
  select(Pseudonym, first_hstnt_value, first_hstnt_datetime, opnamedeel_afdeling, specialty_display_original) %>%
  head(10)
print(sample_coupling)
cat("\n")

# Count of unique patients per ward (opnamedeel_afdeling) in OBS12_with_PMI
cat("--- OBS12_with_PMI Patients Count by Ward (opnamedeel_afdeling) ---\n")
patients_per_ward <- obs12_with_pmi %>%
  group_by(opnamedeel_afdeling) %>%
  summarise(
    N_Patients = n_distinct(Pseudonym),
    .groups = "drop"
  ) %>%
  arrange(desc(N_Patients))

print(patients_per_ward, n = Inf)
cat(sprintf("\nTotal OBS12_with_PMI patients: %d\n", n_distinct(obs12_with_pmi$Pseudonym)))
cat(sprintf("Patients with ward data: %d\n", sum(!is.na(obs12_with_pmi$opnamedeel_afdeling))))
cat(sprintf("Patients without ward data: %d\n\n", sum(is.na(obs12_with_pmi$opnamedeel_afdeling))))

# Summary of first hsTnT by ward location
cat("--- First hsTnT Values by Ward (opnamedeel_afdeling) ---\n")
hstnt_by_ward <- obs12_with_pmi %>%
  filter(!is.na(first_hstnt_value)) %>%
  group_by(opnamedeel_afdeling) %>%
  summarise(
    N = n(),
    Mean_hsTnT = round(mean(first_hstnt_value, na.rm = TRUE), 1),
    Median_hsTnT = round(median(first_hstnt_value, na.rm = TRUE), 1),
    SD_hsTnT = round(sd(first_hstnt_value, na.rm = TRUE), 1),
    Min_hsTnT = round(min(first_hstnt_value, na.rm = TRUE), 1),
    Max_hsTnT = round(max(first_hstnt_value, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(N))

print(hstnt_by_ward, n = Inf)

cat("\n--- First hsTnT Values by Specialty ---\n")
hstnt_by_specialty <- obs12_with_pmi %>%
  filter(!is.na(first_hstnt_value)) %>%
  group_by(specialty_display_original) %>%
  summarise(
    N = n(),
    Mean_hsTnT = round(mean(first_hstnt_value, na.rm = TRUE), 1),
    Median_hsTnT = round(median(first_hstnt_value, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(N))

print(hstnt_by_specialty, n = Inf)
cat("\n")

# Summary of first hsTnT by year
cat("--- First hsTnT Values by Year (2017-2023) ---\n")
hstnt_by_year <- obs12_with_pmi %>%
  filter(!is.na(first_hstnt_value), !is.na(first_hstnt_datetime)) %>%
  mutate(Year = as.numeric(format(as.POSIXct(first_hstnt_datetime), "%Y"))) %>%
  filter(Year >= 2017, Year <= 2023) %>%
  group_by(Year) %>%
  summarise(
    N_Patients = n(),
    Mean_hsTnT = round(mean(first_hstnt_value, na.rm = TRUE), 1),
    Median_hsTnT = round(median(first_hstnt_value, na.rm = TRUE), 1),
    SD_hsTnT = round(sd(first_hstnt_value, na.rm = TRUE), 1),
    Min_hsTnT = round(min(first_hstnt_value, na.rm = TRUE), 1),
    Max_hsTnT = round(max(first_hstnt_value, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(Year)

print(hstnt_by_year, n = Inf)
cat("\n")

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
  ) %>%
  filter(!is.na(PMI_category))  # Remove rows with NA PMI_category

cat("OBS12 patients after removing NA PMI_category:", nrow(obs12_with_pmi), "\n\n")

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
  ) %>%
  filter(!is.na(PMI_category))  # Remove rows with NA PMI_category

cat("Agreed cases after removing NA PMI_category:", nrow(agreed_survival), "\n\n")

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

# Test continuous variables for normality using OBS12 data
continuous_vars <- c("leeftijd", "RCRI_score")

cat("Testing normality for continuous variables in OBS12:\n\n")
normality_results_obs12 <- data.frame()

for(var in continuous_vars) {
  if(var %in% names(obs12_with_pmi)) {
    test_data <- obs12_with_pmi[[var]][!is.na(obs12_with_pmi[[var]])]
    if(length(test_data) > 3 & length(test_data) < 5000) {
      shapiro_test <- shapiro.test(test_data)
      cat(var, ":\n")
      cat("  W =", round(shapiro_test$statistic, 4), "\n")
      cat("  p-value =", format.pval(shapiro_test$p.value, digits = 3), "\n")
      cat("  Distribution:", ifelse(shapiro_test$p.value < 0.05, "Non-normal", "Normal"), "\n\n")
      
      normality_results_obs12 <- rbind(normality_results_obs12, 
                                    data.frame(Variable = var, 
                                              W = shapiro_test$statistic,
                                              p_value = shapiro_test$p.value,
                                              Distribution = ifelse(shapiro_test$p.value < 0.05, "Non-normal", "Normal")))
    }
  }
}

nonnormal_vars <- normality_results_obs12 %>%
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
                    "RCRI_score", "death_in_hospital")

cat_vars <- c("gender_display", "emergency_surg", "surg_specialty",
              "history#Coronary Artery Disease", "history#Myocardial Infarction",
              "history#Pheripheral Artery Disease", "history#Stroke / TIA",
              "history#Chronic Heart Failure", "history#Atrial Fibrilation",
              "history#Moderate/Severe Valvular Disease", "history#Diabetus Mellitus, non-insulin",
              "history#Diabetus Mellitus, insulin dependent", "history#Chronic Kidney Disease",
              "history#Hypertension", "history#Chronic Obstructive Pulmonary Disease",
              "death_in_hospital")

# Table 1: OBS12 - All patients
cat("\n=== TABLE 1: OBS12 - ALL PATIENTS ===\n")
table1_obs12 <- CreateTableOne(vars = vars_for_table,
                              data = obs12_with_pmi,
                              factorVars = cat_vars,
                              test = FALSE)
print(table1_obs12, nonnormal = nonnormal_vars, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

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

# ========== BAR CHART: PMI CAUSES AS PERCENTAGE ==========

cat("\n--- Creating Bar Chart of PMI Causes (OBS12) ---\n")

# Prepare data for bar chart with in-hospital mortality
pmi_causes_chart_data <- obs12_with_pmi %>%
  filter(!is.na(PMI_category)) %>%  # Exclude NA PMI categories
  group_by(PMI_category, PMI_type) %>%
  summarise(
    N = n(),
    Deaths_InHospital = sum(death_in_hospital, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    Percentage = round(N / sum(N) * 100, 1),
    Mortality_InHospital_Pct = round(Deaths_InHospital / N * 100, 1),
    # Rename PMI categories for cleaner display
    PMI_category_display = case_when(
      PMI_category == "T2MI_with_cause" ~ "T2MI+",
      PMI_category == "T2MI_without_cause" ~ "T2MI-",
      PMI_category == "renal_fail" ~ "Acute renal failure",
      PMI_category == "ctrauma" ~ "Trauma",
      PMI_category == "stroke" ~ "Stroke",
      PMI_category == "sepsis" ~ "Sepsis",
      PMI_category == "tachy" ~ "Tachyarrhythmia",
      PMI_category == "ex_car_other" ~ "Other",
      TRUE ~ as.character(PMI_category)
    )
  ) %>%
  arrange(desc(Percentage))

# Create dual bar charts: Distribution and Mortality side by side
# Light blue for Noncardiac (extracardiac), Light red for Cardiac

# Load gridExtra if not already loaded
if (!require("gridExtra")) install.packages("gridExtra")
library(gridExtra)
library(grid)

# Chart 1: Distribution of PMI aetiologies
plot1 <- ggplot(pmi_causes_chart_data, aes(x = reorder(PMI_category_display, Percentage),
                                            y = Percentage,
                                            fill = PMI_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Cardiac" = "#FFB6C1", "Noncardiac" = "#ADD8E6")) +
  coord_flip() +
  labs(
    title = "Distribution of PMI Aetiologies",
    x = "PMI Category",
    y = "Percentage (%)",
    fill = "PMI Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "none"  # Remove legend here, will add shared legend below
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  geom_text(aes(label = paste0(N, " (", Percentage, "%)")),
            hjust = -0.1,
            size = 3)

# Chart 2: In-hospital mortality by PMI aetiology
plot2 <- ggplot(mortality_data <- pmi_causes_chart_data,
                aes(x = reorder(PMI_category_display, Percentage),
                    y = Mortality_InHospital_Pct,
                    fill = PMI_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Cardiac" = "#FFB6C1", "Noncardiac" = "#ADD8E6")) +
  coord_flip() +
  labs(
    title = "In-Hospital Mortality by PMI Aetiology",
    x = "",
    y = "Mortality (%)",
    fill = "PMI Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.text.y = element_blank(),  # Remove y-axis labels (same as plot 1)
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 10),
    legend.position = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  geom_text(aes(label = paste0(Deaths_InHospital, " (", Mortality_InHospital_Pct, "%)")),
            hjust = -0.1,
            size = 3)

# Create a dummy plot to extract legend
legend_plot <- ggplot(pmi_causes_chart_data, aes(x = Percentage, y = PMI_category_display, fill = PMI_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Cardiac" = "#FFB6C1", "Noncardiac" = "#ADD8E6")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(fill = "PMI Type")

# Extract legend
library(ggplotify)
get_legend <- function(plot) {
  tmp <- ggplot_gtable(ggplot_build(plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if(length(leg) > 0) return(tmp$grobs[[leg]])
  return(NULL)
}

legend <- get_legend(legend_plot)

# Combine plots side by side with shared legend at bottom
combined_plot <- grid.arrange(
  arrangeGrob(plot1, plot2, ncol = 2),
  legend,
  nrow = 2,
  heights = c(10, 1),
  top = textGrob("PMI Aetiology: Distribution and In-Hospital Mortality",
                 gp = gpar(fontsize = 14, fontface = "bold"))
)

# Save the combined plot
ggsave("PMI_Distribution_Mortality_OBS12.png", plot = combined_plot, width = 16, height = 8, dpi = 300)
cat("Combined bar chart saved as 'PMI_Distribution_Mortality_OBS12.png'\n\n")

# Print the data table
cat("PMI Causes Distribution:\n")
print(pmi_causes_chart_data %>% select(PMI_category_display, PMI_type, N, Percentage), n = Inf)
cat("\n")

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

# ========== IN-HOSPITAL MORTALITY ADJUSTED FOR EMERGENCY SURGERY ==========

cat("\n\n=== IN-HOSPITAL MORTALITY: EMERGENCY SURGERY ADJUSTMENT ===\n")
cat("Addressing potential confounding by emergency surgery status\n\n")

# Check emergency surgery distribution
cat("--- Emergency Surgery Distribution (OBS12) ---\n")
emergency_distribution <- obs12_with_pmi %>%
  filter(!is.na(emergency_surg)) %>%
  group_by(emergency_surg) %>%
  summarise(
    N = n(),
    Deaths = sum(death_in_hospital == 1, na.rm = TRUE),
    Mortality_pct = round(Deaths / N * 100, 1),
    .groups = 'drop'
  ) %>%
  mutate(
    Surgery_Type = if_else(emergency_surg == 1, "Emergency", "Elective")
  )

print(emergency_distribution, n = Inf)

# Mortality by emergency status and PMI type
cat("\n--- In-Hospital Mortality by Emergency Status and PMI Type (OBS12) ---\n")
mortality_emergency_pmi <- obs12_with_pmi %>%
  filter(!is.na(emergency_surg) & !is.na(PMI_type)) %>%
  group_by(PMI_type, emergency_surg) %>%
  summarise(
    N = n(),
    Deaths = sum(death_in_hospital == 1, na.rm = TRUE),
    Mortality_pct = round(Deaths / N * 100, 1),
    .groups = 'drop'
  ) %>%
  mutate(
    Surgery_Type = if_else(emergency_surg == 1, "Emergency", "Elective")
  ) %>%
  arrange(PMI_type, desc(emergency_surg))

print(mortality_emergency_pmi, n = Inf)

# TEST: Is PMI type distribution different between emergency and elective?
cat("\n--- Chi-square Test: PMI Type Distribution by Emergency Status ---\n")
cat("Testing if cardiac vs noncardiac proportions differ between emergency and elective surgeries\n\n")

pmi_emergency_table <- obs12_with_pmi %>%
  filter(!is.na(emergency_surg) & !is.na(PMI_type)) %>%
  select(PMI_type, emergency_surg) %>%
  table()

cat("Contingency Table:\n")
print(pmi_emergency_table)
cat("\n")

if(sum(pmi_emergency_table) > 0) {
  pmi_emergency_test <- chisq.test(pmi_emergency_table)
  print(pmi_emergency_test)
  cat("\n*** P-VALUE for PMI type distribution (Emergency vs Elective): ", 
      format.pval(pmi_emergency_test$p.value, digits = 3), " ***\n", sep = "")
  
  if(pmi_emergency_test$p.value < 0.05) {
    cat("INTERPRETATION: PMI type distribution DIFFERS significantly between emergency and elective surgeries\n\n")
  } else {
    cat("INTERPRETATION: No significant difference in PMI type distribution between emergency and elective surgeries\n\n")
  }
} else {
  cat("Insufficient data for chi-square test\n\n")
}

# Logistic regression: Mortality adjusted for emergency surgery
cat("\n--- Logistic Regression Models: In-Hospital Mortality ---\n\n")

# Prepare data
mortality_logistic_data <- obs12_with_pmi %>%
  filter(!is.na(death_in_hospital) & !is.na(emergency_surg) & !is.na(PMI_type)) %>%
  mutate(
    PMI_type = factor(PMI_type, levels = c("Cardiac", "Noncardiac"))  # Cardiac as reference
  )

cat("Model 1: PMI Type (Unadjusted)\n")
cat("Reference category: Cardiac PMI\n")
model1 <- glm(death_in_hospital ~ PMI_type, 
              data = mortality_logistic_data, 
              family = binomial)
print(summary(model1))

# Calculate OR and 95% CI
or1 <- exp(coef(model1))
ci1 <- exp(confint(model1))
cat("\nOdds Ratios with 95% CI:\n")
print(cbind(OR = or1, ci1))

cat("\n\nModel 2: PMI Type adjusted for Emergency Surgery only\n")
cat("Reference category: Cardiac PMI\n")
model2 <- glm(death_in_hospital ~ PMI_type + emergency_surg, 
              data = mortality_logistic_data, 
              family = binomial)
print(summary(model2))

# Calculate OR and 95% CI
or2 <- exp(coef(model2))
ci2 <- exp(confint(model2))
cat("\nOdds Ratios with 95% CI:\n")
print(cbind(OR = or2, ci2))

cat("\n\nModel 3: Adjusted for emergency surgery and age\n")
cat("Reference category: Cardiac PMI\n")
model3 <- glm(death_in_hospital ~ PMI_type + emergency_surg + leeftijd, 
              data = mortality_logistic_data, 
              family = binomial)
print(summary(model3))

# Calculate OR and 95% CI
or3 <- exp(coef(model3))
ci3 <- exp(confint(model3))
cat("\nOdds Ratios with 95% CI:\n")
print(cbind(OR = or3, ci3))

cat("\n\nModel 4: Multivariable model adjusted for age, sex, and emergency surgery\n")
cat("Reference category: Cardiac PMI\n")
model4 <- glm(death_in_hospital ~ PMI_type + emergency_surg + leeftijd + gender_display, 
              data = mortality_logistic_data, 
              family = binomial)
print(summary(model4))

# Calculate OR and 95% CI
or4 <- exp(coef(model4))
ci4 <- exp(confint(model4))
cat("\nOdds Ratios with 95% CI:\n")
print(cbind(OR = or4, ci4))

# Compare models
cat("\n--- Model Comparison ---\n")
cat("Model 1 - Unadjusted OR for Noncardiac vs Cardiac PMI:", round(or1["PMI_typeNoncardiac"], 2), "\n")
cat("Model 2 - OR adjusted for emergency surgery only:", round(or2["PMI_typeNoncardiac"], 2), "\n")
cat("Model 3 - OR adjusted for emergency surgery and age:", round(or3["PMI_typeNoncardiac"], 2), "\n")
cat("Model 4 - OR adjusted for age, sex, emergency surgery:", round(or4["PMI_typeNoncardiac"], 2), "\n\n")

cat("Emergency surgery OR (Model 2, adjusted for PMI type):", round(or2["emergency_surg"], 2), "\n")
cat("Emergency surgery OR (Model 3, adjusted for PMI type and age):", round(or3["emergency_surg"], 2), "\n")
cat("Emergency surgery OR (Model 4, multivariable):", round(or4["emergency_surg"], 2), "\n\n")

# Create summary table
cat("\n--- Summary Table of Adjusted ORs ---\n")
summary_table <- data.frame(
  Model = c("Unadjusted", "Adjusted for emergency", "Adjusted for emergency + age", "Multivariable adjusted"),
  OR_PMI_Noncardiac = c(
    round(or1["PMI_typeNoncardiac"], 2),
    round(or2["PMI_typeNoncardiac"], 2),
    round(or3["PMI_typeNoncardiac"], 2),
    round(or4["PMI_typeNoncardiac"], 2)
  ),
  CI_Lower = c(
    round(ci1["PMI_typeNoncardiac", 1], 2),
    round(ci2["PMI_typeNoncardiac", 1], 2),
    round(ci3["PMI_typeNoncardiac", 1], 2),
    round(ci4["PMI_typeNoncardiac", 1], 2)
  ),
  CI_Upper = c(
    round(ci1["PMI_typeNoncardiac", 2], 2),
    round(ci2["PMI_typeNoncardiac", 2], 2),
    round(ci3["PMI_typeNoncardiac", 2], 2),
    round(ci4["PMI_typeNoncardiac", 2], 2)
  ),
  Adjusted_for = c(
    "None",
    "Emergency surgery",
    "Emergency + age",
    "Emergency + age + sex"
  )
)
print(summary_table)

# Visualization: Mortality by PMI type and emergency status
cat("\n--- Creating Emergency Surgery Mortality Visualization ---\n")

mortality_plot_data <- mortality_emergency_pmi %>%
  mutate(
    Surgery_Type = factor(Surgery_Type, levels = c("Elective", "Emergency"))
  )

emergency_mortality_plot <- ggplot(mortality_plot_data, 
                                   aes(x = Surgery_Type, 
                                       y = Mortality_pct, 
                                       fill = PMI_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Cardiac" = "#FFB6C1", "Noncardiac" = "#ADD8E6")) +
  labs(
    title = "In-Hospital Mortality by Surgery Type and PMI Type",
    subtitle = "OBS12 Patients",
    x = "Surgery Type",
    y = "In-Hospital Mortality (%)",
    fill = "PMI Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.text = element_text(size = 11),
    legend.position = "bottom"
  ) +
  geom_text(aes(label = paste0(Deaths, "/", N, "\n(", Mortality_pct, "%)")),
            position = position_dodge(width = 0.7),
            vjust = -0.5,
            size = 3.5)

ggsave("Mortality_Emergency_vs_Elective_by_PMI.png", 
       plot = emergency_mortality_plot, 
       width = 10, 
       height = 7, 
       dpi = 300)

cat("Emergency surgery mortality plot saved as 'Mortality_Emergency_vs_Elective_by_PMI.png'\n")

cat("\n=== EMERGENCY SURGERY ADJUSTMENT COMPLETE ===\n")

# ========== SUBGROUP ANALYSIS: ELECTIVE SURGERIES ONLY ==========

cat("\n\n=== SUBGROUP ANALYSIS: ELECTIVE SURGERIES ONLY ===\n")
cat("Restricting analysis to elective surgeries to eliminate confounding\n\n")

# Filter to elective surgeries only
elective_only <- obs12_with_pmi %>%
  filter(emergency_surg == 0 & !is.na(PMI_type))

cat("Total elective surgery patients:", nrow(elective_only), "\n")
cat("Elective patients with Cardiac PMI:", sum(elective_only$PMI_type == "Cardiac"), "\n")
cat("Elective patients with Noncardiac PMI:", sum(elective_only$PMI_type == "Noncardiac"), "\n\n")

# Mortality by PMI type in elective surgeries only
cat("--- In-Hospital Mortality by PMI Type (Elective Surgeries Only) ---\n")
elective_mortality_pmi <- elective_only %>%
  group_by(PMI_type) %>%
  summarise(
    N = n(),
    Deaths = sum(death_in_hospital == 1, na.rm = TRUE),
    Mortality_pct = round(Deaths / N * 100, 1),
    .groups = 'drop'
  )

print(elective_mortality_pmi, n = Inf)

# Chi-square test
cat("\n--- Chi-square Test: PMI Type vs Mortality (Elective Only) ---\n")
elective_chisq_table <- elective_only %>%
  filter(!is.na(death_in_hospital)) %>%
  select(PMI_type, death_in_hospital) %>%
  table()

if(sum(elective_chisq_table) > 0) {
  elective_chisq <- chisq.test(elective_chisq_table)
  print(elective_chisq)
  cat("\nP-value:", format.pval(elective_chisq$p.value, digits = 3), "\n")
}

# Logistic regression in elective surgeries only
cat("\n--- Logistic Regression Models (Elective Surgeries Only) ---\n\n")

elective_logistic_data <- elective_only %>%
  filter(!is.na(death_in_hospital)) %>%
  mutate(
    PMI_type = factor(PMI_type, levels = c("Cardiac", "Noncardiac"))
  )

cat("Model A: Unadjusted (elective surgeries only)\n")
cat("Reference category: Cardiac PMI\n")
modelA <- glm(death_in_hospital ~ PMI_type, 
              data = elective_logistic_data, 
              family = binomial)
print(summary(modelA))

orA <- exp(coef(modelA))
ciA <- exp(confint(modelA))
cat("\nOdds Ratios with 95% CI:\n")
print(cbind(OR = orA, ci = ciA))

cat("\n\nModel B: Adjusted for age and sex (elective surgeries only)\n")
cat("Reference category: Cardiac PMI\n")
modelB <- glm(death_in_hospital ~ PMI_type + leeftijd + gender_display, 
              data = elective_logistic_data, 
              family = binomial)
print(summary(modelB))

orB <- exp(coef(modelB))
ciB <- exp(confint(modelB))
cat("\nOdds Ratios with 95% CI:\n")
print(cbind(OR = orB, ci = ciB))

cat("\n--- Subgroup Summary ---\n")
cat("Among ELECTIVE surgeries only:\n")
cat("Unadjusted OR for Noncardiac vs Cardiac PMI:", round(orA["PMI_typeNoncardiac"], 2), 
    "(95% CI:", round(ciA["PMI_typeNoncardiac", 1], 2), "-", round(ciA["PMI_typeNoncardiac", 2], 2), ")\n")
cat("Adjusted OR (age, sex):", round(orB["PMI_typeNoncardiac"], 2),
    "(95% CI:", round(ciB["PMI_typeNoncardiac", 1], 2), "-", round(ciB["PMI_typeNoncardiac", 2], 2), ")\n\n")

# Brief comparison: Emergency surgeries
cat("\n=== COMPARISON: EMERGENCY SURGERIES ===\n")

emergency_only <- obs12_with_pmi %>%
  filter(emergency_surg == 1 & !is.na(PMI_type))

cat("Total emergency surgery patients:", nrow(emergency_only), "\n")

cat("\n--- In-Hospital Mortality by PMI Type (Emergency Surgeries) ---\n")
emergency_mortality_pmi <- emergency_only %>%
  group_by(PMI_type) %>%
  summarise(
    N = n(),
    Deaths = sum(death_in_hospital == 1, na.rm = TRUE),
    Mortality_pct = round(Deaths / N * 100, 1),
    .groups = 'drop'
  )

print(emergency_mortality_pmi, n = Inf)

cat("\n=== SUBGROUP ANALYSES COMPLETE ===\n")
cat("\nKey advantage of elective subgroup: More comparable patients, less acute confounding\n")
cat("Key advantage: Shows PMI effect in 'cleaner' surgical population\n")
cat("Both approaches (adjustment + subgroup) provide complementary insights\n")

# ========== VAS PAIN SCORE ANALYSIS ==========

cat("\n\n=== VAS PAIN SCORE ANALYSIS ===\n")
cat("NOTE: Only VAS scores BEFORE first troponin collection datetime are included\n\n")

# Get first troponin collection datetime per patient from lab data
first_tropo_time <- lab %>%
  mutate(collection_collectedDateTime = as.POSIXct(collection_collectedDateTime)) %>%
  filter(!is.na(collection_collectedDateTime)) %>%
  group_by(pseudonym_value) %>%
  arrange(collection_collectedDateTime) %>%
  slice(1) %>%
  ungroup() %>%
  select(pseudonym_value, first_tropo_datetime = collection_collectedDateTime)

# Extract VAS pain scores from postoperative vitals, filtering to BEFORE first troponin time
vas_data_raw <- postoperativevitals %>%
  filter(code_display_original == "VAS") %>%
  mutate(
    VAS_value = as.numeric(valueQuantity_value),
    vas_datetime = as.POSIXct(effectiveDateTime)
  ) %>%
  select(pseudonym_value, VAS_value, vas_datetime)

# Join with first troponin time and filter
vas_data <- vas_data_raw %>%
  left_join(first_tropo_time, by = "pseudonym_value") %>%
  filter(!is.na(first_tropo_datetime)) %>%
  filter(vas_datetime < first_tropo_datetime) %>%  # ONLY VAS BEFORE troponin
  mutate(
    # Classify VAS scores into categories
    VAS_category = case_when(
      VAS_value >= 0 & VAS_value <= 3 ~ "VAS 0-3",
      VAS_value >= 4 & VAS_value <= 7 ~ "VAS 4-7",
      VAS_value > 7 ~ "VAS >7",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(VAS_category))  # Remove any invalid VAS scores

cat("Total VAS measurements BEFORE first troponin:", nrow(vas_data), "\n")
cat("Unique patients with VAS measurements BEFORE troponin:", length(unique(vas_data$pseudonym_value)), "\n")

# Link VAS data to OBS12 patients with PMI classifications
# IMPORTANT: Using HIGHEST (worst) VAS score per patient BEFORE troponin collection
vas_obs12 <- obs12_with_pmi %>%
  left_join(
    vas_data %>% 
      group_by(pseudonym_value) %>%
      summarise(
        n_VAS_measurements = n(),
        mean_VAS = round(mean(VAS_value, na.rm = TRUE), 1),
        max_VAS = max(VAS_value, na.rm = TRUE),
        # Determine worst VAS category per patient based on HIGHEST VAS score BEFORE troponin
        VAS_category = case_when(
          any(VAS_value > 7) ~ "VAS >7",
          any(VAS_value >= 4 & VAS_value <= 7) ~ "VAS 4-7",
          any(VAS_value >= 0 & VAS_value <= 3) ~ "VAS 0-3",
          TRUE ~ NA_character_
        ),
        .groups = 'drop'
      ),
    by = c("Pseudonym" = "pseudonym_value")
  )

# Patients with VAS data in OBS12
cat("\nOBS12 patients with VAS measurements (before troponin):", sum(!is.na(vas_obs12$VAS_category)), "\n")
cat("OBS12 patients without VAS measurements (before troponin):", sum(is.na(vas_obs12$VAS_category)), "\n")
cat("\n*** NOTE: Each patient classified by their HIGHEST (worst) VAS score measured BEFORE first troponin collection ***\n")

# --- 1. DISTRIBUTION OF VAS CATEGORIES IN OBS12 ---

cat("\n--- VAS Category Distribution in OBS12 ---\n")

vas_distribution_obs12 <- vas_obs12 %>%
  filter(!is.na(VAS_category)) %>%
  group_by(VAS_category) %>%
  summarise(
    N = n(),
    Percentage = round(N / nrow(vas_obs12 %>% filter(!is.na(VAS_category))) * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(match(VAS_category, c("VAS 0-3", "VAS 4-7", "VAS >7")))

print(vas_distribution_obs12, n = Inf)

# --- 2. DISTRIBUTION BY CARDIAC VS NONCARDIAC ---

cat("\n--- VAS Category Distribution by PMI Type (OBS12) ---\n")

vas_by_pmi <- vas_obs12 %>%
  filter(!is.na(VAS_category) & !is.na(PMI_type)) %>%
  group_by(PMI_type, VAS_category) %>%
  summarise(
    N = n(),
    .groups = 'drop'
  ) %>%
  group_by(PMI_type) %>%
  mutate(
    Percentage = round(N / sum(N) * 100, 1)
  ) %>%
  arrange(PMI_type, match(VAS_category, c("VAS 0-3", "VAS 4-7", "VAS >7")))

print(vas_by_pmi, n = Inf)

# Statistical test: Chi-square test for VAS category by PMI type
cat("\n--- Chi-square Test: VAS Category Distribution vs PMI Type ---\n")
vas_contingency <- vas_obs12 %>%
  filter(!is.na(VAS_category) & !is.na(PMI_type)) %>%
  select(VAS_category, PMI_type) %>%
  table()

if(nrow(vas_contingency) > 0 && ncol(vas_contingency) > 0) {
  chisq_test_pmi <- chisq.test(vas_contingency)
  print(chisq_test_pmi)
  p_value_pmi_distribution <- chisq_test_pmi$p.value
  cat("\n*** P-VALUE for VAS distribution by PMI type: ", 
      format.pval(p_value_pmi_distribution, digits = 3), " ***\n", sep = "")
} else {
  cat("Insufficient data for chi-square test\n")
  p_value_pmi_distribution <- NA
}

# --- 3. VAS CATEGORIES AND IN-HOSPITAL MORTALITY ---

cat("\n--- In-Hospital Mortality by VAS Category (OBS12) ---\n")

vas_mortality_overall <- vas_obs12 %>%
  filter(!is.na(VAS_category)) %>%
  group_by(VAS_category) %>%
  summarise(
    N = n(),
    Deaths = sum(death_in_hospital == 1, na.rm = TRUE),
    Mortality_pct = round(Deaths / N * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(match(VAS_category, c("VAS 0-3", "VAS 4-7", "VAS >7")))

print(vas_mortality_overall, n = Inf)

# Chi-square test: VAS category vs in-hospital mortality
cat("\n--- Chi-square Test: VAS Category vs In-Hospital Mortality ---\n")
vas_mortality_table <- vas_obs12 %>%
  filter(!is.na(VAS_category)) %>%
  mutate(death_in_hospital = factor(death_in_hospital, levels = c(0, 1))) %>%
  select(VAS_category, death_in_hospital) %>%
  table()

if(sum(vas_mortality_table) > 0) {
  chisq_mort <- chisq.test(vas_mortality_table)
  print(chisq_mort)
  p_value_mortality_overall <- chisq_mort$p.value
  cat("\n*** P-VALUE for VAS category vs in-hospital mortality: ", 
      format.pval(p_value_mortality_overall, digits = 3), " ***\n", sep = "")
} else {
  cat("Insufficient data for chi-square test\n")
  p_value_mortality_overall <- NA
}

# --- 4. VAS CATEGORIES AND MORTALITY STRATIFIED BY PMI TYPE ---

cat("\n--- In-Hospital Mortality by VAS Category, Stratified by PMI Type (OBS12) ---\n")

vas_mortality_stratified <- vas_obs12 %>%
  filter(!is.na(VAS_category) & !is.na(PMI_type)) %>%
  group_by(PMI_type, VAS_category) %>%
  summarise(
    N = n(),
    Deaths = sum(death_in_hospital == 1, na.rm = TRUE),
    Mortality_pct = round(Deaths / N * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(PMI_type, match(VAS_category, c("VAS 0-3", "VAS 4-7", "VAS >7")))

print(vas_mortality_stratified, n = Inf)

# Separate chi-square tests for cardiac and noncardiac
cat("\n--- Chi-square Tests by PMI Type ---\n")

for(pmi_type in c("Cardiac", "Noncardiac")) {
  cat("\n", pmi_type, " PMI:\n", sep="")
  
  vas_mort_subset <- vas_obs12 %>%
    filter(!is.na(VAS_category) & PMI_type == pmi_type) %>%
    mutate(death_in_hospital = factor(death_in_hospital, levels = c(0, 1))) %>%
    select(VAS_category, death_in_hospital) %>%
    table()
  
  if(sum(vas_mort_subset) > 0 && nrow(vas_mort_subset) > 1) {
    chisq_subset <- tryCatch(
      chisq.test(vas_mort_subset),
      error = function(e) {
        cat("  Unable to perform chi-square test:", e$message, "\n")
        return(NULL)
      }
    )
    if(!is.null(chisq_subset)) {
      print(chisq_subset)
    }
  } else {
    cat("  Insufficient data for chi-square test\n")
  }
}

# --- 5. VISUALIZATION: VAS CATEGORIES AND MORTALITY ---

cat("\n--- Creating VAS Pain Score Visualization ---\n")

# Prepare data for visualization
vas_plot_data <- vas_mortality_stratified %>%
  mutate(
    VAS_category = factor(VAS_category, levels = c("VAS 0-3", "VAS 4-7", "VAS >7"))
  )

# Create subtitle with p-value for mortality plot
if(!is.na(p_value_mortality_overall)) {
  p_text_mortality <- paste0("Chi-square test p-value = ", format.pval(p_value_mortality_overall, digits = 3))
} else {
  p_text_mortality <- ""
}

# Create bar chart: Mortality by VAS category and PMI type
vas_mortality_plot <- ggplot(vas_plot_data, 
                              aes(x = VAS_category, 
                                  y = Mortality_pct, 
                                  fill = PMI_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Cardiac" = "#FFB6C1", "Noncardiac" = "#ADD8E6")) +
  labs(
    title = "In-Hospital Mortality by VAS Pain Score Category",
    subtitle = paste0("Stratified by PMI Type (OBS12) - VAS before troponin\n", p_text_mortality),
    x = "VAS Pain Score Category",
    y = "In-Hospital Mortality (%)",
    fill = "PMI Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text = element_text(size = 11),
    legend.position = "bottom"
  ) +
  geom_text(aes(label = paste0(Deaths, "/", N, "\n(", Mortality_pct, "%)")),
            position = position_dodge(width = 0.7),
            vjust = -0.5,
            size = 3)

# Save plot
ggsave("VAS_Mortality_by_PMI_Type.png", 
       plot = vas_mortality_plot, 
       width = 10, 
       height = 7, 
       dpi = 300)

cat("VAS mortality plot saved as 'VAS_Mortality_by_PMI_Type.png'\n")

# Create subtitle with p-value for distribution plot
if(!is.na(p_value_pmi_distribution)) {
  p_text_distribution <- paste0("Chi-square test p-value = ", format.pval(p_value_pmi_distribution, digits = 3))
} else {
  p_text_distribution <- ""
}

# Create distribution plot
vas_distribution_plot <- ggplot(vas_by_pmi, 
                                 aes(x = VAS_category, 
                                     y = Percentage, 
                                     fill = PMI_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Cardiac" = "#FFB6C1", "Noncardiac" = "#ADD8E6")) +
  labs(
    title = "Distribution of VAS Pain Scores by PMI Type",
    subtitle = paste0("OBS12 Patients - VAS before troponin\n", p_text_distribution),
    x = "VAS Pain Score Category",
    y = "Percentage (%)",
    fill = "PMI Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text = element_text(size = 11),
    legend.position = "bottom"
  ) +
  geom_text(aes(label = paste0(N, "\n(", Percentage, "%)")),
            position = position_dodge(width = 0.7),
            vjust = -0.5,
            size = 3)

ggsave("VAS_Distribution_by_PMI_Type.png", 
       plot = vas_distribution_plot, 
       width = 10, 
       height = 7, 
       dpi = 300)

cat("VAS distribution plot saved as 'VAS_Distribution_by_PMI_Type.png'\n")

cat("\n=== VAS PAIN SCORE ANALYSIS COMPLETE ===\n")

# ========== ANALYSIS 1: KAPLAN-MEIER 30-DAY IN-HOSPITAL MORTALITY ==========
# Cardiac vs Extracardiac with proper censoring for discharge alive

cat("\n\n=== ANALYSIS 1: KAPLAN-MEIER 30-DAY IN-HOSPITAL MORTALITY ===\n")
cat("Cardiac vs Extracardiac PMI\n")
cat("NOTE: Patients who die have event at day of death.\n")
cat("      Patients discharged alive are censored at day of discharge.\n")
cat("      Patients still hospitalized at day 30 are censored at day 30.\n\n")

# Build time-to-event data from opname
# Need: surgery date (from verrichtingen or Date), discharge/death date, and outcome
km_data <- obs12_with_pmi %>%
  left_join(
    opname %>%
      mutate(
        opname_begindatum = as.Date(opname_begindatum),
        opname_einddatum = as.Date(opname_einddatum),
        died_in_hospital = opname_bestemming %in% c("Overleden (zonder obductie)", "Overleden (met obductie)")
      ) %>%
      group_by(pseudonym_value) %>%
      summarise(
        admission_date = min(opname_begindatum, na.rm = TRUE),
        discharge_date = max(opname_einddatum, na.rm = TRUE),
        died = any(died_in_hospital),
        .groups = "drop"
      ),
    by = c("Pseudonym" = "pseudonym_value")
  ) %>%
  filter(!is.na(admission_date)) %>%
  mutate(
    # Use Date (troponin/surgery reference date) as time zero
    surgery_date = as.Date(Date),
    # Time from surgery to end of follow-up
    time_to_event = as.numeric(
      pmin(discharge_date, surgery_date + 30, na.rm = TRUE) - surgery_date
    ),
    # Ensure minimum time of 0
    time_to_event = pmax(time_to_event, 0),
    # Cap at 30 days
    time_to_event = pmin(time_to_event, 30),
    # Status: 1 = death within 30 days, 0 = censored (discharged alive or still hospitalized)
    km_status = case_when(
      died & time_to_event <= 30 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  filter(!is.na(time_to_event) & !is.na(PMI_type))

cat("Patients in KM analysis:", nrow(km_data), "\n")
cat("  Cardiac PMI:", sum(km_data$PMI_type == "Cardiac"), "\n")
cat("  Noncardiac PMI:", sum(km_data$PMI_type == "Noncardiac"), "\n")
cat("  Deaths within 30 days:", sum(km_data$km_status == 1), "\n")
cat("  Censored (discharged alive or day 30):", sum(km_data$km_status == 0), "\n\n")

# Fit Kaplan-Meier
km_fit <- survfit(Surv(time_to_event, km_status) ~ PMI_type, data = km_data)
cat("--- Kaplan-Meier Summary ---\n")
print(km_fit)

# Log-rank test
km_logrank <- survdiff(Surv(time_to_event, km_status) ~ PMI_type, data = km_data)
cat("\n--- Log-rank Test ---\n")
print(km_logrank)
km_pvalue <- 1 - pchisq(km_logrank$chisq, length(km_logrank$n) - 1)
cat("P-value:", format.pval(km_pvalue, digits = 3), "\n")

# Plot KM curve - BJA style
km_plot <- ggsurvplot(
  km_fit,
  data = km_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  xlim = c(0, 30),
  break.time.by = 5,
  palette = c("#E64B35", "#4DBBD5"),
  legend.labs = c("Cardiac", "Extracardiac"),
  legend.title = "PMI Aetiology",
  xlab = "Days from surgery",
  ylab = "Survival probability",
  title = "30-Day In-Hospital Mortality: Cardiac vs Extracardiac PMI",
  subtitle = "Kaplan-Meier curve with 95% CI",
  ggtheme = theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10),
      legend.position = "bottom"
    ),
  risk.table.height = 0.25,
  risk.table.title = "Number at risk",
  censor.shape = "+",
  censor.size = 3
)

print(km_plot)
ggsave("KM_30day_Cardiac_vs_Extracardiac.png",
       plot = print(km_plot, newpage = FALSE),
       width = 10, height = 8, dpi = 300)
cat("KM curve saved as 'KM_30day_Cardiac_vs_Extracardiac.png'\n")

cat("\n=== KM ANALYSIS COMPLETE ===\n")
cat("NOTE: Patients discharged alive before day 30 are censored at discharge.\n")
cat("      This is visible as '+' marks on the KM curve.\n")
cat("      This means the KM curve represents probability of remaining alive\n")
cat("      IN HOSPITAL - not overall survival.\n")

# ========== ANALYSIS 2: COMPETING RISKS ANALYSIS ==========
# Addresses bias from informative censoring (discharge alive competes with death)

cat("\n\n=== ANALYSIS 2: COMPETING RISKS ANALYSIS (30-DAY IN-HOSPITAL) ===\n")
cat("Addresses informative censoring bias from KM analysis\n")
cat("Events: 1 = in-hospital death, 2 = discharge alive, 0 = still hospitalized at day 30\n\n")

# Build competing risks data
cr_data <- obs12_with_pmi %>%
  left_join(
    opname %>%
      mutate(
        opname_begindatum = as.Date(opname_begindatum),
        opname_einddatum = as.Date(opname_einddatum),
        died_in_hospital = opname_bestemming %in% c("Overleden (zonder obductie)", "Overleden (met obductie)")
      ) %>%
      group_by(pseudonym_value) %>%
      summarise(
        admission_date = min(opname_begindatum, na.rm = TRUE),
        discharge_date = max(opname_einddatum, na.rm = TRUE),
        died = any(died_in_hospital),
        .groups = "drop"
      ),
    by = c("Pseudonym" = "pseudonym_value")
  ) %>%
  filter(!is.na(admission_date)) %>%
  mutate(
    surgery_date = as.Date(Date),
    days_to_end = as.numeric(discharge_date - surgery_date),
    days_to_end = pmax(days_to_end, 0),
    # Competing risks status: 0 = censored at 30, 1 = death, 2 = discharged alive
    cr_status = case_when(
      died & days_to_end <= 30 ~ 1,              # died in hospital within 30 days
      !died & days_to_end <= 30 ~ 2,             # discharged alive within 30 days
      TRUE ~ 0                                    # still hospitalized at day 30 (censored)
    ),
    cr_time = case_when(
      cr_status == 0 ~ 30,                       # censored at day 30
      TRUE ~ pmin(days_to_end, 30)               # event time
    ),
    cr_time = pmax(cr_time, 0.5)                 # avoid zero times for CIF estimation
  ) %>%
  filter(!is.na(cr_time) & !is.na(PMI_type))

cat("Patients in competing risks analysis:", nrow(cr_data), "\n")
cat("  Status 0 (still hospitalized at day 30):", sum(cr_data$cr_status == 0), "\n")
cat("  Status 1 (in-hospital death):", sum(cr_data$cr_status == 1), "\n")
cat("  Status 2 (discharged alive):", sum(cr_data$cr_status == 2), "\n")
cat("  Cardiac PMI:", sum(cr_data$PMI_type == "Cardiac"), "\n")
cat("  Extracardiac PMI:", sum(cr_data$PMI_type == "Noncardiac"), "\n\n")

# --- Cumulative Incidence Functions (CIF) ---
cat("--- Cumulative Incidence Functions ---\n\n")

# Create group variable
cr_group <- factor(cr_data$PMI_type, levels = c("Cardiac", "Noncardiac"))

# Calculate CIF
cif_fit <- cuminc(ftime = cr_data$cr_time,
                  fstatus = cr_data$cr_status,
                  group = cr_group)

# Print CIF summary
cat("CIF estimates at key timepoints:\n")
cif_summary <- timepoints(cif_fit, times = c(5, 10, 15, 20, 25, 30))
print(cif_summary)

# --- Gray's Test ---
cat("\n--- Gray's Test ---\n")
cat("Comparing cumulative incidence between Cardiac and Extracardiac PMI\n\n")

# Gray's test results are embedded in cuminc output
gray_tests <- cif_fit$Tests
cat("Gray's test for in-hospital DEATH:\n")
cat("  Statistic:", round(gray_tests[1, "stat"], 3), "\n")
cat("  P-value:", format.pval(gray_tests[1, "pv"], digits = 3), "\n")
cat("\nGray's test for DISCHARGE ALIVE:\n")
cat("  Statistic:", round(gray_tests[2, "stat"], 3), "\n")
cat("  P-value:", format.pval(gray_tests[2, "pv"], digits = 3), "\n\n")

gray_p_death <- gray_tests[1, "pv"]
gray_p_discharge <- gray_tests[2, "pv"]

# --- Fine-Gray Regression Models ---
cat("\n--- Fine-Gray Subdistribution Hazard Regression ---\n\n")

# Prepare covariates
cr_data_fg <- cr_data %>%
  mutate(
    cardiac_group = if_else(PMI_type == "Cardiac", 1, 0),
    age = as.numeric(leeftijd),
    female = if_else(gender_display == "Vrouw", 1, 0),
    emergency = as.numeric(emergency_surg)
  ) %>%
  filter(!is.na(age) & !is.na(female) & !is.na(emergency))

# --- Models for DEATH (event = 1) ---
cat("=== DEATH OUTCOME (event = 1) ===\n\n")

# Unadjusted model for death
cat("Model 1: Unadjusted sHR for Cardiac vs Extracardiac (Death)\n")
fg_death_unadj <- crr(
  ftime = cr_data_fg$cr_time,
  fstatus = cr_data_fg$cr_status,
  cov1 = cr_data_fg$cardiac_group,
  failcode = 1,
  cencode = 0
)
cat("  sHR (Cardiac vs Extracardiac):", round(exp(fg_death_unadj$coef), 2), "\n")
cat("  95% CI: [", round(exp(fg_death_unadj$coef - 1.96 * sqrt(fg_death_unadj$var)), 2),
    ", ", round(exp(fg_death_unadj$coef + 1.96 * sqrt(fg_death_unadj$var)), 2), "]\n")
cat("  P-value:", format.pval(fg_death_unadj$coef / sqrt(fg_death_unadj$var) |>
                                (\(z) 2 * pnorm(abs(z), lower.tail = FALSE))(), digits = 3), "\n")
print(summary(fg_death_unadj))

# Adjusted model for death
cat("\nModel 2: Adjusted sHR for Cardiac vs Extracardiac (Death)\n")
cat("Adjusted for: age, sex, emergency surgery status\n")
cov_matrix_death <- as.matrix(cr_data_fg[, c("cardiac_group", "age", "female", "emergency")])
fg_death_adj <- crr(
  ftime = cr_data_fg$cr_time,
  fstatus = cr_data_fg$cr_status,
  cov1 = cov_matrix_death,
  failcode = 1,
  cencode = 0
)
print(summary(fg_death_adj))

fg_death_adj_results <- data.frame(
  Variable = c("Cardiac (vs Extracardiac)", "Age (per year)", "Female (vs Male)", "Emergency surgery"),
  sHR = round(exp(fg_death_adj$coef), 2),
  CI_lower = round(exp(fg_death_adj$coef - 1.96 * sqrt(diag(fg_death_adj$var))), 2),
  CI_upper = round(exp(fg_death_adj$coef + 1.96 * sqrt(diag(fg_death_adj$var))), 2),
  p_value = round(2 * pnorm(abs(fg_death_adj$coef / sqrt(diag(fg_death_adj$var))), lower.tail = FALSE), 4)
)
cat("\nAdjusted Fine-Gray model summary (Death):\n")
print(fg_death_adj_results, row.names = FALSE)

# --- Models for DISCHARGE ALIVE (event = 2) ---
cat("\n\n=== DISCHARGE ALIVE OUTCOME (event = 2) ===\n\n")

# Unadjusted model for discharge
cat("Model 3: Unadjusted sHR for Cardiac vs Extracardiac (Discharge Alive)\n")
fg_discharge_unadj <- crr(
  ftime = cr_data_fg$cr_time,
  fstatus = cr_data_fg$cr_status,
  cov1 = cr_data_fg$cardiac_group,
  failcode = 2,
  cencode = 0
)
print(summary(fg_discharge_unadj))

# Adjusted model for discharge
cat("\nModel 4: Adjusted sHR for Cardiac vs Extracardiac (Discharge Alive)\n")
fg_discharge_adj <- crr(
  ftime = cr_data_fg$cr_time,
  fstatus = cr_data_fg$cr_status,
  cov1 = cov_matrix_death,
  failcode = 2,
  cencode = 0
)
print(summary(fg_discharge_adj))

fg_discharge_adj_results <- data.frame(
  Variable = c("Cardiac (vs Extracardiac)", "Age (per year)", "Female (vs Male)", "Emergency surgery"),
  sHR = round(exp(fg_discharge_adj$coef), 2),
  CI_lower = round(exp(fg_discharge_adj$coef - 1.96 * sqrt(diag(fg_discharge_adj$var))), 2),
  CI_upper = round(exp(fg_discharge_adj$coef + 1.96 * sqrt(diag(fg_discharge_adj$var))), 2),
  p_value = round(2 * pnorm(abs(fg_discharge_adj$coef / sqrt(diag(fg_discharge_adj$var))), lower.tail = FALSE), 4)
)
cat("\nAdjusted Fine-Gray model summary (Discharge Alive):\n")
print(fg_discharge_adj_results, row.names = FALSE)

# --- Publication-Ready Two-Panel CIF Figure (BJA style) ---
cat("\n--- Creating Publication-Ready CIF Figure ---\n")

# Extract CIF data for plotting
cif_plot_data <- data.frame(
  time = numeric(),
  est = numeric(),
  group = character(),
  event = character()
)

# Extract from cuminc object
for (nm in names(cif_fit)) {
  if (nm == "Tests") next
  parts <- strsplit(nm, " ")[[1]]
  grp <- parts[1]
  evt <- parts[2]
  event_label <- ifelse(evt == "1", "Death", "Discharge Alive")
  df_tmp <- data.frame(
    time = cif_fit[[nm]]$time,
    est = cif_fit[[nm]]$est,
    group = grp,
    event = event_label
  )
  cif_plot_data <- rbind(cif_plot_data, df_tmp)
}

# Filter to 0-30 days
cif_plot_data <- cif_plot_data %>%
  filter(time <= 30)

# Panel A: Cumulative incidence of in-hospital death
panel_a_data <- cif_plot_data %>% filter(event == "Death")

panel_a <- ggplot(panel_a_data, aes(x = time, y = est * 100, color = group)) +
  geom_step(linewidth = 1) +
  scale_color_manual(
    values = c("Cardiac" = "#E64B35", "Noncardiac" = "#4DBBD5"),
    labels = c("Cardiac" = "Cardiac", "Noncardiac" = "Extracardiac")
  ) +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  labs(
    title = "A) Cumulative Incidence of In-Hospital Death",
    x = "Days from surgery",
    y = "Cumulative incidence (%)",
    color = "PMI Aetiology"
  ) +
  annotate("text", x = 15, y = 9,
           label = paste0("Gray's test p = ", format.pval(gray_p_death, digits = 3)),
           size = 3.5, hjust = 0) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

# Panel B: Cumulative incidence of discharge alive
panel_b_data <- cif_plot_data %>% filter(event == "Discharge Alive")

panel_b <- ggplot(panel_b_data, aes(x = time, y = est * 100, color = group)) +
  geom_step(linewidth = 1) +
  scale_color_manual(
    values = c("Cardiac" = "#E64B35", "Noncardiac" = "#4DBBD5"),
    labels = c("Cardiac" = "Cardiac", "Noncardiac" = "Extracardiac")
  ) +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(
    title = "B) Cumulative Incidence of Discharge Alive",
    x = "Days from surgery",
    y = "Cumulative incidence (%)",
    color = "PMI Aetiology"
  ) +
  annotate("text", x = 15, y = 90,
           label = paste0("Gray's test p = ", format.pval(gray_p_discharge, digits = 3)),
           size = 3.5, hjust = 0) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

# Combine panels side by side
if (!require("patchwork")) install.packages("patchwork")
library(patchwork)

cif_combined <- panel_a + panel_b +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("CIF_Competing_Risks_Death_Discharge.png",
       plot = cif_combined,
       width = 14, height = 6, dpi = 300)
cat("Publication-ready CIF figure saved as 'CIF_Competing_Risks_Death_Discharge.png'\n")
print(cif_combined)

cat("\n=== COMPETING RISKS ANALYSIS COMPLETE ===\n")

# ========== ANALYSIS 3: PEAK hsTnT PER PATIENT WITH DAYS AFTER SURGERY ==========

cat("\n\n=== ANALYSIS 3: PEAK hsTnT VALUE PER PATIENT ===\n")
cat("Highest hsTnT (valueQuantity_value) per unique patient from lab data\n")
cat("Days to peak calculated from surgery date in verrichtingen\n\n")

# Get surgery date from verrichtingen per pseudonym_value
surgery_dates <- verrichtingen %>%
  mutate(
    surgery_date_verr = as.Date(as.character(verrichting_begindatumtijd))
  ) %>%
  group_by(pseudonym_value) %>%
  summarise(
    surgery_date_verr = min(surgery_date_verr, na.rm = TRUE),
    .groups = "drop"
  )

cat("Patients with surgery dates from verrichtingen:", nrow(surgery_dates), "\n")

# Get peak hsTnT per patient from lab
peak_hstnt <- lab %>%
  mutate(
    valueQuantity_value = as.numeric(gsub(",", ".", valueQuantity_value)),
    collection_collectedDateTime = as.POSIXct(collection_collectedDateTime),
    lab_date = as.Date(collection_collectedDateTime)
  ) %>%
  filter(!is.na(valueQuantity_value)) %>%
  group_by(pseudonym_value) %>%
  arrange(desc(valueQuantity_value)) %>%
  slice(1) %>%
  ungroup() %>%
  select(pseudonym_value,
         peak_hstnt_value = valueQuantity_value,
         peak_hstnt_datetime = collection_collectedDateTime,
         peak_hstnt_date = lab_date)

cat("Patients with peak hsTnT data:", nrow(peak_hstnt), "\n")

# Merge with surgery dates and calculate days to peak
peak_hstnt_with_surgery <- peak_hstnt %>%
  inner_join(surgery_dates, by = "pseudonym_value") %>%
  mutate(
    days_to_peak = as.numeric(peak_hstnt_date - surgery_date_verr)
  )

cat("Patients with both peak hsTnT and surgery date:", nrow(peak_hstnt_with_surgery), "\n\n")

# Filter to only included patients (obs12_with_pmi)
peak_hstnt_included <- peak_hstnt_with_surgery %>%
  inner_join(
    obs12_with_pmi %>% select(Pseudonym, PMI_type) %>% distinct(),
    by = c("pseudonym_value" = "Pseudonym")
  )

cat("Included patients with peak hsTnT data:", nrow(peak_hstnt_included), "\n\n")

# Summary statistics
cat("--- Peak hsTnT Summary (included patients) ---\n")
cat("Overall:\n")
cat("  N:", nrow(peak_hstnt_included), "\n")
cat("  Median peak hsTnT:", round(median(peak_hstnt_included$peak_hstnt_value, na.rm = TRUE), 1), "\n")
cat("  IQR:", round(quantile(peak_hstnt_included$peak_hstnt_value, 0.25, na.rm = TRUE), 1), "-",
    round(quantile(peak_hstnt_included$peak_hstnt_value, 0.75, na.rm = TRUE), 1), "\n")
cat("  Mean days to peak:", round(mean(peak_hstnt_included$days_to_peak, na.rm = TRUE), 1), "\n")
cat("  Median days to peak:", round(median(peak_hstnt_included$days_to_peak, na.rm = TRUE), 1), "\n\n")

# By PMI type
cat("--- Peak hsTnT by PMI Type ---\n")
peak_hstnt_by_pmi <- peak_hstnt_included %>%
  group_by(PMI_type) %>%
  summarise(
    N = n(),
    Median_peak_hsTnT = round(median(peak_hstnt_value, na.rm = TRUE), 1),
    IQR_lower = round(quantile(peak_hstnt_value, 0.25, na.rm = TRUE), 1),
    IQR_upper = round(quantile(peak_hstnt_value, 0.75, na.rm = TRUE), 1),
    Mean_days_to_peak = round(mean(days_to_peak, na.rm = TRUE), 1),
    Median_days_to_peak = round(median(days_to_peak, na.rm = TRUE), 1),
    .groups = "drop"
  )
print(peak_hstnt_by_pmi, n = Inf)

# Detailed table per patient (first 20)
cat("\n--- Sample: Peak hsTnT per patient (first 20) ---\n")
print(
  peak_hstnt_included %>%
    select(pseudonym_value, PMI_type, peak_hstnt_value, days_to_peak) %>%
    arrange(desc(peak_hstnt_value)) %>%
    head(20),
  n = 20
)

# Distribution of days to peak
cat("\n--- Distribution of Days to Peak hsTnT ---\n")
days_to_peak_dist <- peak_hstnt_included %>%
  mutate(days_category = case_when(
    days_to_peak <= 0 ~ "Day 0 (surgery day)",
    days_to_peak == 1 ~ "Day 1",
    days_to_peak == 2 ~ "Day 2",
    days_to_peak == 3 ~ "Day 3",
    days_to_peak <= 7 ~ "Day 4-7",
    days_to_peak <= 14 ~ "Day 8-14",
    TRUE ~ "Day >14"
  )) %>%
  count(days_category, sort = FALSE) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))
print(days_to_peak_dist, n = Inf)

# Merge peak hsTnT data back to obs12_with_pmi for downstream use
obs12_with_pmi <- obs12_with_pmi %>%
  left_join(
    peak_hstnt_with_surgery %>%
      select(pseudonym_value, peak_hstnt_value, days_to_peak),
    by = c("Pseudonym" = "pseudonym_value")
  )

cat("\n=== PEAK hsTnT ANALYSIS COMPLETE ===\n")

# ========== ANALYSIS 4: LOGISTIC REGRESSION ON IN-HOSPITAL MORTALITY ==========

cat("\n\n=== ANALYSIS 4: LOGISTIC REGRESSION - IN-HOSPITAL MORTALITY ===\n")
cat("Comprehensive logistic regression models for in-hospital mortality\n\n")

# Prepare data for logistic regression
logistic_data <- obs12_with_pmi %>%
  filter(!is.na(death_in_hospital)) %>%
  mutate(
    PMI_type = factor(PMI_type, levels = c("Noncardiac", "Cardiac")),
    age = as.numeric(leeftijd),
    female = if_else(gender_display == "Vrouw", 1, 0),
    emergency = as.numeric(emergency_surg),
    has_CAD = as.numeric(`history#Coronary Artery Disease`),
    has_CHF = as.numeric(`history#Chronic Heart Failure`),
    has_CKD = as.numeric(`history#Chronic Kidney Disease`),
    has_DM = pmax(as.numeric(`history#Diabetus Mellitus, non-insulin`),
                  as.numeric(`history#Diabetus Mellitus, insulin dependent`), na.rm = TRUE),
    has_hypertension = as.numeric(`history#Hypertension`),
    has_afib = as.numeric(`history#Atrial Fibrilation`),
    has_COPD = as.numeric(`history#Chronic Obstructive Pulmonary Disease`),
    hypotension = as.numeric(any_MAP_below_65),
    tachycardia = as.numeric(any_HR_above_120)
  )

cat("Patients in logistic regression:", nrow(logistic_data), "\n")
cat("Deaths:", sum(logistic_data$death_in_hospital == 1), "\n")
cat("Survivors:", sum(logistic_data$death_in_hospital == 0), "\n\n")

# Model L1: Univariable - PMI type only
cat("--- Model L1: Univariable (PMI type) ---\n")
cat("Reference: Noncardiac PMI\n")
logit_L1 <- glm(death_in_hospital ~ PMI_type, data = logistic_data, family = binomial)
or_L1 <- exp(cbind(OR = coef(logit_L1), confint(logit_L1)))
print(summary(logit_L1))
cat("\nOR (95% CI):\n")
print(round(or_L1, 3))

# Model L2: Adjusted for age and sex
cat("\n--- Model L2: Adjusted for age and sex ---\n")
logit_L2 <- glm(death_in_hospital ~ PMI_type + age + female, data = logistic_data, family = binomial)
or_L2 <- exp(cbind(OR = coef(logit_L2), confint(logit_L2)))
print(summary(logit_L2))
cat("\nOR (95% CI):\n")
print(round(or_L2, 3))

# Model L3: Adjusted for age, sex, emergency surgery
cat("\n--- Model L3: Adjusted for age, sex, emergency surgery ---\n")
logit_L3 <- glm(death_in_hospital ~ PMI_type + age + female + emergency,
                 data = logistic_data, family = binomial)
or_L3 <- exp(cbind(OR = coef(logit_L3), confint(logit_L3)))
print(summary(logit_L3))
cat("\nOR (95% CI):\n")
print(round(or_L3, 3))

# Model L4: Full model with comorbidities
cat("\n--- Model L4: Full multivariable model ---\n")
cat("Adjusted for: age, sex, emergency, CAD, CHF, CKD, DM, hypertension, RCRI\n")
logit_L4 <- glm(death_in_hospital ~ PMI_type + age + female + emergency +
                   has_CAD + has_CHF + has_CKD + has_DM + has_hypertension + RCRI_score,
                 data = logistic_data, family = binomial)
or_L4 <- exp(cbind(OR = coef(logit_L4), confint(logit_L4)))
print(summary(logit_L4))
cat("\nOR (95% CI):\n")
print(round(or_L4, 3))

# Model L5: Including postoperative vitals
cat("\n--- Model L5: Including postoperative hemodynamic instability ---\n")
logistic_data_vitals <- logistic_data %>%
  filter(!is.na(hypotension) & !is.na(tachycardia))

logit_L5 <- glm(death_in_hospital ~ PMI_type + age + female + emergency +
                   has_CAD + has_CHF + has_CKD + RCRI_score +
                   hypotension + tachycardia,
                 data = logistic_data_vitals, family = binomial)
or_L5 <- exp(cbind(OR = coef(logit_L5), confint(logit_L5)))
print(summary(logit_L5))
cat("\nOR (95% CI):\n")
print(round(or_L5, 3))

# Summary comparison table
cat("\n--- Summary: Cardiac PMI OR across models ---\n")
model_comparison <- data.frame(
  Model = c("L1: Univariable",
            "L2: + age, sex",
            "L3: + age, sex, emergency",
            "L4: + comorbidities, RCRI",
            "L5: + hemodynamic instability"),
  OR_Cardiac = c(
    round(or_L1["PMI_typeCardiac", "OR"], 2),
    round(or_L2["PMI_typeCardiac", "OR"], 2),
    round(or_L3["PMI_typeCardiac", "OR"], 2),
    round(or_L4["PMI_typeCardiac", "OR"], 2),
    round(or_L5["PMI_typeCardiac", "OR"], 2)
  ),
  CI_lower = c(
    round(or_L1["PMI_typeCardiac", 2], 2),
    round(or_L2["PMI_typeCardiac", 2], 2),
    round(or_L3["PMI_typeCardiac", 2], 2),
    round(or_L4["PMI_typeCardiac", 2], 2),
    round(or_L5["PMI_typeCardiac", 2], 2)
  ),
  CI_upper = c(
    round(or_L1["PMI_typeCardiac", 3], 2),
    round(or_L2["PMI_typeCardiac", 3], 2),
    round(or_L3["PMI_typeCardiac", 3], 2),
    round(or_L4["PMI_typeCardiac", 3], 2),
    round(or_L5["PMI_typeCardiac", 3], 2)
  ),
  AIC = c(AIC(logit_L1), AIC(logit_L2), AIC(logit_L3), AIC(logit_L4), AIC(logit_L5)),
  N = c(nrow(logistic_data), nrow(logistic_data), nrow(logistic_data),
        nrow(logistic_data), nrow(logistic_data_vitals))
)
print(model_comparison, row.names = FALSE)

# Hosmer-Lemeshow goodness-of-fit for best model
cat("\n--- Model Fit Assessment (Model L4) ---\n")
cat("AIC:", AIC(logit_L4), "\n")
cat("Null deviance:", logit_L4$null.deviance, "on", logit_L4$df.null, "df\n")
cat("Residual deviance:", logit_L4$deviance, "on", logit_L4$df.residual, "df\n")
cat("Pseudo R-squared (McFadden):",
    round(1 - logit_L4$deviance / logit_L4$null.deviance, 3), "\n")

# Forest plot of OR from Model L4
cat("\n--- Creating Forest Plot (Model L4) ---\n")

forest_data <- data.frame(
  Variable = names(coef(logit_L4))[-1],
  OR = exp(coef(logit_L4))[-1],
  CI_lower = exp(confint(logit_L4))[-1, 1],
  CI_upper = exp(confint(logit_L4))[-1, 2]
) %>%
  mutate(
    Variable = case_when(
      Variable == "PMI_typeCardiac" ~ "Cardiac PMI (vs Noncardiac)",
      Variable == "age" ~ "Age (per year)",
      Variable == "female" ~ "Female sex",
      Variable == "emergency" ~ "Emergency surgery",
      Variable == "has_CAD" ~ "Coronary artery disease",
      Variable == "has_CHF" ~ "Heart failure",
      Variable == "has_CKD" ~ "Chronic kidney disease",
      Variable == "has_DM" ~ "Diabetes mellitus",
      Variable == "has_hypertension" ~ "Hypertension",
      Variable == "RCRI_score" ~ "RCRI score (per point)",
      TRUE ~ Variable
    )
  )

forest_plot <- ggplot(forest_data, aes(x = OR, y = reorder(Variable, OR))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  scale_x_log10() +
  labs(
    title = "In-Hospital Mortality: Multivariable Logistic Regression",
    subtitle = "Odds Ratios with 95% CI (Model L4)",
    x = "Odds Ratio (log scale)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 10)
  )

print(forest_plot)
ggsave("Forest_Plot_InHospital_Mortality.png",
       plot = forest_plot,
       width = 10, height = 6, dpi = 300)
cat("Forest plot saved as 'Forest_Plot_InHospital_Mortality.png'\n")

cat("\n=== LOGISTIC REGRESSION ANALYSIS COMPLETE ===\n")

# ========== ANALYSIS 5: OBS12 vs AGREED - MISCLASSIFICATION ANALYSIS ==========

cat("\n\n=== ANALYSIS 5: MISCLASSIFICATION ANALYSIS (OBS12 vs AGREED) ===\n")
cat("Detailed classification of non-agreed cases between OBS12 and OBS34\n")
cat("Identifies where most disagreements occur\n\n")

# Start from the comparison_pmi object which has both observer assessments
# Identify non-agreed cases
nonagreed <- comparison_pmi %>%
  filter(!agreed | is.na(agreed))

cat("Total patients with both assessments:", nrow(comparison_pmi), "\n")
cat("Agreed patients:", sum(comparison_pmi$agreed, na.rm = TRUE), "\n")
cat("Non-agreed patients:", nrow(nonagreed), "\n\n")

# Get detailed sub-aetiology information for non-agreed cases
nonagreed_detail <- nonagreed %>%
  left_join(
    obs12 %>%
      select(`Participant Id`,
             extra_12_detail = cause_extra_car,
             cardiac_12_detail = cause_cardiac,
             T2MI_12 = cause_T2MI,
             cause_extra_car_yes_12 = cause_extra_car_yes,
             Cause_cardiac_yes_12 = Cause_cardiac_yes) %>%
      distinct(`Participant Id`, .keep_all = TRUE),
    by = "Participant Id"
  ) %>%
  left_join(
    obs34 %>%
      select(`Participant Id`,
             extra_34_detail = cause_extra_car,
             cardiac_34_detail = cause_cardiac,
             T2MI_34 = cause_T2MI,
             cause_extra_car_yes_34 = cause_extra_car_yes,
             Cause_cardiac_yes_34 = Cause_cardiac_yes) %>%
      distinct(`Participant Id`, .keep_all = TRUE),
    by = "Participant Id"
  ) %>%
  mutate(
    # Detailed sub-aetiology for OBS12
    subaetiology_12 = case_when(
      cause_extra_car_yes_12 == 1 ~ paste0("Extracardiac: ", extra_12_detail),
      Cause_cardiac_yes_12 == 1 ~ paste0("Cardiac: ", cardiac_12_detail),
      T2MI_12 == 1 ~ "Cardiac: T2MI with cause",
      T2MI_12 == 0 & cause_extra_car_yes_12 == 0 & Cause_cardiac_yes_12 == 0 ~ "Cardiac: T2MI without cause",
      TRUE ~ "Unknown"
    ),
    # Detailed sub-aetiology for OBS34
    subaetiology_34 = case_when(
      cause_extra_car_yes_34 == 1 ~ paste0("Extracardiac: ", extra_34_detail),
      Cause_cardiac_yes_34 == 1 ~ paste0("Cardiac: ", cardiac_34_detail),
      T2MI_34 == 1 ~ "Cardiac: T2MI with cause",
      T2MI_34 == 0 & cause_extra_car_yes_34 == 0 & Cause_cardiac_yes_34 == 0 ~ "Cardiac: T2MI without cause",
      TRUE ~ "Unknown"
    ),
    # Direction of disagreement
    disagreement_direction = case_when(
      PMI_type_12 == "Cardiac" & PMI_type_34 == "Noncardiac" ~ "OBS12 Cardiac -> OBS34 Extracardiac",
      PMI_type_12 == "Noncardiac" & PMI_type_34 == "Cardiac" ~ "OBS12 Extracardiac -> OBS34 Cardiac",
      is.na(PMI_type_12) | is.na(PMI_type_34) ~ "Missing classification",
      TRUE ~ "Other disagreement"
    )
  )

# --- General disagreement direction ---
cat("--- General Disagreement Direction ---\n")
disagreement_direction_summary <- nonagreed_detail %>%
  count(disagreement_direction, sort = TRUE) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))
print(disagreement_direction_summary, n = Inf)

# --- Detailed sub-aetiology disagreement ---
cat("\n--- Detailed Disagreement Pairs (OBS12 vs OBS34 sub-aetiologies) ---\n")
disagreement_pairs <- nonagreed_detail %>%
  count(subaetiology_12, subaetiology_34, sort = TRUE) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))
print(disagreement_pairs, n = Inf)

# --- OBS12 sub-aetiologies in non-agreed cases ---
cat("\n--- OBS12 Classification in Non-Agreed Cases ---\n")
obs12_nonagreed_dist <- nonagreed_detail %>%
  count(subaetiology_12, sort = TRUE) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))
print(obs12_nonagreed_dist, n = Inf)

# --- OBS34 sub-aetiologies in non-agreed cases ---
cat("\n--- OBS34 Classification in Non-Agreed Cases ---\n")
obs34_nonagreed_dist <- nonagreed_detail %>%
  count(subaetiology_34, sort = TRUE) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))
print(obs34_nonagreed_dist, n = Inf)

# --- VISUALIZATION: Alluvial/Sankey-style bar chart of disagreements ---
cat("\n--- Creating Misclassification Visualization ---\n")

# Bar chart showing disagreement pairs
# Top N disagreement patterns
top_disagreements <- disagreement_pairs %>%
  head(15) %>%
  mutate(
    pair_label = paste0(subaetiology_12, "\n-> ", subaetiology_34),
    # Color by direction
    direction = case_when(
      grepl("^Cardiac", subaetiology_12) & grepl("^Extracardiac", subaetiology_34) ~ "Cardiac -> Extracardiac",
      grepl("^Extracardiac", subaetiology_12) & grepl("^Cardiac", subaetiology_34) ~ "Extracardiac -> Cardiac",
      grepl("^Cardiac", subaetiology_12) & grepl("^Cardiac", subaetiology_34) ~ "Cardiac subtype disagreement",
      grepl("^Extracardiac", subaetiology_12) & grepl("^Extracardiac", subaetiology_34) ~ "Extracardiac subtype disagreement",
      TRUE ~ "Other"
    )
  )

misclass_plot <- ggplot(top_disagreements,
                        aes(x = reorder(pair_label, n), y = n, fill = direction)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c(
    "Cardiac -> Extracardiac" = "#E64B35",
    "Extracardiac -> Cardiac" = "#4DBBD5",
    "Cardiac subtype disagreement" = "#FFB6C1",
    "Extracardiac subtype disagreement" = "#ADD8E6",
    "Other" = "#999999"
  )) +
  geom_text(aes(label = paste0(n, " (", Percentage, "%)")),
            hjust = -0.1, size = 3) +
  labs(
    title = "Inter-Observer Disagreement Patterns",
    subtitle = paste0("Non-agreed cases (n=", nrow(nonagreed), "): OBS12 vs OBS34 classifications"),
    x = "Disagreement pattern\n(OBS12 -> OBS34)",
    y = "Number of patients",
    fill = "Direction"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))

print(misclass_plot)
ggsave("Misclassification_OBS12_vs_OBS34.png",
       plot = misclass_plot,
       width = 14, height = 8, dpi = 300)
cat("Misclassification plot saved as 'Misclassification_OBS12_vs_OBS34.png'\n")

# --- Summary: Cardiac vs Extracardiac level disagreement ---
cat("\n--- Cardiac vs Extracardiac Level Disagreement Summary ---\n")
general_disagreement <- nonagreed_detail %>%
  filter(!is.na(PMI_type_12) & !is.na(PMI_type_34)) %>%
  count(PMI_type_12, PMI_type_34, sort = TRUE) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))
print(general_disagreement, n = Inf)

# Stacked bar chart: where does most discussion happen?
cat("\n--- Creating Summary Disagreement Bar Chart ---\n")

# Per sub-aetiology: how often is it involved in a disagreement?
subaet_involvement <- bind_rows(
  nonagreed_detail %>%
    count(subaetiology = subaetiology_12, name = "n_as_obs12"),
  nonagreed_detail %>%
    count(subaetiology = subaetiology_34, name = "n_as_obs34")
) %>%
  group_by(subaetiology) %>%
  summarise(
    total_involvement = sum(n_as_obs12, na.rm = TRUE) + sum(n_as_obs34, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_involvement)) %>%
  mutate(
    category = case_when(
      grepl("^Cardiac", subaetiology) ~ "Cardiac",
      grepl("^Extracardiac", subaetiology) ~ "Extracardiac",
      TRUE ~ "Other"
    )
  )

involvement_plot <- ggplot(subaet_involvement %>% head(12),
                           aes(x = reorder(subaetiology, total_involvement),
                               y = total_involvement,
                               fill = category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("Cardiac" = "#E64B35", "Extracardiac" = "#4DBBD5", "Other" = "#999999")) +
  geom_text(aes(label = total_involvement), hjust = -0.2, size = 3.5) +
  labs(
    title = "Sub-Aetiologies Most Involved in Inter-Observer Disagreement",
    subtitle = "Frequency of involvement in non-agreed cases (as either OBS12 or OBS34 classification)",
    x = "Sub-aetiology",
    y = "Total involvement count",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "bottom"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(involvement_plot)
ggsave("SubAetiology_Disagreement_Involvement.png",
       plot = involvement_plot,
       width = 12, height = 7, dpi = 300)
cat("Sub-aetiology disagreement plot saved as 'SubAetiology_Disagreement_Involvement.png'\n")

cat("\n=== MISCLASSIFICATION ANALYSIS COMPLETE ===\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("\n✓ Comparison table: OBS12 vs Agreed PMI categories\n")
cat("✓ PMI category overviews (noncardiac, cardiac, T2MI)\n")
cat("✓ Surgical specialty analysis with p-values for cardiac vs noncardiac\n")
cat("✓ Baseline characteristics tables (using obs12_with_pmi)\n")
cat("✓ Surgical specialty p-values integrated into OBS12 and Agreed tables\n")
cat("✓ IN-HOSPITAL MORTALITY defined by opname_bestemming only\n")
cat("✓ Postoperative vitals analysis with hypotension/tachycardia detection\n")
cat("✓ In-hospital mortality by vital sign threshold violations\n")
cat("✓ Mortality tables for total cohort and cardiac PMI subgroup\n")
cat("✓ **NEW: First hsTnT value coupled with admission location (specialty & ward)**\n")
cat("✓ **NEW: Enhanced bar charts with cardiac vs noncardiac colors**\n")
cat("✓ **NEW: Mortality by PMI aetiology bar charts**\n")
cat("✓ **NEW: Combined vitals thresholds and mortality visualization**\n")
cat("✓ **NEW: Stratified mortality analysis by PMI type and thresholds**\n")
cat("✓ **NEW: Dual-panel figure showing PMI aetiology distribution and mortality**\n")
cat("✓ **NEW: In-hospital mortality adjusted for emergency surgery (4 logistic models)**\n")
cat("✓ **NEW: P-value test for PMI type distribution (Emergency vs Elective)**\n")
cat("✓ **NEW: Subgroup analysis in elective surgeries only**\n")
cat("✓ **NEW: Early filters removing NA Pseudonym and NA PMI_category**\n")
cat("✓ VAS pain score analysis - ONLY scores BEFORE first troponin collection\n")
cat("✓ 30-day and 365-day mortality analyses removed (only in-hospital mortality used)\n")
cat("✓ **NEW: 30-day KM curve for in-hospital mortality (cardiac vs extracardiac) with censoring**\n")
cat("✓ **NEW: Competing risks analysis (CIF, Gray's test, Fine-Gray regression)**\n")
cat("✓ **NEW: Publication-ready two-panel CIF figure (BJA style)**\n")
cat("✓ **NEW: Peak hsTnT per patient with days to peak from verrichtingen surgery date**\n")
cat("✓ **NEW: Comprehensive logistic regression on in-hospital mortality (5 models + forest plot)**\n")
cat("✓ **NEW: OBS12 vs OBS34 misclassification analysis with detailed sub-aetiology breakdown**\n")
cat("✓ **NEW: Misclassification visualization showing disagreement patterns**\n")
