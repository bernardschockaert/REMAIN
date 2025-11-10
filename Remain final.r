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
  pivot_wider(names_from = PMI_type, values_from = N, values_fill = 0) %>%
  mutate(Total = Cardiac + Noncardiac) %>%
  arrange(desc(Total))

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
    pivot_wider(names_from = PMI_type, values_from = N, values_fill = 0) %>%
    mutate(Total = Cardiac + Noncardiac,
           ECG = factor(ECG, levels = c(0, 1), labels = c("No ECG (0)", "ECG Done (1)")))

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

# ========== PMI CATEGORY K-M CURVES (30-DAY) ==========

cat("\n\n=== KAPLAN-MEIER CURVES: PMI CATEGORIES (30-day) ===\n\n")

# OBS12 - 30-day survival by PMI category
cat("--- 30-Day Survival by PMI Category (OBS12) ---\n")

obs12_pmi_cat_summary <- obs12_with_pmi %>%
  group_by(PMI_category) %>%
  summarise(
    N = n(),
    Deaths_30d = sum(death_30d, na.rm = TRUE),
    Mortality_30d = round(Deaths_30d / N * 100, 1)
  ) %>%
  arrange(desc(N))

print(obs12_pmi_cat_summary)

# Filter categories with sufficient sample size (≥5 patients)
obs12_pmi_for_km <- obs12_with_pmi %>%
  group_by(PMI_category) %>%
  filter(n() >= 5) %>%
  ungroup()

cat("\nCategories included in 30-day KM curve (n≥5):", 
    length(unique(obs12_pmi_for_km$PMI_category)), "\n")
cat("Total patients in KM analysis:", nrow(obs12_pmi_for_km), "\n\n")

if(nrow(obs12_pmi_for_km) > 0) {
  surv_obj_30d_cat_obs12 <- Surv(time = obs12_pmi_for_km$survival_time_30d, 
                                  event = obs12_pmi_for_km$death_30d)
  fit_30d_cat_obs12 <- survfit(surv_obj_30d_cat_obs12 ~ PMI_category, data = obs12_pmi_for_km)
  survdiff_30d_cat_obs12 <- survdiff(surv_obj_30d_cat_obs12 ~ PMI_category, data = obs12_pmi_for_km)
  
  pval_label_30d_cat_obs12 <- paste0("Log-rank p = ", 
                                     format.pval(survdiff_30d_cat_obs12$pvalue, digits = 3))
  
  ggsurvplot(
    fit_30d_cat_obs12,
    data = obs12_pmi_for_km,
    risk.table = TRUE,
    pval = pval_label_30d_cat_obs12,
    conf.int = TRUE,
    xlim = c(0, 30),
    xlab = "Time (days)",
    ylab = "Survival probability",
    title = "30-Day Survival by PMI Category (OBS12)",
    legend.title = "PMI Category",
    break.time.by = 5,
    palette = "jco"
  )
}

# Agreed cases - 30-day survival by PMI category
cat("\n--- 30-Day Survival by PMI Category (Agreed Cases) ---\n")

agreed_pmi_cat_summary <- agreed_survival %>%
  group_by(PMI_category) %>%
  summarise(
    N = n(),
    Deaths_30d = sum(death_30d, na.rm = TRUE),
    Mortality_30d = round(Deaths_30d / N * 100, 1)
  ) %>%
  arrange(desc(N))

print(agreed_pmi_cat_summary)

# Filter categories with sufficient sample size (≥5 patients)
agreed_pmi_for_km <- agreed_survival %>%
  group_by(PMI_category) %>%
  filter(n() >= 5) %>%
  ungroup()

cat("\nCategories included in 30-day KM curve (n≥5):", 
    length(unique(agreed_pmi_for_km$PMI_category)), "\n")
cat("Total patients in KM analysis:", nrow(agreed_pmi_for_km), "\n\n")

if(nrow(agreed_pmi_for_km) > 0) {
  surv_obj_30d_cat_agreed <- Surv(time = agreed_pmi_for_km$survival_time_30d, 
                                   event = agreed_pmi_for_km$death_30d)
  fit_30d_cat_agreed <- survfit(surv_obj_30d_cat_agreed ~ PMI_category, data = agreed_pmi_for_km)
  survdiff_30d_cat_agreed <- survdiff(surv_obj_30d_cat_agreed ~ PMI_category, data = agreed_pmi_for_km)
  
  pval_label_30d_cat_agreed <- paste0("Log-rank p = ", 
                                      format.pval(survdiff_30d_cat_agreed$pvalue, digits = 3))
  
  ggsurvplot(
    fit_30d_cat_agreed,
    data = agreed_pmi_for_km,
    risk.table = TRUE,
    pval = pval_label_30d_cat_agreed,
    conf.int = TRUE,
    xlim = c(0, 30),
    xlab = "Time (days)",
    ylab = "Survival probability",
    title = "30-Day Survival by PMI Category (Agreed Cases)",
    legend.title = "PMI Category",
    break.time.by = 5,
    palette = "jco"
  )
}

# ========== T2MI KAPLAN-MEIER CURVES ==========

cat("\n\n=== KAPLAN-MEIER CURVES: T2MI WITH vs WITHOUT CAUSE ===\n\n")

# Prepare T2MI data for OBS12
obs12_t2mi_data <- obs12_with_pmi %>%
  filter(PMI_category %in% c("T2MI_with_cause", "T2MI_without_cause")) %>%
  mutate(T2MI_group = factor(PMI_category, 
                             levels = c("T2MI_with_cause", "T2MI_without_cause"),
                             labels = c("T2MI with cause", "T2MI without cause")))

# Prepare T2MI data for agreed cases
agreed_t2mi_data <- agreed_survival %>%
  filter(PMI_category %in% c("T2MI_with_cause", "T2MI_without_cause")) %>%
  mutate(T2MI_group = factor(PMI_category, 
                             levels = c("T2MI_with_cause", "T2MI_without_cause"),
                             labels = c("T2MI with cause", "T2MI without cause")))

cat("--- Number of T2MI patients (OBS12) ---\n")
cat("T2MI with cause:", sum(obs12_t2mi_data$T2MI_group == "T2MI with cause"), "\n")
cat("T2MI without cause:", sum(obs12_t2mi_data$T2MI_group == "T2MI without cause"), "\n\n")

cat("--- Number of T2MI patients (Agreed Cases) ---\n")
cat("T2MI with cause:", sum(agreed_t2mi_data$T2MI_group == "T2MI with cause"), "\n")
cat("T2MI without cause:", sum(agreed_t2mi_data$T2MI_group == "T2MI without cause"), "\n\n")

# OBS12 - 30-day survival for T2MI
if(nrow(obs12_t2mi_data) >= 5) {
  cat("\n--- 30-Day Survival: T2MI with vs without cause (OBS12) ---\n")
  surv_obj_30d_t2mi_obs12 <- Surv(time = obs12_t2mi_data$survival_time_30d, 
                                  event = obs12_t2mi_data$death_30d)
  fit_30d_t2mi_obs12 <- survfit(surv_obj_30d_t2mi_obs12 ~ T2MI_group, data = obs12_t2mi_data)
  survdiff_30d_t2mi_obs12 <- survdiff(surv_obj_30d_t2mi_obs12 ~ T2MI_group, data = obs12_t2mi_data)
  
  pval_label_30d_t2mi_obs12 <- paste0("Log-rank p = ", format.pval(survdiff_30d_t2mi_obs12$pvalue, digits = 3))
  
  ggsurvplot(
    fit_30d_t2mi_obs12,
    data = obs12_t2mi_data,
    risk.table = TRUE,
    pval = pval_label_30d_t2mi_obs12,
    conf.int = TRUE,
    xlim = c(0, 30),
    xlab = "Time (days)",
    ylab = "Survival probability",
    title = "30-Day Survival: T2MI with vs without Cause (OBS12)",
    legend.title = "T2MI Type",
    legend.labs = c("T2MI with cause", "T2MI without cause"),
    break.time.by = 5,
    palette = c("#FC4E07", "#00AFBB")
  )
} else {
  cat("Insufficient T2MI cases in OBS12 for 30-day survival analysis\n")
}

# OBS12 - 365-day survival for T2MI
if(nrow(obs12_t2mi_data) >= 5) {
  cat("\n--- 365-Day Survival: T2MI with vs without cause (OBS12) ---\n")
  surv_obj_365d_t2mi_obs12 <- Surv(time = obs12_t2mi_data$survival_time_365d, 
                                   event = obs12_t2mi_data$death_365d)
  fit_365d_t2mi_obs12 <- survfit(surv_obj_365d_t2mi_obs12 ~ T2MI_group, data = obs12_t2mi_data)
  survdiff_365d_t2mi_obs12 <- survdiff(surv_obj_365d_t2mi_obs12 ~ T2MI_group, data = obs12_t2mi_data)
  
  pval_label_365d_t2mi_obs12 <- paste0("Log-rank p = ", format.pval(survdiff_365d_t2mi_obs12$pvalue, digits = 3))
  
  ggsurvplot(
    fit_365d_t2mi_obs12,
    data = obs12_t2mi_data,
    risk.table = TRUE,
    pval = pval_label_365d_t2mi_obs12,
    conf.int = TRUE,
    xlim = c(0, 365),
    xlab = "Time (days)",
    ylab = "Survival probability",
    title = "365-Day Survival: T2MI with vs without Cause (OBS12)",
    legend.title = "T2MI Type",
    legend.labs = c("T2MI with cause", "T2MI without cause"),
    break.time.by = 60,
    palette = c("#FC4E07", "#00AFBB")
  )
} else {
  cat("Insufficient T2MI cases in OBS12 for 365-day survival analysis\n")
}

# Agreed cases - 30-day survival for T2MI
if(nrow(agreed_t2mi_data) >= 5) {
  cat("\n--- 30-Day Survival: T2MI with vs without cause (Agreed Cases) ---\n")
  surv_obj_30d_t2mi_agreed <- Surv(time = agreed_t2mi_data$survival_time_30d, 
                                   event = agreed_t2mi_data$death_30d)
  fit_30d_t2mi_agreed <- survfit(surv_obj_30d_t2mi_agreed ~ T2MI_group, data = agreed_t2mi_data)
  survdiff_30d_t2mi_agreed <- survdiff(surv_obj_30d_t2mi_agreed ~ T2MI_group, data = agreed_t2mi_data)
  
  pval_label_30d_t2mi_agreed <- paste0("Log-rank p = ", format.pval(survdiff_30d_t2mi_agreed$pvalue, digits = 3))
  
  ggsurvplot(
    fit_30d_t2mi_agreed,
    data = agreed_t2mi_data,
    risk.table = TRUE,
    pval = pval_label_30d_t2mi_agreed,
    conf.int = TRUE,
    xlim = c(0, 30),
    xlab = "Time (days)",
    ylab = "Survival probability",
    title = "30-Day Survival: T2MI with vs without Cause (Agreed Cases)",
    legend.title = "T2MI Type",
    legend.labs = c("T2MI with cause", "T2MI without cause"),
    break.time.by = 5,
    palette = c("#FC4E07", "#00AFBB")
  )
} else {
  cat("Insufficient T2MI cases in Agreed Cases for 30-day survival analysis\n")
}

# Agreed cases - 365-day survival for T2MI
if(nrow(agreed_t2mi_data) >= 5) {
  cat("\n--- 365-Day Survival: T2MI with vs without cause (Agreed Cases) ---\n")
  surv_obj_365d_t2mi_agreed <- Surv(time = agreed_t2mi_data$survival_time_365d, 
                                    event = agreed_t2mi_data$death_365d)
  fit_365d_t2mi_agreed <- survfit(surv_obj_365d_t2mi_agreed ~ T2MI_group, data = agreed_t2mi_data)
  survdiff_365d_t2mi_agreed <- survdiff(surv_obj_365d_t2mi_agreed ~ T2MI_group, data = agreed_t2mi_data)
  
  pval_label_365d_t2mi_agreed <- paste0("Log-rank p = ", format.pval(survdiff_365d_t2mi_agreed$pvalue, digits = 3))
  
  ggsurvplot(
    fit_365d_t2mi_agreed,
    data = agreed_t2mi_data,
    risk.table = TRUE,
    pval = pval_label_365d_t2mi_agreed,
    conf.int = TRUE,
    xlim = c(0, 365),
    xlab = "Time (days)",
    ylab = "Survival probability",
    title = "365-Day Survival: T2MI with vs without Cause (Agreed Cases)",
    legend.title = "T2MI Type",
    legend.labs = c("T2MI with cause", "T2MI without cause"),
    break.time.by = 60,
    palette = c("#FC4E07", "#00AFBB")
  )
} else {
  cat("Insufficient T2MI cases in Agreed Cases for 365-day survival analysis\n")
}

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

# ========== KAPLAN-MEIER CURVES: CARDIAC vs NONCARDIAC WITH LOG-RANK TEST ==========

cat("\n\n=== KAPLAN-MEIER CURVES: CARDIAC vs NONCARDIAC ===\n")
cat("(Log-rank Mantel-Cox test for curve comparison)\n\n")

# ========== MORTALITY SUMMARY (for KM curve verification) ==========

cat("\n--- MORTALITY SUMMARY FOR KM CURVE VERIFICATION ---\n\n")

cat("OBS12 mortality by PMI type:\n")
obs12_mortality_summary <- obs12_with_pmi %>%
  group_by(PMI_type) %>%
  summarise(
    N = n(),
    Deaths_30d = sum(death_30d == 1, na.rm = TRUE),
    Mortality_30d = round(Deaths_30d / N * 100, 1),
    Deaths_365d = sum(death_365d == 1, na.rm = TRUE),
    Mortality_365d = round(Deaths_365d / N * 100, 1)
  )
print(obs12_mortality_summary)

cat("\nAgreed cases mortality by PMI type:\n")
agreed_mortality_summary <- agreed_survival %>%
  group_by(PMI_type) %>%
  summarise(
    N = n(),
    Deaths_30d = sum(death_30d == 1, na.rm = TRUE),
    Mortality_30d = round(Deaths_30d / N * 100, 1),
    Deaths_365d = sum(death_365d == 1, na.rm = TRUE),
    Mortality_365d = round(Deaths_365d / N * 100, 1)
  )
print(agreed_mortality_summary)

cat("\nNote: These values should match the KM curve risk tables.\n\n")

# OBS12 - 365-day survival (includes 30-day mark on x-axis)
cat("\n--- 365-Day Survival (with 30-day mark): Cardiac vs Noncardiac (OBS12) ---\n")
surv_obj_365d_obs12 <- Surv(time = obs12_with_pmi$survival_time_365d, 
                             event = obs12_with_pmi$death_365d)
fit_365d_obs12 <- survfit(surv_obj_365d_obs12 ~ PMI_type, data = obs12_with_pmi)
survdiff_365d_obs12 <- survdiff(surv_obj_365d_obs12 ~ PMI_type, data = obs12_with_pmi)

pval_label_365d_obs12 <- paste0(
  "Log-rank χ² = ", round(survdiff_365d_obs12$chisq, 2), ", p = ", format.pval(survdiff_365d_obs12$pvalue, digits = 3)
)

ggsurvplot(
  fit_365d_obs12,
  data = obs12_with_pmi,
  risk.table = TRUE,
  pval = pval_label_365d_obs12,
  conf.int = TRUE,
  xlim = c(0, 365),
  xlab = "Time (days)",
  ylab = "Survival probability",
  title = "365-Day Survival: Cardiac vs Noncardiac PMI (OBS12)",
  legend.title = "PMI Type",
  legend.labs = c("Cardiac", "Noncardiac"),
  break.time.by = 30,
  palette = c("#E7B800", "#2E9FDF")
)

# Agreed cases - 365-day survival (includes 30-day mark on x-axis)
cat("\n--- 365-Day Survival (with 30-day mark): Cardiac vs Noncardiac (Agreed Cases) ---\n")
surv_obj_365d_agreed <- Surv(time = agreed_survival$survival_time_365d, 
                              event = agreed_survival$death_365d)
fit_365d_agreed <- survfit(surv_obj_365d_agreed ~ PMI_type, data = agreed_survival)
survdiff_365d_agreed <- survdiff(surv_obj_365d_agreed ~ PMI_type, data = agreed_survival)

pval_label_365d_agreed <- paste0(
  "Log-rank χ² = ", round(survdiff_365d_agreed$chisq, 2), ", p = ", format.pval(survdiff_365d_agreed$pvalue, digits = 3)
)

ggsurvplot(
  fit_365d_agreed,
  data = agreed_survival,
  risk.table = TRUE,
  pval = pval_label_365d_agreed,
  conf.int = TRUE,
  xlim = c(0, 365),
  xlab = "Time (days)",
  ylab = "Survival probability",
  title = "365-Day Survival: Cardiac vs Noncardiac PMI (Agreed Cases)",
  legend.title = "PMI Type",
  legend.labs = c("Cardiac", "Noncardiac"),
  break.time.by = 30,
  palette = c("#E7B800", "#2E9FDF")
)

cat("\n\n=== ANALYSIS COMPLETE ===\n")
cat("\n✓ Inter-rater agreement (Cohen's Kappa)\n")
cat("✓ PMI category breakdown (OBS12 vs Agreed)\n")
cat("✓ Surgical specialty analysis (Chi-square/Fisher p-values)\n")
cat("✓ Baseline characteristics tables (NO mortality - baseline only)\n")
cat("✓ Mortality summary for KM curve verification\n")
cat("✓ Kaplan-Meier curves (365-day) with Log-rank test:\n")
cat("  - Cardiac vs Noncardiac (OBS12 & Agreed)\n")
cat("  - PMI categories (30-day)\n")
cat("  - T2MI with vs without cause (30-day & 365-day)\n")
cat("\nKey points:\n")
cat("  • Baseline tables: Chi-square test for group comparisons\n")
cat("  • KM curves: Log-rank (Mantel-Cox) test for survival comparison\n")
cat("  • NO Cox regression or hazard ratios included\n")
cat("  • Uniform Date from coupling file for all survival calculations\n")
