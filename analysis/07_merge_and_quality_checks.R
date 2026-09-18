# Dataset assembly and quality checks
# HGEN 603 final project
# Requires authorized access to the All of Us Researcher Workbench Controlled Tier.
# No participant-level data are included in this repository.

# ---- BEFORE merging check ----
# ---- Consistency of person_id ----
# Consistency of person_id
n_distinct(demographic_final$person_id)
n_distinct(final_condition_dataset$person_id)
n_distinct(med_final_df$person_id)
n_distinct(measurement_final_df$person_id)
n_distinct(family_history_df$person_id)
n_distinct(visit_final_df$person_id)
# ---- Missingness check ----
library(naniar)
miss_var_summary(demographic_final)
miss_var_summary(final_condition_dataset)
miss_var_summary(med_final_df)
miss_var_summary(measurement_final_df)
miss_var_summary(family_history_df)
# ---- Check duplicates ----
any(duplicated(demographic_final$person_id))
any(duplicated(final_condition_dataset$person_id))
any(duplicated(med_final_df$person_id))
# check the condition dataset to see why I have duplicates
head(final_condition_dataset, 20)
# Make a new condition datset that for each person we have only one row
library(dplyr)
condition_summary <- final_condition_dataset %>%
  group_by(person_id) %>%
  summarise(
    has_mdd = as.integer(any(mdd_related == 1, na.rm = TRUE)),
    has_anxiety = as.integer(any(anxiety_related == 1, na.rm = TRUE)),
    has_bipolar = as.integer(any(bipolar_related == 1, na.rm = TRUE)),
    has_oud = as.integer(any(oud_related == 1, na.rm = TRUE)),
    has_htn = as.integer(any(hypertension_related == 1, na.rm = TRUE)),
    first_mdd_date = if (any(mdd_related == 1, na.rm = TRUE)) {
      min(condition_start_datetime[mdd_related == 1], na.rm = TRUE)
    } else {
      as.Date(NA)
    },
    first_anxiety_date = if (any(anxiety_related == 1, na.rm = TRUE)) {
      min(condition_start_datetime[anxiety_related == 1], na.rm = TRUE)
    } else {
      as.Date(NA)
    },
    first_bipolar_date = if (any(bipolar_related == 1, na.rm = TRUE)) {
      min(condition_start_datetime[bipolar_related == 1], na.rm = TRUE)
    } else {
      as.Date(NA)
    },
    first_oud_date = if (any(oud_related == 1, na.rm = TRUE)) {
      min(condition_start_datetime[oud_related == 1], na.rm = TRUE)
    } else {
      as.Date(NA)
    }
  ) %>%
  ungroup()
final_df <- med_final_df %>%
  left_join(demographic_final, by = "person_id") %>%
  left_join(condition_summary, by = "person_id") %>%
  left_join(measurement_final_df, by = "person_id") %>%
  left_join(family_history_df, by = "person_id") %>%
  left_join(visit_final_df, by = "person_id")
## Check if merging worked
nrow(final_df)
n_distinct(final_df$person_id)
colnames(final_df)
summary(final_df$TRD)
table(final_df$TRD, useNA = "ifany")
colMeans(is.na(final_df)) * 100
# ---- Check variable types ----
str(final_df)
# ---- Fix family history NA ----
final_df <- final_df %>%
  mutate(
    fh_depression = ifelse(is.na(fh_depression), 0, fh_depression),
    fh_anxiety = ifelse(is.na(fh_anxiety), 0, fh_anxiety),
    fh_bipolar = ifelse(is.na(fh_bipolar), 0, fh_bipolar),
    fh_ptsd = ifelse(is.na(fh_ptsd), 0, fh_ptsd),
    fh_substance = ifelse(is.na(fh_substance), 0, fh_substance)
  )
# ---- Create age ----
# Create index_date
final_df <- final_df %>%
  mutate(
    index_date = dplyr::coalesce(TRD_date, first_mdd_date) # take 'TRD_date' if exists otherwise first_mdd_date
  )
# Check this index
summary(final_df$index_date)
# Check missing values
final_df %>%
  summarise(
    total = n(),
    n_with_index = sum(!is.na(index_date)),
    n_missing = sum(is.na(index_date))
  )
# Check by TRD group
final_df %>%
  group_by(TRD) %>%
  summarise(
    n = n(),
    missing_index = sum(is.na(index_date)),
    percent_missing = mean(is.na(index_date)) * 100
  )
# Check date range
final_df %>%
  summarise(
    min_date = min(index_date, na.rm = TRUE),
    max_date = max(index_date, na.rm = TRUE)
  )
# Check distributiion
library(ggplot2)
ggplot(final_df, aes(x = index_date)) +
  geom_histogram(bins = 50)
# Age calculation
final_df <- final_df %>%
  mutate(
    age = as.numeric(index_date - date_of_birth) / 365.25
  )
# validating Age
summary(final_df$age)
final_df %>%
  summarise(
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    n_negative = sum(age < 0, na.rm = TRUE)
  )
# Remove unrealistic ages
final_df <- final_df %>%
  mutate(
    age = ifelse(age < 18 | age > 100, NA, age)
  )
# Recheck age
summary(final_df$age)
# Check age again
final_df %>%
  group_by(TRD) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE)
  )
# Calculate time to TRD
final_df <- final_df %>%
  mutate(
    time_to_TRD = ifelse(
      TRD == 1,
      as.numeric(TRD_date - first_mdd_date) / 365.25,
      NA
    )
  )
# Check this value
summary(final_df$time_to_TRD)
# Summary of this value
final_df %>%
  summarise(
    min_time = min(time_to_TRD, na.rm = TRUE),
    max_time = max(time_to_TRD, na.rm = TRUE),
    n_negative = sum(time_to_TRD < 0, na.rm = TRUE)
  )
# Since we have negative value for time to TRD, we have to confirm it first and then drop the invalid values
final_df %>%
  filter(time_to_TRD < 0) %>%
  select(person_id, first_mdd_date, TRD_date) %>%
  head(20)
# remove invalid values
final_df <- final_df %>%
  mutate(
    time_to_TRD = ifelse(time_to_TRD < 0, NA, time_to_TRD)
  )
# Recheck after cleaning
final_df %>%
  summarise(
    min_time = min(time_to_TRD, na.rm = TRUE),
    max_time = max(time_to_TRD, na.rm = TRUE),
    n_negative = sum(time_to_TRD < 0, na.rm = TRUE)
  )
# ---- Small fixes before start analysis ----
# TRD is 'num' so we want to change it to factor
final_df <- final_df %>%
  mutate(
    TRD = factor(TRD, levels = c(0, 1), labels = c("Non-TRD", "TRD"))
  )
# Create one visit variable
final_df <- final_df %>%
  mutate(
    n_psy_visits = ifelse(TRD == "TRD",
      n_psy_visits_1y_TRD,
      n_psy_visits_1y_nonTRD
    )
  )
summary(final_df$n_psy_visits)
# Compare by group
final_df %>%
  summarise(
    n_missing = sum(is.na(n_psy_visits)),
    pct_missing = mean(is.na(n_psy_visits)) * 100,
    n_zero = sum(n_psy_visits == 0, na.rm = TRUE)
  )
final_df %>%
  group_by(TRD) %>%
  summarise(
    n_missing = sum(is.na(n_psy_visits)),
    pct_missing = mean(is.na(n_psy_visits)) * 100,
    median_visits = median(n_psy_visits, na.rm = TRUE),
    mean_visits = mean(n_psy_visits, na.rm = TRUE)
  )
# Convert NA -> 0
final_df <- final_df %>%
  mutate(
    n_psy_visits = ifelse(is.na(n_psy_visits), 0, n_psy_visits)
  )
# Recheck again
summary(final_df$n_psy_visits)
final_df %>%
  group_by(TRD) %>%
  summarise(
    median_visits = median(n_psy_visits),
    mean_visits = mean(n_psy_visits)
  )
# Because of skewness, create log1p for better calculatiin
# Convert binary variables to factors
final_df <- final_df %>%
  mutate(
    TRD = factor(TRD, levels = c("Non-TRD", "TRD")),
    has_anxiety = factor(has_anxiety),
    has_bipolar = factor(has_bipolar),
    has_oud = factor(has_oud),
    has_htn = factor(has_htn),
    fh_depression = factor(fh_depression),
    fh_anxiety = factor(fh_anxiety),
    fh_bipolar = factor(fh_bipolar),
    fh_ptsd = factor(fh_ptsd),
    fh_substance = factor(fh_substance),
    augmentation_flag = factor(augmentation_flag),
    ketamine_flag = factor(ketamine_flag)
  )
# ---- Create clean subset dataset for regression analysis ----
# Make a clean regression subset dataset used only for regression models
# Create regression dataset (NO need to mutate again)
regression_df <- final_df %>%
  select(
    # Outcome
    TRD,
    # Demographics (required covariates)
    age,
    sex_at_birth,
    race,
    ethnicity,
    # Clinical comorbidities
    has_anxiety,
    has_bipolar,
    has_oud,
    has_htn,
    # Family history
    fh_depression,
    fh_anxiety,
    # Healthcare utilization (important predictor)
    n_psy_visits
  )
# Structure and variable types
str(regression_df)
# Outcome variable check
table(regression_df$TRD)
# Missing data
colMeans(is.na(regression_df)) * 100
# How many people will actually be used in the model?
regression_df %>%
  summarise(n_total = n())
regression_df %>%
  drop_na() %>%
  summarise(n_complete = n())
# Check distributions
summary(regression_df$age)
summary(regression_df$n_psy_visits)
hist(regression_df$age)
hist(regression_df$n_psy_visits)
# I want to make sure predictors vary across TRD groups
regression_df %>%
  group_by(TRD) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_visits = mean(n_psy_visits, na.rm = TRUE)
  )
# Check categorical variables
table(regression_df$TRD, regression_df$has_htn)
table(regression_df$TRD, regression_df$has_anxiety)
# Check for extreme imbalance
prop.table(table(regression_df$TRD))
# Since visit data is so skewed, It's better to transform visits before modeling:
regression_df <- regression_df %>%
  mutate(
    log_visits = log1p(n_psy_visits)
  )
