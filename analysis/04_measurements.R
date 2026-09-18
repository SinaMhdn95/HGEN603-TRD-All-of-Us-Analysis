# Clinical measurement data processing
# HGEN 603 final project
# Requires authorized access to the All of Us Researcher Workbench Controlled Tier.
# No participant-level data are included in this repository.

# ---- Measurements dataset ----
# ---- LOAD DATA ----
library(tidyverse)
library(bigrquery)
# This query represents dataset "MDD-TRD_P1" for domain "measurement" and was generated for All of Us Controlled Tier Dataset v8
dataset_66886376_measurement_sql <- paste("
SELECT
measurement.person_id,
measurement.measurement_concept_id,
m_standard_concept.concept_name as standard_concept_name,
m_standard_concept.concept_code as standard_concept_code,
m_standard_concept.vocabulary_id as standard_vocabulary,
measurement.measurement_datetime,
m_type.concept_name as measurement_type_concept_name,
measurement.value_as_number,
m_value.concept_name as value_as_concept_name,
m_unit.concept_name as unit_concept_name,
measurement.range_low,
measurement.range_high,
measurement.visit_occurrence_id,
m_visit.concept_name as visit_occurrence_concept_name
FROM
( SELECT
*
FROM
`measurement` measurement
WHERE
(
measurement_concept_id IN (SELECT
DISTINCT c.concept_id
FROM
`cb_criteria` c
JOIN
(SELECT
CAST(cr.id as string) AS id
FROM
`cb_criteria` cr
WHERE
concept_id IN (3004249, 3009201, 3010156, 3012888, 3019170, 3019762, 3020149, 3020460, 3025315, 3036277, 3038553, 4007831, 40765040, 42529187, 44804610)
AND full_text LIKE '%_rank1]%'      ) a
ON (c.path LIKE CONCAT('%.', a.id, '.%')
OR c.path LIKE CONCAT('%.', a.id)
OR c.path LIKE CONCAT(a.id, '.%')
OR c.path = a.id)
WHERE
is_standard = 1
AND is_selectable = 1)
)
AND (
measurement.PERSON_ID IN (SELECT
distinct person_id
FROM
`cb_search_person` cb_search_person
WHERE
cb_search_person.person_id IN (SELECT
criteria.person_id
FROM
(SELECT
DISTINCT person_id, entry_date, concept_id
FROM
`cb_search_all_events`
WHERE
(concept_id IN(SELECT
DISTINCT c.concept_id
FROM
`cb_criteria` c
JOIN
(SELECT
CAST(cr.id as string) AS id
FROM
`cb_criteria` cr
WHERE
concept_id IN (1568218, 1568217)
AND full_text LIKE '%_rank1]%'      ) a
ON (c.path LIKE CONCAT('%.', a.id, '.%')
OR c.path LIKE CONCAT('%.', a.id)
OR c.path LIKE CONCAT(a.id, '.%')
OR c.path = a.id)
WHERE
is_standard = 0
AND is_selectable = 1)
AND is_standard = 0 )) criteria
UNION
DISTINCT SELECT
criteria.person_id
FROM
(SELECT
DISTINCT person_id, entry_date, concept_id
FROM
`cb_search_all_events`
WHERE
(concept_id IN(SELECT
DISTINCT c.concept_id
FROM
`cb_criteria` c
JOIN
(SELECT
CAST(cr.id as string) AS id
FROM
`cb_criteria` cr
WHERE
concept_id IN (44831090, 44821821, 44822975, 44831089, 44825293)
AND full_text LIKE '%_rank1]%'      ) a
ON (c.path LIKE CONCAT('%.', a.id, '.%')
OR c.path LIKE CONCAT('%.', a.id)
OR c.path LIKE CONCAT(a.id, '.%')
OR c.path = a.id)
WHERE
is_standard = 0
AND is_selectable = 1)
AND is_standard = 0 )) criteria ) )
)
) measurement
LEFT JOIN
`concept` m_standard_concept
ON measurement.measurement_concept_id = m_standard_concept.concept_id
LEFT JOIN
`concept` m_type
ON measurement.measurement_type_concept_id = m_type.concept_id
LEFT JOIN
`concept` m_value
ON measurement.value_as_concept_id = m_value.concept_id
LEFT JOIN
`concept` m_unit
ON measurement.unit_concept_id = m_unit.concept_id
LEFT JOIn
`visit_occurrence` v
ON measurement.visit_occurrence_id = v.visit_occurrence_id
LEFT JOIN
`concept` m_visit
ON v.visit_concept_id = m_visit.concept_id", sep = "")
# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
measurement_66886376_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"), # Comment out this line if you want the export to always overwrite.
  "measurement_66886376",
  "measurement_66886376_*.csv"
)
message(str_glue(
  "The data will be written to {measurement_66886376_path}. Use this path when reading ",
  "the data into your notebooks in the future."
))
# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_66886376_measurement_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  measurement_66886376_path,
  destination_format = "CSV"
)
dataset_66886376_measurement_df %>%
  filter(measurement_concept_id == 44804610) %>%
  summarise(
    concept_name = first(standard_concept_name),
    n = n(),
    n_people = n_distinct(person_id),
    min_value = min(value_as_number, na.rm = TRUE),
    max_value = max(value_as_number, na.rm = TRUE),
    median_value = median(value_as_number, na.rm = TRUE)
  )
dataset_66886376_measurement_df %>%
  filter(measurement_concept_id == 44804610) %>%
  count(value_as_number) %>%
  arrange(value_as_number)
# Check missing data
dataset_66886376_measurement_df %>%
  summarise_all(~ sum(is.na(.)))
# Check number of people
dataset_66886376_measurement_df %>%
  summarise(n_people = n_distinct(person_id))
# Check measurement types
dataset_66886376_measurement_df %>%
  count(standard_concept_name, sort = TRUE)
# Check value distribution
summary(dataset_66886376_measurement_df$value_as_number)
# Check units
dataset_66886376_measurement_df %>%
  count(unit_concept_name)
# Check measurement vs units together
dataset_66886376_measurement_df %>%
  count(standard_concept_name, unit_concept_name) %>%
  arrange(desc(n))
# ---- DATA CLEANING ----
# ---- Make a working copy and clean the datetime ----
# Make a working copy and parse the datetime (turns measurement_datetime from character into a true date-time)
measurement_df <- dataset_66886376_measurement_df %>%
  mutate(
    measurement_datetime = as.POSIXct(measurement_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )
# ---- Create a clean measurement group ----
# point1: [search text]....str_detect(x, pattern):Does the text in x contain this pattern?
# point2: [pattern matching]....regex("pattern", ignore_case = TRUE): This tells R that treat this as a
#        regular expression pattern and ignore uppercase/lowercase differences
# point3: ^ = beginning of the string and $ = end of the string, so this patterns mean match the entire text exactly as...
#       if we do not use ^ and $, search would be less strict and it will look for pattern anywhere
measurement_df <- measurement_df %>%
  mutate(
    measurement_group = case_when(
      str_detect(standard_concept_name, regex("^Systolic blood pressure$", ignore_case = TRUE)) ~ "sbp",
      str_detect(standard_concept_name, regex("^Diastolic blood pressure$", ignore_case = TRUE)) ~ "dbp",
      str_detect(standard_concept_name, regex("^Body weight$", ignore_case = TRUE)) ~ "weight",
      str_detect(standard_concept_name, regex("^Body height$", ignore_case = TRUE)) ~ "height",
      str_detect(standard_concept_name, regex("Body mass index", ignore_case = TRUE)) ~ "bmi",
      str_detect(standard_concept_name, regex("Thyrotropin", ignore_case = TRUE)) ~ "tsh",
      str_detect(standard_concept_name, regex("C reactive protein", ignore_case = TRUE)) ~ "crp",
      str_detect(standard_concept_name, regex("25-hydroxyvitamin D|25-Hydroxyvitamin D|Vitamin D, 25-hydroxy|25-hydroxyergocalciferol", ignore_case = TRUE)) ~ "vitamin_d",
      str_detect(standard_concept_name, regex("PHQ-9", ignore_case = TRUE)) ~ "phq9",
      TRUE ~ "other"
    )
  )
# Inspect what got grouped
# point: count(data, column1, column2):Counts how many times each combination appears.
measurement_df %>%
  count(measurement_group, standard_concept_name, sort = TRUE)
# ---- Standardize units into one final value column ----
# Let's create one cleaned numeric column
measurement_df <- measurement_df %>%
  mutate(
    value_clean = case_when(
      # Blood pressure to mmHg
      measurement_group %in% c("sbp", "dbp") &
        unit_concept_name %in% c("millimeter mercury column", "No matching concept", "No information", "unit", "times", NA_character_) ~ value_as_number,
      # Height to cm
      measurement_group == "height" & unit_concept_name == "centimeter" ~ value_as_number,
      measurement_group == "height" & unit_concept_name == "inch (international)" ~ value_as_number * 2.54,
      measurement_group == "height" & unit_concept_name == "foot (international)" ~ value_as_number * 30.48,
      measurement_group == "height" & unit_concept_name == "meter" ~ value_as_number * 100,
      # Weight to kg
      measurement_group == "weight" & unit_concept_name == "kilogram" ~ value_as_number,
      measurement_group == "weight" & unit_concept_name == "gram" ~ value_as_number / 1000,
      measurement_group == "weight" & unit_concept_name == "pound (US)" ~ value_as_number * 0.453592,
      measurement_group == "weight" & unit_concept_name == "ounce (avoirdupois)" ~ value_as_number * 0.0283495,
      # BMI to kg/m^2
      measurement_group == "bmi" & unit_concept_name %in% c("kilogram per square meter", "kg/sq. m", "ratio", "No matching concept", NA_character_) ~ value_as_number,
      # TSH to uIU/mL equivalent
      measurement_group == "tsh" & unit_concept_name %in% c(
        "micro-international unit per milliliter",
        "microunit per milliliter",
        "uIU/mL",
        "milli-international unit per liter",
        "mIU/L",
        "milliunit per liter"
      ) ~ value_as_number,
      # CRP to mg/L
      measurement_group == "crp" & unit_concept_name %in% c("milligram per liter", "mg/L") ~ value_as_number,
      measurement_group == "crp" & unit_concept_name %in% c("milligram per deciliter", "mg/dL") ~ value_as_number * 10,
      measurement_group == "crp" & unit_concept_name == "microgram per milliliter" ~ value_as_number,
      # Vitamin D to ng/mL
      measurement_group == "vitamin_d" & unit_concept_name == "nanogram per milliliter" ~ value_as_number,
      measurement_group == "vitamin_d" & unit_concept_name == "microgram per liter" ~ value_as_number,
      measurement_group == "vitamin_d" & unit_concept_name == "nanogram per deciliter" ~ value_as_number / 10,
      measurement_group == "vitamin_d" & unit_concept_name == "picogram per milliliter" ~ value_as_number / 1000,
      # PHQ-9
      measurement_group == "phq9" ~ value_as_number,
      TRUE ~ NA_real_ # if none of the earlier conditions match, make value_clean missing
    )
  )
# test whether the above code is worked
measurement_df %>%
  filter(!is.na(value_clean)) %>%
  count(measurement_group, unit_concept_name, sort = TRUE)
measurement_df %>%
  group_by(measurement_group) %>%
  summarise(
    min_value = min(value_clean, na.rm = TRUE),
    median_value = median(value_clean, na.rm = TRUE),
    max_value = max(value_clean, na.rm = TRUE)
  )
# Remove unrealistic values (from aove code chunck)
measurement_df <- measurement_df %>%
  mutate(
    value_clean = case_when(
      measurement_group == "sbp" &
        (value_clean < 50 | value_clean > 300) ~ NA_real_,
      measurement_group == "dbp" &
        (value_clean < 30 | value_clean > 200) ~ NA_real_,
      measurement_group == "height" &
        (value_clean < 100 | value_clean > 250) ~ NA_real_,
      measurement_group == "weight" &
        (value_clean < 30 | value_clean > 300) ~ NA_real_,
      measurement_group == "bmi" &
        (value_clean < 10 | value_clean > 100) ~ NA_real_,
      measurement_group == "tsh" &
        (value_clean < 0.001 | value_clean > 100) ~ NA_real_,
      measurement_group == "vitamin_d" &
        (value_clean < 5 | value_clean > 200) ~ NA_real_,
      measurement_group == "crp" &
        (value_clean < 0 | value_clean > 500) ~ NA_real_,
      TRUE ~ value_clean
    )
  )
# Test if filting is worked
measurement_df %>%
  group_by(measurement_group) %>%
  summarise(
    min_value = min(value_clean, na.rm = TRUE),
    median_value = median(value_clean, na.rm = TRUE),
    max_value = max(value_clean, na.rm = TRUE)
  )
measurement_df %>%
  filter(!is.na(value_clean)) %>%
  count(measurement_group, unit_concept_name, sort = TRUE)
# ---- Find measurements around TRD diagnosis and make a TRD window ----
# First I have to make sure we convert to Date format
measurement_df <- measurement_df %>%
  mutate(
    measurement_date = as.Date(measurement_datetime)
  )
# Define TRD windows for TRD patients
# before_TRD = before the 12-month window starts
# TRD_window = inside the 12-month window
measurement_trd <- measurement_df %>%
  left_join(
    med_final_df %>%
      select(person_id, TRD, TRD_date),
    by = "person_id"
  ) %>%
  filter(
    TRD == 1,
    !is.na(TRD_date)
  ) %>%
  mutate(
    window_start = TRD_date - 180,
    window_end = TRD_date + 180,
    period = case_when(
      measurement_date < window_start ~ "before_TRD",
      measurement_date >= window_start & measurement_date <= window_end ~ "TRD_window",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period))
head(measurement_trd)
# ---- Create index date for non-TRD ----
# Define an index date for non-TRD patients
# Here I use first medication date as the anchor
non_trd_index <- med_final_df %>%
  filter(TRD == 0) %>%
  select(person_id, TRD) %>%
  left_join(
    med_clean %>%
      group_by(person_id) %>%
      summarise(
        index_date = min(drug_start, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "person_id"
  ) %>%
  filter(!is.na(index_date))
# Keep only measurements inside the non-TRD window
measurement_non_trd <- measurement_df %>%
  left_join(non_trd_index, by = "person_id") %>%
  mutate(
    window_start = index_date - 180,
    window_end   = index_date + 180
  ) %>%
  filter(
    measurement_date >= window_start,
    measurement_date <= window_end
  ) %>%
  mutate(
    period = "nonTRD_window"
  )
# Combine TRD and non-TRD measurement data
measurement_all <- bind_rows(
  measurement_trd,
  measurement_non_trd
) %>%
  filter(measurement_group != "other")
# ---- Validate our measurement codes so far ----
measurement_all %>%
  count(period)
measurement_all %>%
  count(period, measurement_group, sort = TRUE)
measurement_all %>%
  group_by(period, measurement_group) %>%
  summarise(
    n_people = n_distinct(person_id),
    n_rows = n(),
    min_value = min(value_clean, na.rm = TRUE),
    mean_value = mean(value_clean, na.rm = TRUE),
    median_value = median(value_clean, na.rm = TRUE),
    max_value = max(value_clean, na.rm = TRUE),
    .groups = "drop"
  )
# ---- Summarise repeated measurements ----
# Summarise repeated measurements within each window
# If a person has weight 3 times, take the mean
measurement_summary <- measurement_all %>%
  group_by(person_id, measurement_group, period) %>%
  summarise(
    mean_value = mean(value_clean, na.rm = TRUE),
    n_measurements = n(),
    .groups = "drop"
  )
# Check that each person has only one value per measurement per period
measurement_summary %>%
  count(person_id, measurement_group, period) %>%
  filter(n > 1)
# ---- Build person-level wide table ----
measurement_final_df <- measurement_summary %>%
  select(person_id, measurement_group, period, mean_value) %>%
  pivot_wider(
    names_from = c(measurement_group, period),
    values_from = mean_value
  )
head(measurement_final_df)
# Final checks
measurement_final_df %>%
  summarise(
    n_rows = n(),
    n_people = n_distinct(person_id)
  )
measurement_final_df %>%
  count(person_id) %>%
  filter(n > 1)
colnames(measurement_final_df)
head(measurement_final_df)
