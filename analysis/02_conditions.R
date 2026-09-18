# Condition data extraction and cleaning
# HGEN 603 final project
# Requires authorized access to the All of Us Researcher Workbench Controlled Tier.
# No participant-level data are included in this repository.

# ---- Condition dataset ----
# ---- LOAD DATA ----
library(tidyverse)
library(bigrquery)
# This query represents dataset "MDD-TRD_P1" for domain "condition" and was generated for All of Us Controlled Tier Dataset v8
dataset_66886376_condition_sql <- paste("
SELECT
c_occurrence.person_id,
c_occurrence.condition_concept_id,
c_standard_concept.concept_name as standard_concept_name,
c_standard_concept.concept_code as standard_concept_code,
c_standard_concept.vocabulary_id as standard_vocabulary,
c_occurrence.condition_start_datetime,
c_occurrence.condition_end_datetime,
c_occurrence.condition_type_concept_id,
c_type.concept_name as condition_type_concept_name,
c_occurrence.stop_reason,
c_occurrence.visit_occurrence_id,
visit.concept_name as visit_occurrence_concept_name,
c_occurrence.condition_source_value,
c_occurrence.condition_source_concept_id,
c_source_concept.concept_name as source_concept_name,
c_source_concept.concept_code as source_concept_code,
c_source_concept.vocabulary_id as source_vocabulary,
c_occurrence.condition_status_source_value,
c_occurrence.condition_status_concept_id,
c_status.concept_name as condition_status_concept_name
FROM
( SELECT
*
FROM
`condition_occurrence` c_occurrence
WHERE
(
condition_concept_id IN (SELECT
DISTINCT c.concept_id
FROM
`cb_criteria` c
JOIN
(SELECT
CAST(cr.id as string) AS id
FROM
`cb_criteria` cr
WHERE
concept_id IN (201820, 201826, 316866, 320128, 36684319, 36717092, 4149320, 4152280, 4250023, 4282096, 4282316, 42872411, 4304010, 4307111, 4307956, 4310821, 432285, 432301, 4327337, 432876, 432883, 4338031, 434613, 434911, 43531624, 436074, 436075, 436665, 436676, 438120, 438130, 438406, 438998, 440690, 442077, 607540, 607543)
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
c_occurrence.PERSON_ID IN (SELECT
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
) c_occurrence
LEFT JOIN
`concept` c_standard_concept
ON c_occurrence.condition_concept_id = c_standard_concept.concept_id
LEFT JOIN
`concept` c_type
ON c_occurrence.condition_type_concept_id = c_type.concept_id
LEFT JOIN
`visit_occurrence` v
ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id
LEFT JOIN
`concept` visit
ON v.visit_concept_id = visit.concept_id
LEFT JOIN
`concept` c_source_concept
ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id
LEFT JOIN
`concept` c_status
ON c_occurrence.condition_status_concept_id = c_status.concept_id", sep = "")
# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_66886376_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"), # Comment out this line if you want the export to always overwrite.
  "condition_66886376",
  "condition_66886376_*.csv"
)
message(str_glue(
  "The data will be written to {condition_66886376_path}. Use this path when reading ",
  "the data into your notebooks in the future."
))
# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_66886376_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_66886376_path,
  destination_format = "CSV"
)
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_66886376_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), condition_type_concept_name = col_character(), stop_reason = col_character(), visit_occurrence_concept_name = col_character(), condition_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), condition_status_source_value = col_character(), condition_status_concept_name = col_character())
  bind_rows(
    map(
      system2("gsutil", args = c("ls", export_path), stdout = TRUE, stderr = TRUE),
      function(csv) {
        message(str_glue("Loading {csv}."))
        chunk <- read_csv(pipe(str_glue("gsutil cat {csv}")), col_types = col_types, show_col_types = FALSE)
        if (is.null(col_types)) {
          col_types <- spec(chunk)
        }
        chunk
      }
    )
  )
}
dataset_66886376_condition_df <- read_bq_export_from_workspace_bucket(condition_66886376_path)
dim(dataset_66886376_condition_df)
head(dataset_66886376_condition_df, 5)
# ---- DATA EXPLORATION ----
# Check size of condition table
dim(dataset_66886376_condition_df)
head(dataset_66886376_condition_df, 5)
# Check number of unique people (How many people do I have?)
dataset_66886376_condition_df %>%
  summarise(n_people = n_distinct(person_id))
# Counts how many times each diagnosis appears (What diagnoses do they have?)
dataset_66886376_condition_df %>%
  count(standard_concept_name, sort = TRUE)
# Check missing values in every column
dataset_66886376_condition_df %>%
  summarise(across(everything(), ~ sum(is.na(.))))
# How many diagnosis per person?
dataset_66886376_condition_df %>%
  count(person_id) %>%
  summarise(
    min = min(n),
    median = median(n),
    mean = mean(n),
    max = max(n)
  )
# How many unique diagnosis we have?
dataset_66886376_condition_df %>%
  summarise(total_conditions = n_distinct(standard_concept_name))
dataset_66886376_condition_df %>%
  distinct(standard_concept_name)
# Check th date range
dataset_66886376_condition_df %>%
  summarise(
    min_date = min(condition_start_datetime, na.rm = TRUE),
    max_date = max(condition_start_datetime, na.rm = TRUE)
  )
# Check vocabulary types(Which coding systems appear)
dataset_66886376_condition_df %>%
  count(standard_vocabulary)
# Condition type sources(Where diagnoses come from)
dataset_66886376_condition_df %>%
  count(condition_type_concept_name, sort = TRUE)
head(dataset_66886376_condition_df)
# ---- DATA CLEANING ----
# ---- Remove duplicates ----
# Keep the raw table unchanged and Create a clean copy of dataset
condition_df <- dataset_66886376_condition_df
# Checks whether my dataset has duplicate rows or not
condition_df <- condition_df %>%
  distinct(
    person_id,
    condition_concept_id,
    standard_concept_name,
    standard_concept_code,
    condition_start_datetime,
    condition_end_datetime,
    condition_type_concept_name,
    visit_occurrence_id,
    .keep_all = TRUE # Keep all columns, but remove duplicate rows based only on the columns we specified
  )
# Verify it
condition_df %>%
  summarise(
    n_rows = n(),
    n_distinct_rows = n_distinct(
      person_id,
      condition_concept_id,
      standard_concept_name,
      condition_start_datetime,
      condition_end_datetime,
      condition_type_concept_name,
      visit_occurrence_id
    )
  )
# Convert dates to proper date format
condition_df <- condition_df %>%
  mutate(
    condition_start_datetime = as.POSIXct(condition_start_datetime), # converts text dates into real date-time objects
    condition_end_datetime = as.POSIXct(condition_end_datetime)
  )
# clean non-informative text fields
condition_df <- condition_df %>%
  mutate(
    condition_type_concept_name = case_when(
      condition_type_concept_name == "No matching concept" ~ NA_character_,
      TRUE ~ condition_type_concept_name
    ),
    visit_occurrence_concept_name = case_when(
      visit_occurrence_concept_name == "No matching concept" ~ NA_character_,
      TRUE ~ visit_occurrence_concept_name
    )
  )
condition_df %>%
  count(condition_type_concept_name, sort = TRUE)
# ---- Create broad condition groups ----
condition_df <- condition_df %>%
  mutate(
    condition_group = case_when(
      # bipolar
      str_detect(
        standard_concept_name,
        regex("bipolar", ignore_case = TRUE)
      ) ~ "bipolar_related",
      # Depression related
      str_detect(
        standard_concept_name,
        regex("depress", ignore_case = TRUE)
      ) ~ "mdd_related",
      # hypertension related
      str_detect(
        standard_concept_name,
        regex("hypertension|hypertensive|pre-eclampsia|preeclampsia|hellp", ignore_case = TRUE)
      ) ~ "hypertension_related",
      # diabetes related
      str_detect(
        standard_concept_name,
        regex("diabetes|diabetic", ignore_case = TRUE)
      ) ~ "diabetes_related",
      # anxiety related
      str_detect(
        standard_concept_name,
        regex("anxiety|panic|phobia|phobic|obsessive|adjustment disorder|stress|situational disturbance", ignore_case = TRUE)
      ) ~ "anxiety_related",
      # PTSD related
      str_detect(
        standard_concept_name,
        regex("posttraumatic stress|post-traumatic stress|acute post trauma", ignore_case = TRUE)
      ) ~ "ptsd_stress_related",
      # opioid related
      str_detect(
        standard_concept_name,
        regex("opioid|opium|heroin|methadone", ignore_case = TRUE)
      ) ~ "oud_related",
      TRUE ~ "other"
    )
  )
# Verify the result (check if grouping logic makes sense)
condition_df %>%
  count(condition_group, standard_concept_name, sort = TRUE)
condition_df %>%
  filter(condition_group == "other") %>%
  count(standard_concept_name, sort = TRUE)
# ---- Convert condition date to proper date ----
# Keep only variables we need for now
condition_clean <- condition_df %>%
  select(
    person_id,
    standard_concept_name,
    condition_start_datetime,
    condition_group,
    visit_occurrence_concept_name,
    condition_type_concept_name
  )
# Convert condition date to proper date
condition_clean <- condition_clean %>%
  mutate(
    condition_start_datetime = as.Date(condition_start_datetime)
  )
head(condition_clean)

# Create one person-level indicator for each condition group, then attach the
# indicators to the cleaned condition records.
condition_flags <- condition_clean %>%
  distinct(person_id, condition_group) %>%
  mutate(present = 1L) %>%
  pivot_wider(
    names_from = condition_group,
    values_from = present,
    values_fill = 0L
  )

# Merge the condition flags into the clean condition table.
final_condition_dataset <- condition_clean %>%
  left_join(condition_flags, by = "person_id")
head(final_condition_dataset, 20)
# ---- Save condition dataset ----
# Save final_condition_dataset
saveRDS(final_condition_dataset, "final_condition_dataset.rds")
# Save clean dataset before the above dataset
saveRDS(condition_clean, "condition_clean.rds")
