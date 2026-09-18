# Visit data and healthcare utilization
# HGEN 603 final project
# Requires authorized access to the All of Us Researcher Workbench Controlled Tier.
# No participant-level data are included in this repository.

# ---- Visit dataset ----
# ---- LOAD DATA ----
library(tidyverse)
library(bigrquery)
# This query represents dataset "MDD-TRD_P1" for domain "visit" and was generated for All of Us Controlled Tier Dataset v8
dataset_66886376_visit_sql <- paste("
SELECT
visit.PERSON_ID,
visit.visit_concept_id,
v_standard_concept.concept_name as standard_concept_name,
v_standard_concept.concept_code as standard_concept_code,
v_standard_concept.vocabulary_id as standard_vocabulary,
visit.visit_start_datetime,
visit.visit_end_datetime,
v_type.concept_name as visit_type_concept_name,
v_admitting_source_concept.concept_name as admitting_source_concept_name,
v_discharge.concept_name as discharge_to_concept_name
FROM
`visit_occurrence` visit
LEFT JOIN
`concept` v_standard_concept
ON visit.visit_concept_id = v_standard_concept.concept_id
LEFT JOIN
`concept` v_type
ON visit.visit_type_concept_id = v_type.concept_id
LEFT JOIN
`concept` v_admitting_source_concept
ON visit.admitting_source_concept_id = v_admitting_source_concept.concept_id
LEFT JOIN
`concept` v_discharge
ON visit.discharge_to_concept_id = v_discharge.concept_id
WHERE
(
visit_concept_id IN (38004284, 5083, 581477, 8782, 8971, 9201, 9202, 9203)
)
AND (
visit.PERSON_ID IN (SELECT
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
)", sep = "")
# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
visit_66886376_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"), # Comment out this line if you want the export to always overwrite.
  "visit_66886376",
  "visit_66886376_*.csv"
)
message(str_glue(
  "The data will be written to {visit_66886376_path}. Use this path when reading ",
  "the data into your notebooks in the future."
))
# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_66886376_visit_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  visit_66886376_path,
  destination_format = "CSV"
)
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {visit_66886376_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), visit_type_concept_name = col_character(), source_concept_name = col_character(), admitting_source_concept_name = col_character(), discharge_to_concept_name = col_character())
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
dataset_66886376_visit_df <- read_bq_export_from_workspace_bucket(visit_66886376_path)
dim(dataset_66886376_visit_df)
head(dataset_66886376_visit_df, 5)
# ---- DATA CLEANING ----
# ---- Clean visit dataset ----
# Clean date columns and visit groups
visit_df <- dataset_66886376_visit_df %>%
  mutate(
    visit_start_datetime = as.POSIXct(visit_start_datetime),
    visit_end_datetime = as.POSIXct(visit_end_datetime),
    visit_start_date = as.Date(visit_start_datetime),
    visit_end_date = as.Date(visit_end_datetime)
  ) %>%
  filter(!is.na(visit_start_date)) %>%
  filter(is.na(visit_end_date) | visit_end_date >= visit_start_date) %>%
  mutate(
    visit_group = case_when(
      standard_concept_name %in% c(
        "Inpatient Visit",
        "Inpatient Psychiatric Facility",
        "Psychiatric Hospital"
      ) ~ "Inpatient",
      standard_concept_name == "Emergency Room Visit" ~ "ER",
      standard_concept_name %in% c(
        "Outpatient Visit",
        "Office Visit",
        "Telehealth",
        "Urgent Care Facility"
      ) ~ "Outpatient",
      TRUE ~ "Other"
    )
  ) %>%
  rename(person_id = PERSON_ID)
# Check if it works
visit_df %>%
  count(standard_concept_name)
head(visit_df)
# ---- Create MDD diagnosis date for non-TRD patients ----
# load final condition dataset
final_condition_dataset <- readRDS("final_condition_dataset.rds")
# Create MDD diagnosis date for non-TRD patients
final_condition_dataset <- final_condition_dataset %>%
  mutate(
    condition_start_datetime = as.Date(condition_start_datetime)
  )
mdd_date_df <- final_condition_dataset %>%
  filter(condition_group == "mdd_related") %>%
  group_by(person_id) %>%
  summarise(
    mdd_date = min(condition_start_datetime, na.rm = TRUE),
    .groups = "drop"
  )
# ---- Build anchor dates ----
# TRD patients -> TRD_date
trd_anchor <- med_final_df %>%
  filter(TRD == 1, !is.na(TRD_date)) %>%
  transmute(
    person_id,
    anchor_date = TRD_date,
    group_type = "TRD"
  )
# non-TRD patients -> mdd_date
nontrd_anchor <- med_final_df %>%
  filter(TRD == 0) %>%
  select(person_id, TRD) %>%
  left_join(mdd_date_df, by = "person_id") %>%
  filter(!is.na(mdd_date)) %>%
  transmute(
    person_id,
    anchor_date = mdd_date,
    group_type = "nonTRD"
  )
anchor_df <- bind_rows(trd_anchor, nontrd_anchor)
# ---- Keep psychiatric prescriptions only ----
psy_rx <- med_clean %>%
  filter(
    med_group %in% c(
      "SSRI",
      "SNRI",
      "TCA",
      "MAOI",
      "Atypical_AD",
      "Antipsychotic",
      "Augmentation_other"
    )
  ) %>%
  select(
    person_id,
    rx_date = drug_start,
    med_group
  ) %>%
  distinct()
# ---- Restrict visits to 1 year after anchor date ----
visit_1y <- visit_df %>%
  inner_join(anchor_df, by = "person_id") %>%
  filter(
    visit_start_date >= anchor_date,
    visit_start_date <= anchor_date + 365
  )
# ---- Define psychiatric-related visits ----
library(data.table)
# A visit counts if a psychiatric prescription happens within 14 days AFTER that visit
visit_dt <- as.data.table(visit_1y)
rx_dt <- as.data.table(psy_rx)
visit_dt[, visit_id := .I]
visit_interval <- visit_dt[, .(
  person_id,
  visit_id,
  group_type,
  visit_group,
  visit_start_date,
  start = visit_start_date,
  end = visit_start_date + 14
)]
rx_interval <- rx_dt[, .(
  person_id,
  rx_date,
  start = rx_date,
  end = rx_date
)]
setkey(visit_interval, person_id, start, end)
setkey(rx_interval, person_id, start, end)
psy_visit_match <- foverlaps(
  x = visit_interval,
  y = rx_interval,
  by.x = c("person_id", "start", "end"),
  by.y = c("person_id", "start", "end"),
  type = "any",
  nomatch = 0L
)
psy_visit_df <- unique(psy_visit_match[, .(
  person_id,
  visit_id,
  group_type,
  visit_group,
  visit_start_date
)])
# Collapse multiple visits on the same day to one visit
# Keep the highest-priority visit type:
# Inpatient > ER > Outpatient
psy_visit_df <- psy_visit_df %>%
  mutate(
    visit_priority = case_when(
      visit_group == "Inpatient" ~ 1,
      visit_group == "ER" ~ 2,
      visit_group == "Outpatient" ~ 3,
      TRUE ~ 4
    )
  ) %>%
  arrange(person_id, visit_start_date, visit_priority) %>%
  group_by(person_id, visit_start_date) %>%
  slice(1) %>%
  ungroup() %>%
  select(-visit_priority)
psy_visit_df %>%
  count(person_id, visit_start_date) %>%
  filter(n > 1)
psy_visit_df %>%
  count(visit_group)
psy_visit_df %>%
  count(person_id, visit_group) %>%
  arrange(desc(n)) %>%
  head(20)
# ---- Summarise to person level ----
visit_final_df <- as_tibble(psy_visit_df) %>%
  group_by(person_id, group_type) %>%
  summarise(
    n_psy_visits_1y = n(),
    n_psy_er_1y = sum(visit_group == "ER", na.rm = TRUE),
    n_psy_inpatient_1y = sum(visit_group == "Inpatient", na.rm = TRUE),
    n_psy_outpatient_1y = sum(visit_group == "Outpatient", na.rm = TRUE),
    any_psy_visit_1y = if_else(n_psy_visits_1y > 0, 1, 0),
    any_psy_er_1y = if_else(n_psy_er_1y > 0, 1, 0),
    any_psy_inpatient_1y = if_else(n_psy_inpatient_1y > 0, 1, 0),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = group_type,
    values_from = c(
      n_psy_visits_1y,
      n_psy_er_1y,
      n_psy_inpatient_1y,
      n_psy_outpatient_1y,
      any_psy_visit_1y,
      any_psy_er_1y,
      any_psy_inpatient_1y
    )
  )
# Validation checks
visit_final_df %>%
  summarise(
    n_rows = n(),
    n_people = n_distinct(person_id)
  )
visit_final_df %>%
  count(person_id) %>%
  filter(n > 1)
colnames(visit_final_df)
head(visit_final_df)
visit_final_df %>%
  summarise(across(everything(), ~ sum(is.na(.))))
visit_final_df %>%
  mutate(
    check_TRD = n_psy_visits_1y_TRD - (
      n_psy_er_1y_TRD +
        n_psy_inpatient_1y_TRD +
        n_psy_outpatient_1y_TRD
    ),
    check_nonTRD = n_psy_visits_1y_nonTRD - (
      n_psy_er_1y_nonTRD +
        n_psy_inpatient_1y_nonTRD +
        n_psy_outpatient_1y_nonTRD
    )
  ) %>%
  summarise(
    TRD_problem = sum(check_TRD != 0, na.rm = TRUE),
    nonTRD_problem = sum(check_nonTRD != 0, na.rm = TRUE)
  )
# check distribution of visits
psy_visit_df %>%
  count(visit_group)
# Check total visit by group
visit_final_df %>%
  summarise(
    total_TRD_visits = sum(n_psy_visits_1y_TRD, na.rm = TRUE),
    total_nonTRD_visits = sum(n_psy_visits_1y_nonTRD, na.rm = TRUE)
  )
# Summary of Visits per Person
visit_final_df %>%
  summarise(
    mean_TRD = mean(n_psy_visits_1y_TRD, na.rm = TRUE),
    median_TRD = median(n_psy_visits_1y_TRD, na.rm = TRUE),
    max_TRD = max(n_psy_visits_1y_TRD, na.rm = TRUE),
    mean_nonTRD = mean(n_psy_visits_1y_nonTRD, na.rm = TRUE),
    median_nonTRD = median(n_psy_visits_1y_nonTRD, na.rm = TRUE),
    max_nonTRD = max(n_psy_visits_1y_nonTRD, na.rm = TRUE)
  )
# Distribution of Visits per Person
visit_final_df %>%
  count(n_psy_visits_1y_TRD) %>%
  arrange(n_psy_visits_1y_TRD)
visit_final_df %>%
  count(n_psy_visits_1y_nonTRD) %>%
  arrange(n_psy_visits_1y_nonTRD)
# Check by Visit Type per Person (Very Helpful)
psy_visit_df %>%
  count(person_id, visit_group) %>%
  arrange(desc(n))
