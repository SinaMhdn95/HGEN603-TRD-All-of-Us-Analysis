# Survey-based family history variables
# HGEN 603 final project
# Requires authorized access to the All of Us Researcher Workbench Controlled Tier.
# No participant-level data are included in this repository.

# ---- Survey dataset ----
# ---- LOAD DATA ----
library(tidyverse)
library(bigrquery)
# This query represents dataset "MDD-TRD_P1" for domain "survey" and was generated for All of Us Controlled Tier Dataset v8
dataset_66886376_survey_sql <- paste("
SELECT
answer.person_id,
answer.survey_datetime,
answer.survey,
answer.question_concept_id,
answer.question,
answer.answer_concept_id,
answer.answer,
answer.survey_version_name
FROM
`ds_survey` answer
WHERE
(
question_concept_id IN (43529217, 836804)
)
AND (
answer.PERSON_ID IN (SELECT
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
survey_66886376_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"), # Comment out this line if you want the export to always overwrite.
  "survey_66886376",
  "survey_66886376_*.csv"
)
message(str_glue(
  "The data will be written to {survey_66886376_path}. Use this path when reading ",
  "the data into your notebooks in the future."
))
# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_66886376_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_66886376_path,
  destination_format = "CSV"
)
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_66886376_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
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
dataset_66886376_survey_df <- read_bq_export_from_workspace_bucket(survey_66886376_path)
dim(dataset_66886376_survey_df)
head(dataset_66886376_survey_df, 5)
# ---- DATA CLEANING ----
## Extract Family history
# First, create a working copy of our survey data
survey_fh <- dataset_66886376_survey_df
# Remove noninformative answers
survey_fh_clean <- survey_fh %>%
  filter(!answer %in% c(
    "PMI: Skip",
    "PMI: Dont Know",
    "PMI: Prefer Not To Answer"
  ))
# Separate the two family-history questions
fh_dep_relatives <- survey_fh_clean %>%
  filter(str_detect(question, "who in your family has had depression")) # Depression family-members
fh_mental_conditions <- survey_fh_clean %>%
  filter(str_detect(question, "mental health or substance use conditions")) # Mental health and sub-use family-history
# Inspect each subset
fh_dep_relatives %>% count(answer, sort = TRUE)
fh_mental_conditions %>% count(answer, sort = TRUE)
head(fh_dep_relatives)
head(fh_mental_conditions)
# Create a base table of all participants in the cleaned survey family-history dataset
survey_people <- survey_fh_clean %>%
  distinct(person_id)
# From Q1:
# Create a simple family-history depression flag (0:without, 1= with, 2= Unknown/NA)
fh_depression <- fh_dep_relatives %>%
  filter(answer != "PMI: None") %>% # We only want people with family history
  filter(!str_detect(answer, " - Self$")) %>% # exclude Self so this is strictly FH
  distinct(person_id) %>% # remove duplicates
  mutate(fh_depression = 1) # create new variable and 1 = has family history of depression
# From Q2:
# Depression again
fh_depression2 <- fh_mental_conditions %>%
  filter(str_detect(answer, "Depression")) %>% # keep only rows with "Depression"
  distinct(person_id) %>% # remove duplicates
  mutate(fh_depression2 = 1) # create new variable and 1 = has family history of depression
# Anxiety
fh_anxiety <- fh_mental_conditions %>%
  filter(str_detect(answer, "Anxiety reaction/panic disorder")) %>%
  distinct(person_id) %>%
  mutate(fh_anxiety = 1)
# Bipolar
fh_bipolar <- fh_mental_conditions %>%
  filter(str_detect(answer, "Bipolar disorder")) %>%
  distinct(person_id) %>%
  mutate(fh_bipolar = 1)
# PTSD
fh_ptsd <- fh_mental_conditions %>%
  filter(str_detect(answer, "Post-traumatic stress disorder")) %>%
  distinct(person_id) %>%
  mutate(fh_ptsd = 1)
# Alcohol use disorder (AUD)
fh_alcohol <- fh_mental_conditions %>%
  filter(str_detect(answer, "Alcohol use disorder")) %>%
  distinct(person_id) %>%
  mutate(fh_alcohol = 1)
# Drug use disorder (DUD)
fh_drug <- fh_mental_conditions %>%
  filter(str_detect(answer, "Drug use disorder")) %>%
  distinct(person_id) %>%
  mutate(fh_drug = 1)
# Create one substance-use family-history flag
fh_substance <- full_join(fh_alcohol, fh_drug, by = "person_id") %>%
  mutate(
    fh_alcohol = if_else(is.na(fh_alcohol), 0, fh_alcohol),
    fh_drug = if_else(is.na(fh_drug), 0, fh_drug),
    fh_substance = if_else(fh_alcohol == 1 | fh_drug == 1, 1, 0)
  ) %>%
  select(person_id, fh_substance)
# Merge all family-history variables together
family_history_df <- survey_people %>%
  left_join(fh_depression, by = "person_id") %>%
  left_join(fh_depression2, by = "person_id") %>%
  left_join(fh_anxiety, by = "person_id") %>%
  left_join(fh_bipolar, by = "person_id") %>%
  left_join(fh_ptsd, by = "person_id") %>%
  left_join(fh_substance, by = "person_id")
# Replace missing values with 0
family_history_df <- family_history_df %>%
  mutate(
    fh_depression = if_else(is.na(fh_depression), 0, fh_depression),
    fh_depression2 = if_else(is.na(fh_depression2), 0, fh_depression2),
    fh_anxiety = if_else(is.na(fh_anxiety), 0, fh_anxiety),
    fh_bipolar = if_else(is.na(fh_bipolar), 0, fh_bipolar),
    fh_ptsd = if_else(is.na(fh_ptsd), 0, fh_ptsd),
    fh_substance = if_else(is.na(fh_substance), 0, fh_substance)
  )
# Combine depression variables into ONE
family_history_df <- family_history_df %>%
  mutate(
    fh_depression = if_else(
      fh_depression == 1 | fh_depression2 == 1, 1, 0
    )
  ) %>%
  select(-fh_depression2) # remove extra column
head(family_history_df)
