# Demographic data extraction and cleaning
# HGEN 603 final project
# Requires authorized access to the All of Us Researcher Workbench Controlled Tier.
# No participant-level data are included in this repository.

# ---- 1. Person dataset (Demographic) ----
# ---- LOAD DATA ----
library(tidyverse)
library(bigrquery)
# This query represents dataset "MDD-TRD_P1" for domain "person" and was generated for All of Us Controlled Tier Dataset v8
dataset_66886376_person_sql <- paste("
SELECT
person.person_id,
person.birth_datetime as date_of_birth,
p_race_concept.concept_name as race,
p_ethnicity_concept.concept_name as ethnicity,
p_sex_at_birth_concept.concept_name as sex_at_birth
FROM
`person` person
LEFT JOIN
`concept` p_race_concept
ON person.race_concept_id = p_race_concept.concept_id
LEFT JOIN
`concept` p_ethnicity_concept
ON person.ethnicity_concept_id = p_ethnicity_concept.concept_id
LEFT JOIN
`concept` p_sex_at_birth_concept
ON person.sex_at_birth_concept_id = p_sex_at_birth_concept.concept_id
WHERE
person.PERSON_ID IN (SELECT
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
AND is_standard = 0 )) criteria ) )", sep = "")
# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
person_66886376_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"), # Comment out this line if you want the export to always overwrite.
  "person_66886376",
  "person_66886376_*.csv"
)
message(str_glue(
  "The data will be written to {person_66886376_path}. Use this path when reading ",
  "the data into your notebooks in the future."
))
# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_66886376_person_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  person_66886376_path,
  destination_format = "CSV"
)
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {person_66886376_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(race = col_character(), ethnicity = col_character(), sex_at_birth = col_character())
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
dataset_66886376_person_df <- read_bq_export_from_workspace_bucket(person_66886376_path)
dim(dataset_66886376_person_df)
head(dataset_66886376_person_df, 5)
# ---- DATA CLEANING ----
demographic_df <- dataset_66886376_person_df %>%
  mutate(
    race_clean = case_when( # It is like if / else statements
      # Clean race variable
      race %in% c(
        "None Indicated",
        "PMI: Skip",
        "None of these",
        "I prefer not to answer"
      ) ~ NA_character_, # convert non-informative responses to NA
      TRUE ~ race
    ), # If none of the above conditions apply → keep the original race value
    # Clean ethnicity variable
    ethnicity_clean = case_when(
      ethnicity %in% c(
        "PMI: Skip",
        "PMI: Prefer Not To Answer",
        "What Race Ethnicity: Race Ethnicity None Of These",
        "No matching concept"
      ) ~ NA_character_, # convert non-informative responses to NA
      TRUE ~ ethnicity # keep valid ethnicity values
    ),
    # Clean sex at birth variable
    sex_at_birth_clean = case_when(
      sex_at_birth %in% c(
        "PMI: Skip",
        "No matching concept",
        "I prefer not to answer",
        "Sex At Birth: Sex At Birth None Of These"
      ) ~ NA_character_,
      TRUE ~ sex_at_birth
    )
  )
# Check to see if cleaning worked correctly
demographic_df %>% count(race_clean, sort = TRUE)
demographic_df %>% count(ethnicity_clean, sort = TRUE)
demographic_df %>% count(sex_at_birth_clean, sort = TRUE)
# Check missingness
demographic_df %>%
  summarise(
    missing_race = sum(is.na(race_clean)),
    missing_ethnicity = sum(is.na(ethnicity_clean)),
    missing_sex = sum(is.na(sex_at_birth_clean))
  )
# Convert date_of_birth to date
demographic_df <- demographic_df %>%
  mutate(date_of_birth = as.Date(date_of_birth)) %>%
  mutate(birth_year = lubridate::year(date_of_birth))
head(demographic_df)
# Convert variables to factors
demographic_df <- demographic_df %>%
  mutate(
    race_clean = factor(race_clean),
    ethnicity_clean = factor(ethnicity_clean),
    sex_at_birth_clean = factor(sex_at_birth_clean)
  )
# Clean demographic table
demographic_final <- demographic_df %>%
  select(
    person_id,
    date_of_birth,
    race = race_clean,
    ethnicity = ethnicity_clean,
    sex_at_birth = sex_at_birth_clean,
    birth_year = birth_year
  )
head(demographic_final)
