# Medication data and TRD phenotype construction
# HGEN 603 final project
# Requires authorized access to the All of Us Researcher Workbench Controlled Tier.
# No participant-level data are included in this repository.

# ---- Medication dataset ----
# ---- LOAD DATA ----
library(tidyverse)
library(bigrquery)
# This query represents dataset "MDD-TRD_P1" for domain "drug" and was generated for All of Us Controlled Tier Dataset v8
dataset_66886376_drug_sql <- paste("
SELECT
d_exposure.person_id,
d_exposure.drug_concept_id,
d_standard_concept.concept_name as standard_concept_name,
d_standard_concept.concept_code as standard_concept_code,
d_standard_concept.vocabulary_id as standard_vocabulary,
d_exposure.drug_exposure_start_datetime,
d_exposure.drug_exposure_end_datetime,
d_exposure.verbatim_end_date,
d_type.concept_name as drug_type_concept_name,
d_exposure.stop_reason,
d_exposure.refills,
d_exposure.quantity,
d_exposure.days_supply,
d_exposure.sig,
d_route.concept_name as route_concept_name,
d_exposure.visit_occurrence_id,
d_visit.concept_name as visit_occurrence_concept_name,
d_exposure.dose_unit_source_value
FROM
( SELECT
*
FROM
`drug_exposure` d_exposure
WHERE
(
drug_concept_id IN (SELECT
DISTINCT ca.descendant_id
FROM
`cb_criteria_ancestor` ca
JOIN
(SELECT
DISTINCT c.concept_id
FROM
`cb_criteria` c
JOIN
(SELECT
CAST(cr.id as string) AS id
FROM
`cb_criteria` cr
WHERE
concept_id IN (1366610, 19017241, 19080226, 21604491, 21604499, 21604510, 21604515, 21604525, 21604530, 21604536, 21604540, 21604547, 21604555, 21604557, 21604687, 21604709, 21604719, 35603277, 40234834, 42628962, 44507700, 46275300, 703244, 703547, 714684, 715259, 717607, 725131, 735979, 743670, 750982, 757688, 785649)
AND full_text LIKE '%_rank1]%'       ) a
ON (c.path LIKE CONCAT('%.', a.id, '.%')
OR c.path LIKE CONCAT('%.', a.id)
OR c.path LIKE CONCAT(a.id, '.%')
OR c.path = a.id)
WHERE
is_standard = 1
AND is_selectable = 1) b
ON (ca.ancestor_id = b.concept_id)))
AND (d_exposure.PERSON_ID IN (SELECT
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
) d_exposure
LEFT JOIN
`concept` d_standard_concept
ON d_exposure.drug_concept_id = d_standard_concept.concept_id
LEFT JOIN
`concept` d_type
ON d_exposure.drug_type_concept_id = d_type.concept_id
LEFT JOIN
`concept` d_route
ON d_exposure.route_concept_id = d_route.concept_id
LEFT JOIN
`visit_occurrence` v
ON d_exposure.visit_occurrence_id = v.visit_occurrence_id
LEFT JOIN
`concept` d_visit
ON v.visit_concept_id = d_visit.concept_id", sep = "")
# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
drug_66886376_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"), # Comment out this line if you want the export to always overwrite.
  "drug_66886376",
  "drug_66886376_*.csv"
)
message(str_glue(
  "The data will be written to {drug_66886376_path}. Use this path when reading ",
  "the data into your notebooks in the future."
))
# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_66886376_drug_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  drug_66886376_path,
  destination_format = "CSV"
)
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {drug_66886376_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), drug_type_concept_name = col_character(), stop_reason = col_character(), sig = col_character(), route_concept_name = col_character(), visit_occurrence_concept_name = col_character(), dose_unit_source_value = col_character())
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
dataset_66886376_drug_df <- read_bq_export_from_workspace_bucket(drug_66886376_path)
dim(dataset_66886376_drug_df)
head(dataset_66886376_drug_df, 5)
# ---- DATA EXPLORATION ----
# How many participants have drug data
dataset_66886376_drug_df %>%
  summarise(n_people = n_distinct(person_id))
# How many unique medications
dataset_66886376_drug_df %>%
  summarise(n_drugs = n_distinct(standard_concept_name))
# Most common medications
dataset_66886376_drug_df %>%
  count(standard_concept_name, sort = TRUE)
# Time range
dataset_66886376_drug_df %>%
  summarise(
    min_date = min(drug_exposure_start_datetime, na.rm = TRUE),
    max_date = max(drug_exposure_start_datetime, na.rm = TRUE)
  )
# How many medications per person
dataset_66886376_drug_df %>%
  count(person_id) %>%
  summarise(
    min = min(n),
    median = median(n),
    mean = mean(n),
    max = max(n)
  )
# ---- DATA CLEANING ----
# ---- Convert Dates to Proper Format and remove duplicates ----
# Make a copy from original dataset
medication_dataset <- dataset_66886376_drug_df
# Convert Dates to Proper Format
med_clean <- medication_dataset %>%
  mutate(
    drug_start = as.Date(drug_exposure_start_datetime),
    drug_end = as.Date(drug_exposure_end_datetime)
  )
# Remove duplicate prescriptions
med_clean <- med_clean %>%
  distinct(
    person_id,
    standard_concept_name,
    drug_exposure_start_datetime,
    .keep_all = TRUE
  )
# create final ingredient(specific drug name not groups)column
med_clean <- med_clean %>%
  mutate(
    drug_ingredient = case_when(
      str_detect(drug_name_clean, "sertraline") ~ "sertraline",
      str_detect(drug_name_clean, "fluoxetine") ~ "fluoxetine",
      str_detect(drug_name_clean, "citalopram") ~ "citalopram",
      str_detect(drug_name_clean, "escitalopram") ~ "escitalopram",
      str_detect(drug_name_clean, "paroxetine") ~ "paroxetine",
      str_detect(drug_name_clean, "fluvoxamine") ~ "fluvoxamine",
      str_detect(drug_name_clean, "venlafaxine") ~ "venlafaxine",
      str_detect(drug_name_clean, "duloxetine") ~ "duloxetine",
      str_detect(drug_name_clean, "desvenlafaxine") ~ "desvenlafaxine",
      str_detect(drug_name_clean, "milnacipran") ~ "milnacipran",
      str_detect(drug_name_clean, "bupropion") ~ "bupropion",
      str_detect(drug_name_clean, "mirtazapine") ~ "mirtazapine",
      str_detect(drug_name_clean, "trazodone") ~ "trazodone",
      str_detect(drug_name_clean, "vortioxetine") ~ "vortioxetine",
      str_detect(drug_name_clean, "vilazodone") ~ "vilazodone",
      str_detect(drug_name_clean, "nefazodone") ~ "nefazodone",
      str_detect(drug_name_clean, "amitriptyline") ~ "amitriptyline",
      str_detect(drug_name_clean, "nortriptyline") ~ "nortriptyline",
      str_detect(drug_name_clean, "imipramine") ~ "imipramine",
      str_detect(drug_name_clean, "desipramine") ~ "desipramine",
      str_detect(drug_name_clean, "clomipramine") ~ "clomipramine",
      str_detect(drug_name_clean, "doxepin") ~ "doxepin",
      str_detect(drug_name_clean, "maprotiline") ~ "maprotiline",
      str_detect(drug_name_clean, "trimipramine") ~ "trimipramine",
      str_detect(drug_name_clean, "protriptyline") ~ "protriptyline",
      str_detect(drug_name_clean, "phenelzine") ~ "phenelzine",
      str_detect(drug_name_clean, "tranylcypromine") ~ "tranylcypromine",
      str_detect(drug_name_clean, "isocarboxazid") ~ "isocarboxazid",
      str_detect(drug_name_clean, "aripiprazole") ~ "aripiprazole",
      str_detect(drug_name_clean, "quetiapine") ~ "quetiapine",
      str_detect(drug_name_clean, "olanzapine") ~ "olanzapine",
      str_detect(drug_name_clean, "risperidone") ~ "risperidone",
      str_detect(drug_name_clean, "lurasidone") ~ "lurasidone",
      str_detect(drug_name_clean, "ziprasidone") ~ "ziprasidone",
      str_detect(drug_name_clean, "brexpiprazole") ~ "brexpiprazole",
      str_detect(drug_name_clean, "cariprazine") ~ "cariprazine",
      str_detect(drug_name_clean, "paliperidone") ~ "paliperidone",
      str_detect(drug_name_clean, "clozapine") ~ "clozapine",
      str_detect(drug_name_clean, "haloperidol") ~ "haloperidol",
      str_detect(drug_name_clean, "chlorpromazine") ~ "chlorpromazine",
      str_detect(drug_name_clean, "perphenazine") ~ "perphenazine",
      str_detect(drug_name_clean, "fluphenazine") ~ "fluphenazine",
      str_detect(drug_name_clean, "thiothixene") ~ "thiothixene",
      str_detect(drug_name_clean, "thioridazine") ~ "thioridazine",
      str_detect(drug_name_clean, "trifluoperazine") ~ "trifluoperazine",
      str_detect(drug_name_clean, "loxapine") ~ "loxapine",
      str_detect(drug_name_clean, "molindone") ~ "molindone",
      str_detect(drug_name_clean, "pimozide") ~ "pimozide",
      str_detect(drug_name_clean, "iloperidone") ~ "iloperidone",
      str_detect(drug_name_clean, "asenapine") ~ "asenapine",
      str_detect(drug_name_clean, "amisulpride") ~ "amisulpride",
      str_detect(drug_name_clean, "lumateperone") ~ "lumateperone",
      str_detect(drug_name_clean, "ketamine") ~ "ketamine",
      str_detect(drug_name_clean, "esketamine") ~ "esketamine",
      str_detect(drug_name_clean, "lithium") ~ "lithium",
      str_detect(drug_name_clean, "prochlorperazine") ~ "prochlorperazine",
      str_detect(drug_name_clean, "droperidol") ~ "droperidol",
      str_detect(drug_name_clean, "amoxapine") ~ "amoxapine",
      str_detect(drug_name_clean, "pimavanserin") ~ "pimavanserin",
      str_detect(drug_name_clean, "mesoridazine") ~ "mesoridazine",
      str_detect(drug_name_clean, "promazine") ~ "promazine",
      str_detect(drug_name_clean, "sulpiride") ~ "sulpiride",
      TRUE ~ NA_character_
    )
  )
# Group medications
med_clean <- med_clean %>%
  mutate(
    med_group = case_when(
      drug_ingredient %in% c("sertraline", "fluoxetine", "citalopram", "escitalopram", "paroxetine", "fluvoxamine") ~ "SSRI",
      drug_ingredient %in% c("venlafaxine", "duloxetine", "desvenlafaxine", "milnacipran") ~ "SNRI",
      drug_ingredient %in% c("bupropion", "mirtazapine", "trazodone", "vortioxetine", "vilazodone", "nefazodone") ~ "Atypical_AD",
      drug_ingredient %in% c("amitriptyline", "nortriptyline", "imipramine", "desipramine", "clomipramine", "doxepin", "maprotiline", "trimipramine", "protriptyline", "amoxapine") ~ "TCA",
      drug_ingredient %in% c("phenelzine", "tranylcypromine", "isocarboxazid") ~ "MAOI",
      drug_ingredient %in% c("aripiprazole", "quetiapine", "olanzapine", "risperidone", "lurasidone", "ziprasidone", "brexpiprazole", "cariprazine", "paliperidone", "clozapine", "haloperidol", "chlorpromazine", "perphenazine", "fluphenazine", "thiothixene", "thioridazine", "trifluoperazine", "loxapine", "molindone", "pimozide", "iloperidone", "asenapine", "amisulpride", "lumateperone", "pimavanserin", "mesoridazine", "promazine", "sulpiride") ~ "Antipsychotic",
      drug_ingredient %in% c("ketamine", "esketamine", "lithium") ~ "Augmentation_other",
      drug_ingredient %in% c("prochlorperazine", "droperidol") ~ "Non_MDD_other",
      TRUE ~ "Other"
    )
  )
med_clean %>%
  filter(is.na(drug_ingredient)) %>%
  count(drug_name_clean, sort = TRUE)
nrow(med_clean)
head(med_clean)
# remove duplicates now one more time
med_clean <- med_clean %>%
  distinct(person_id, drug_ingredient, drug_start, .keep_all = TRUE)
# Check the number of patients using drug after cleaning
nrow(med_clean)
med_clean %>%
  summarise(n_persons = n_distinct(person_id))
# Check missing drug ingredients
med_clean %>%
  filter(is.na(drug_ingredient)) %>%
  count(drug_name_clean, sort = TRUE)
# Check missing dates
med_clean %>%
  filter(is.na(drug_start))
# sort medications chronologically
med_clean <- med_clean %>%
  arrange(person_id, drug_start)
# ---- Ketamine cleaning ----
# Check patients using ketamine
med_clean %>%
  filter(drug_ingredient == "ketamine") %>%
  count(visit_occurrence_concept_name)
# filter patients who gets ketamine repeatedly
med_clean <- med_clean %>%
  group_by(person_id) %>%
  mutate(
    ketamine_count = sum(drug_ingredient == "ketamine")
  ) %>%
  ungroup()
med_clean <- med_clean %>%
  mutate(
    ketamine_type = case_when(
      drug_ingredient == "esketamine" ~ "TRD",
      drug_ingredient == "ketamine" & ketamine_count >= 3 ~ "TRD",
      drug_ingredient == "ketamine" ~ "Procedure_or_unclear",
      TRUE ~ NA_character_
    )
  )
# Create a new column "TRD Status" based on using Ket/esKet (separate them based on ketamine use)
med_clean <- med_clean %>%
  mutate(
    ketamine_type = case_when(
      drug_ingredient == "esketamine" ~ "TRD",
      drug_ingredient == "ketamine" & ketamine_count >= 3 ~ "TRD",
      drug_ingredient == "ketamine" ~ "Procedure_or_unclear",
      TRUE ~ "Not_ketamine"
    ),
    ketamine_TRD = if_else(ketamine_type == "TRD", 1, 0)
  )
# Check if it works
med_clean %>%
  count(ketamine_type)
# Check the rows with people using ketamine/esketamine
med_clean %>%
  filter(drug_ingredient %in% c("ketamine", "esketamine")) %>%
  select(person_id, drug_start, visit_occurrence_concept_name, ketamine_count, ketamine_type) %>%
  head(20)
# Check ketamine count logic (TRD should be higher)
med_clean %>%
  filter(drug_ingredient == "ketamine") %>%
  group_by(ketamine_type) %>%
  summarise(
    min = min(ketamine_count),
    max = max(ketamine_count),
    mean = mean(ketamine_count)
  )
# Create  person-level Ketamine Flag
ketamine_flag <- med_clean %>%
  group_by(person_id) %>%
  summarise(
    ketamine_TRD_flag = max(ketamine_type == "TRD", na.rm = TRUE)
  )
ketamine_flag %>%
  count(ketamine_TRD_flag)
# ---- Dates cleaning ----
# Check impossible dates or if we have negative supply days
med_clean %>%
  filter(drug_end < drug_start)
# we are going to convert negative values to positive values just in case
med_clean <- med_clean %>%
  mutate(
    days_supply = abs(days_supply)
  )
# Clean missing end dates
med_clean <- med_clean %>%
  mutate(
    drug_end = case_when(
      !is.na(drug_exposure_end_datetime) ~ as.Date(drug_exposure_end_datetime),
      !is.na(days_supply) ~ drug_start + days_supply,
      TRUE ~ as.Date(NA)
    )
  )
# Check if It works
med_clean %>%
  filter(drug_end < drug_start) %>%
  count()
# ---- Create a duration variable ----
med_clean <- med_clean %>%
  mutate(
    duration_days = as.numeric(drug_end - drug_start)
  )
# Inspect very long durations
med_clean %>%
  filter(duration_days > 365) %>%
  select(person_id, drug_ingredient, drug_start, drug_end, duration_days, drug_type_concept_name) %>%
  arrange(desc(duration_days)) %>%
  head(20)
# ---- Identify MDD patients based on their prescriptions ----
# Create a person-level count of psychiatric prescriptions
med_counts <- med_clean %>%
  filter(med_group != "Non_relevant") %>%
  group_by(person_id) %>%
  summarise(
    n_rx = n(),
    n_unique_drugs = n_distinct(drug_ingredient),
    .groups = "drop"
  ) %>%
  mutate(
    one_rx_only = if_else(n_rx == 1, 1, 0),
    confirmed_treated_MDD = if_else(n_rx >= 2, 1, 0)
  )
med_counts
# ---- Identify and grouping patients with TRD based on our difinition ----
# Patient is TRD if: ≥ 2 switches OR augmentation OR ketamine/esketamine (ketamine_type == "TRD")
# Prepare medication timeline
med_clean <- med_clean %>%
  arrange(person_id, drug_start)
unique(med_clean$med_group)
######## Identify switching #########
switch_df <- med_clean %>%
  filter(med_group %in% c("SSRI", "SNRI", "TCA", "Atypical_AD", "MAOI")) %>%
  arrange(person_id, drug_start) %>%
  group_by(person_id) %>%
  mutate(
    prev_drug = lag(drug_ingredient),
    new_drug = if_else(
      !is.na(prev_drug) & drug_ingredient != prev_drug,
      1, 0
    ),
    switch_number = cumsum(new_drug)
  ) %>%
  summarise(
    n_unique_drugs = n_distinct(drug_ingredient),
    n_switch = max(switch_number, na.rm = TRUE),
    switch_TRD_date = if_else(
      any(switch_number >= 2),
      min(drug_start[switch_number >= 2], na.rm = TRUE),
      as.Date(NA)
    ),
    .groups = "drop"
  )
# Test if this approach works or not
sample_ids <- sample(switch_df$person_id, 5)
med_clean %>%
  filter(person_id %in% sample_ids) %>%
  arrange(person_id, drug_start) %>%
  select(person_id, drug_ingredient, drug_start)
switch_df %>%
  filter(person_id %in% sample_ids)
######### Identify Augmentation ##########
augmentation_drugs <- c("Antipsychotic", "Augmentation_other", "Atypical_AD")
base_antidepressants <- c("SSRI", "SNRI", "TCA", "MAOI")
augmentation_df <- med_clean %>%
  arrange(person_id, drug_start) %>% # We Compare one medication with the next one, they need to be in chronological order
  group_by(person_id) %>%
  mutate(
    next_group = lead(med_group), # lead() means:look at the next row, So if current row is: SSRI and next row is: Antipsychotic
    # then next_group becomes "Antipsychotic".
    next_start = lead(drug_start), # Again, lead() looks one row ahead.So now for each current medication,
    # you know when the next medication begins.
    overlap = if_else( # This asks: does the next medication start before the current medication ends?
      !is.na(next_start) & drug_end >= next_start, # If yes, then they overlap.
      1, 0
    ),
    augmentation = if_else(
      overlap == 1 &
        med_group %in% c("SSRI", "SNRI", "TCA", "MAOI") &
        next_group %in% c("Antipsychotic", "Augmentation_other", "Atypical_AD"),
      1, 0
    )
  ) %>%
  summarise( # We want just one person-level variable:0 = no augmentation
    # 1 = had augmentation at least once
    augmentation_flag = max(augmentation, na.rm = TRUE),
    .groups = "drop"
  )
# ---- Create TRD date (the day they diagnosied as TRD patients) ----
# Augmentation_date_df <- med_clean %>%
augmentation_date_df <- med_clean %>%
  arrange(person_id, drug_start) %>%
  group_by(person_id) %>%
  mutate(
    next_group = lead(med_group),
    next_start = lead(drug_start),
    overlap = if_else(
      !is.na(next_start) & !is.na(drug_end) & drug_end >= next_start,
      1, 0
    ),
    augmentation = if_else(
      overlap == 1 &
        med_group %in% c("SSRI", "SNRI", "TCA", "MAOI") &
        next_group %in% c("Antipsychotic", "Augmentation_other", "Atypical_AD"),
      1, 0
    )
  ) %>%
  filter(augmentation == 1) %>%
  summarise(
    augmentation_TRD_date = min(next_start, na.rm = TRUE),
    .groups = "drop"
  )
# Ketamine TRD date
ketamine_date_df <- med_clean %>%
  filter(ketamine_TRD == 1) %>%
  group_by(person_id) %>%
  summarise(
    ketamine_TRD_date = min(drug_start),
    .groups = "drop"
  )
# Combine TRD dates
trd_date_df <- switch_df %>%
  select(person_id, switch_TRD_date) %>%
  full_join(augmentation_date_df, by = "person_id") %>%
  full_join(ketamine_date_df, by = "person_id") %>%
  mutate(
    TRD_date = pmin(
      switch_TRD_date,
      augmentation_TRD_date,
      ketamine_TRD_date,
      na.rm = TRUE
    )
  )
# Add buffer window (10 days before TRD)
trd_date_df <- trd_date_df %>%
  mutate(
    TRD_window_start = TRD_date - 14
  )
# Check how many patients got each type of TRD date
trd_date_df %>%
  summarise(
    n_switch_date = sum(!is.na(switch_TRD_date)),
    n_aug_date = sum(!is.na(augmentation_TRD_date)),
    n_ketamine_date = sum(!is.na(ketamine_TRD_date)),
    n_TRD_date = sum(!is.na(TRD_date))
  )
# Check that TRD_date is the earliest of the three
trd_date_df %>%
  select(person_id, switch_TRD_date, augmentation_TRD_date, ketamine_TRD_date, TRD_date) %>%
  head(20)
# Check that the window start is exactly 14 days before
trd_date_df %>%
  mutate(diff_days = as.numeric(TRD_date - TRD_window_start)) %>%
  count(diff_days)
# Check date ranges
trd_date_df %>%
  summarise(
    min_TRD_date = min(TRD_date, na.rm = TRUE),
    max_TRD_date = max(TRD_date, na.rm = TRUE)
  )
# Check whether TRD_window_start ever comes after TRD_date
trd_date_df %>%
  filter(TRD_window_start > TRD_date)
# ---- Create final medication dataset ----
# Person-level ketamine flag
ketamine_df <- med_clean %>%
  group_by(person_id) %>%
  summarise(
    ketamine_flag = max(ketamine_TRD, na.rm = TRUE),
    .groups = "drop"
  )
# Final medication dataset
med_final_df <- switch_df %>%
  full_join(augmentation_df, by = "person_id") %>%
  full_join(ketamine_df, by = "person_id") %>%
  full_join(augmentation_date_df, by = "person_id") %>%
  full_join(ketamine_date_df, by = "person_id") %>%
  mutate(
    n_unique_drugs = if_else(is.na(n_unique_drugs), 0, n_unique_drugs),
    n_switch = if_else(is.na(n_switch), 0, n_switch),
    augmentation_flag = if_else(is.na(augmentation_flag), 0, augmentation_flag),
    ketamine_flag = if_else(is.na(ketamine_flag), 0, ketamine_flag),
    TRD_by_switch = if_else(!is.na(switch_TRD_date), 1, 0),
    TRD_by_augmentation = augmentation_flag,
    TRD_by_ketamine = ketamine_flag
  ) %>%
  rowwise() %>%
  mutate(
    TRD_date = {
      x <- c(switch_TRD_date, augmentation_TRD_date, ketamine_TRD_date)
      x <- x[!is.na(x)]
      if (length(x) == 0) as.Date(NA) else min(x)
    },
    TRD = if_else(
      TRD_by_switch == 1 |
        TRD_by_augmentation == 1 |
        TRD_by_ketamine == 1,
      1, 0
    ),
    TRD_window_start = if_else(!is.na(TRD_date), TRD_date - 14, as.Date(NA))
  ) %>%
  ungroup() %>%
  select(
    person_id,
    n_unique_drugs,
    n_switch,
    augmentation_flag,
    ketamine_flag,
    TRD_by_switch,
    TRD_by_augmentation,
    TRD_by_ketamine,
    switch_TRD_date,
    augmentation_TRD_date,
    ketamine_TRD_date,
    TRD_date,
    TRD_window_start,
    TRD
  )
# Check the result
med_final_df %>% count(TRD)
med_final_df %>% count(TRD_by_switch)
med_final_df %>% count(TRD_by_augmentation)
med_final_df %>% count(TRD_by_ketamine)
head(med_final_df)
# ---- Fix an issue and check the final dataset ----
# Some TRD patients has no TRD date
# Find TRD patients with missing TRD date
trd_missing_date <- med_final_df %>%
  filter(TRD == 1, is.na(TRD_date))
nrow(trd_missing_date)
head(trd_missing_date)
# See which TRD mechanism flagged them
trd_missing_date %>%
  summarise(
    n_missing = n(),
    by_switch = sum(TRD_by_switch == 1, na.rm = TRUE),
    by_augmentation = sum(TRD_by_augmentation == 1, na.rm = TRUE),
    by_ketamine = sum(TRD_by_ketamine == 1, na.rm = TRUE)
  )
colnames(switch_df)
colnames(trd_date_df)
# Make sure TRD_date is always the earliest of the 3 dates
med_final_df %>%
  filter(TRD == 1) %>%
  mutate(
    earliest_date = pmin(
      switch_TRD_date,
      augmentation_TRD_date,
      ketamine_TRD_date,
      na.rm = TRUE
    )
  ) %>%
  filter(TRD_date != earliest_date)
# Make sure TRD_window_start is always 14 days before TRD_date
med_final_df %>%
  filter(TRD == 1) %>%
  mutate(diff_days = as.numeric(TRD_date - TRD_window_start)) %>%
  count(diff_days)
# Check date classes
class(med_final_df$switch_TRD_date)
class(med_final_df$augmentation_TRD_date)
class(med_final_df$ketamine_TRD_date)
class(med_final_df$TRD_date)
class(med_final_df$TRD_window_start)
# Check for duplicate person_id
med_final_df %>%
  count(person_id) %>%
  filter(n > 1)
# Check whether counts make sense
med_final_df %>%
  summarise(
    n_people = n(),
    n_trd = sum(TRD == 1, na.rm = TRUE),
    n_nontrd = sum(TRD == 0, na.rm = TRUE),
    n_switch_date = sum(!is.na(switch_TRD_date)),
    n_aug_date = sum(!is.na(augmentation_TRD_date)),
    n_ketamine_date = sum(!is.na(ketamine_TRD_date))
  )
