# Descriptive and multivariable analyses
# HGEN 603 final project
# Requires authorized access to the All of Us Researcher Workbench Controlled Tier.
# No participant-level data are included in this repository.

# ---- Analysis ----
# ---- Step1: Descriptive dataset for Table1 and figures ----
# Make a table for demographic part
# Make age groups
library(dplyr)
library(gtsummary)
library(gt)
# Create clean Table 1 dataset
table1_df <- final_df %>%
  mutate(
    age_group = case_when(
      age >= 18 & age <= 25 ~ "18-25",
      age >= 26 & age <= 40 ~ "26-40",
      age >= 41 & age <= 65 ~ "41-65",
      age > 65 ~ ">65",
      TRUE ~ NA_character_
    ),
    age_group = factor(age_group, levels = c("18-25", "26-40", "41-65", ">65")),
    # Convert binary variables from 0/1 to No/Yes
    across(
      c(
        has_anxiety, has_bipolar, has_oud, has_htn,
        fh_depression, fh_anxiety, fh_substance
      ),
      ~ factor(.x, levels = c(0, 1), labels = c("No", "Yes"))
    )
  ) %>%
  select(
    TRD,
    age_group,
    sex_at_birth,
    race,
    ethnicity,
    has_anxiety,
    has_bipolar,
    has_oud,
    has_htn,
    fh_depression,
    fh_anxiety,
    fh_substance,
    n_psy_visits,
    n_unique_drugs
  )
# Create Table 1
table1 <- table1_df %>%
  tbl_summary(
    by = TRD,
    type = list(
      c(
        has_anxiety, has_bipolar, has_oud, has_htn,
        fh_depression, fh_anxiety, fh_substance
      ) ~ "categorical"
    ),
    label = list(
      age_group ~ "Age group",
      sex_at_birth ~ "Sex at birth",
      race ~ "Race",
      ethnicity ~ "Ethnicity",
      has_anxiety ~ "Anxiety",
      has_bipolar ~ "Bipolar disorder",
      has_oud ~ "Opioid use disorder",
      has_htn ~ "Hypertension",
      fh_depression ~ "Family history: Depression",
      fh_anxiety ~ "Family history: Anxiety",
      fh_substance ~ "Family history: Substance use",
      n_psy_visits ~ "Medical visits in 1 year",
      n_unique_drugs ~ "Number of unique medications"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_p() %>%
  bold_labels()
# Convert to gt
gt_table <- as_gt(table1)
# ---- Clinical comorbidities and Family history plot ----
# Make TRD vs Non-TRD comparison plot (bar plot of key variables)
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggplot2)
# Create wide summary
plot_df_wide <- final_df %>%
  group_by(TRD) %>%
  summarise(
    Anxiety = mean(has_anxiety == "1", na.rm = TRUE),
    Bipolar = mean(has_bipolar == "1", na.rm = TRUE),
    OUD = mean(has_oud == "1", na.rm = TRUE),
    Hypertension = mean(has_htn == "1", na.rm = TRUE),
    `Depression` = mean(fh_depression == "1", na.rm = TRUE),
    `Anxiety FH` = mean(fh_anxiety == "1", na.rm = TRUE),
    .groups = "drop"
  )
# Convert to long format without pivot_longer
plot_df <- data.frame(
  TRD = rep(plot_df_wide$TRD, each = 6),
  Variable = rep(names(plot_df_wide)[-1], times = nrow(plot_df_wide)),
  Proportion = as.numeric(t(plot_df_wide[, -1]))
)
# Add group/category labels
plot_df <- plot_df %>%
  mutate(
    Category = case_when(
      Variable %in% c("Anxiety", "Bipolar", "OUD", "Hypertension") ~ "Clinical comorbidities",
      Variable %in% c("Depression", "Anxiety FH") ~ "Family history"
    ),
    Variable = case_when(
      Variable == "OUD" ~ "Opioid use disorder",
      Variable == "Anxiety FH" ~ "Anxiety",
      TRUE ~ Variable
    )
  )
# P-values
p_df <- data.frame(
  Variable = c("Anxiety", "Bipolar", "Opioid use disorder", "Hypertension", "Depression", "Anxiety"),
  Category = c(
    "Clinical comorbidities",
    "Clinical comorbidities",
    "Clinical comorbidities",
    "Clinical comorbidities",
    "Family history",
    "Family history"
  ),
  p_value = c(
    chisq.test(table(final_df$TRD, final_df$has_anxiety))$p.value,
    chisq.test(table(final_df$TRD, final_df$has_bipolar))$p.value,
    chisq.test(table(final_df$TRD, final_df$has_oud))$p.value,
    chisq.test(table(final_df$TRD, final_df$has_htn))$p.value,
    chisq.test(table(final_df$TRD, final_df$fh_depression))$p.value,
    chisq.test(table(final_df$TRD, final_df$fh_anxiety))$p.value
  )
) %>%
  mutate(
    sig_label = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )
# Significance label positions
label_df <- plot_df %>%
  group_by(Category, Variable) %>%
  summarise(y_position = max(Proportion) + 0.05, .groups = "drop") %>%
  left_join(p_df, by = c("Category", "Variable"))
# Plot
ggplot(plot_df, aes(x = Variable, y = Proportion, fill = TRD)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_text(
    aes(label = paste0(round(Proportion * 100, 1), "%")),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 3,
    fontface = "bold"
  ) +
  geom_text(
    data = label_df,
    aes(x = Variable, y = y_position, label = sig_label),
    inherit.aes = FALSE,
    size = 5,
    fontface = "bold"
  ) +
  facet_wrap(~Category, scales = "free_x") +
  scale_fill_manual(
    values = c("Non-TRD" = "#E76F51", "TRD" = "#2A9D8F"),
    labels = c("Non-TRD", "TRD")
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x * 100, "%"),
    limits = c(0, 0.9)
  ) +
  labs(
    title = "Clinical Comorbidities and Family History",
    x = NULL,
    y = "Prevalence (%)",
    fill = "TRD status",
    caption = "* p < 0.05, ** p < 0.01, *** p < 0.001"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey85"),
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 13)
  )
# ---- Race and Ethnicity distribution plot ----
library(dplyr)
library(ggplot2)
# Colorblind-friendly palette (Okabe-Ito inspired)
cb_palette <- c(
  "#0072B2", "#D55E00", "#CC79A7",
  "#009E73", "#E69F00", "#56B4E9",
  "#F0E442", "#332288", "#882255"
)
race_plot_df <- final_df %>%
  filter(!is.na(race)) %>%
  count(race) %>%
  mutate(
    prop = n / sum(n),
    percent_label = paste0(round(prop * 100, 1), "%")
  ) %>%
  arrange(prop) %>%
  mutate(race = factor(race, levels = race))
race_plot <- ggplot(race_plot_df, aes(x = prop, y = race, fill = race)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = percent_label),
    hjust = -0.1,
    size = 4.5,
    fontface = "bold"
  ) +
  scale_fill_manual(values = cb_palette) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Race Distribution",
    x = "Percentage of participants",
    y = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5), # centered
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold", size = 13),
    axis.text.x = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey85"),
    plot.margin = margin(10, 50, 10, 10)
  )
race_plot
# Pie chart for ethnicity
eth_palette <- c("#0072B2", "#D55E00")
eth_plot_df <- final_df %>%
  filter(!is.na(ethnicity)) %>%
  count(ethnicity) %>%
  mutate(
    prop = n / sum(n),
    percent_label = paste0(round(prop * 100, 1), "%")
  ) %>%
  arrange(prop) %>%
  mutate(ethnicity = factor(ethnicity, levels = ethnicity))
eth_plot <- ggplot(eth_plot_df, aes(x = prop, y = ethnicity, fill = ethnicity)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(
    aes(label = percent_label),
    hjust = -0.1,
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_manual(values = eth_palette) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Ethnicity Distribution",
    x = "Percentage of participants",
    y = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5), # centered
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.text.x = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey85"),
    plot.margin = margin(10, 50, 10, 10)
  )
eth_plot
# ---- Age plot ----
# Age plot
age_plot_df2 <- final_df %>%
  mutate(
    age_group = case_when(
      age >= 18 & age <= 25 ~ "18-25",
      age >= 26 & age <= 40 ~ "26-40",
      age >= 41 & age <= 65 ~ "41-65",
      age > 65 ~ ">65",
      TRUE ~ NA_character_
    ),
    age_group = factor(age_group, levels = c("18-25", "26-40", "41-65", ">65"))
  ) %>%
  filter(!is.na(age_group)) %>%
  count(TRD, age_group) %>%
  group_by(TRD) %>%
  mutate(
    prop = n / sum(n),
    percent_label = paste0(round(prop * 100, 1), "%")
  ) %>%
  ungroup()
age_donut_plot <- ggplot(age_plot_df2, aes(x = 2, y = prop, fill = age_group)) +
  geom_col(color = "white", linewidth = 0.8, width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~TRD) +
  xlim(0.5, 2.5) +
  geom_text(
    aes(label = percent_label),
    position = position_stack(vjust = 0.5),
    size = 5,
    fontface = "bold",
    color = "white"
  ) +
  scale_fill_manual(
    values = c(
      "18-25" = "#332288", # dark purple-blue
      "26-40" = "#0072B2", # blue
      "41-65" = "#CC6677", # red/pink
      ">65"   = "#009E73" # green
    )
  ) +
  labs(
    title = "Age Distribution by TRD Status",
    fill = "Age group"
  ) +
  theme_void(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5, color = "white"),
    strip.text = element_text(face = "bold", size = 18, color = "white"),
    legend.title = element_text(face = "bold", size = 15, color = "white"),
    legend.text = element_text(face = "bold", size = 13, color = "white"),
    legend.position = "right"
  )
age_donut_plot
ggsave("age_distribution_by_TRD_donut.png", age_donut_plot,
  width = 12, height = 6, dpi = 300
)
# ---- Sex at birth plot ----
# Sex plot
sex_plot_df <- final_df %>%
  filter(!is.na(TRD), !is.na(sex_at_birth)) %>%
  count(TRD_status = TRD, sex_at_birth, name = "n") %>%
  group_by(TRD_status) %>%
  mutate(
    prop = n / sum(n),
    percent_label = ifelse(prop < 0.01, "", paste0(round(prop * 100, 1), "%")),
    TRD_status = factor(TRD_status, levels = c("Non-TRD", "TRD")),
    sex_at_birth = factor(sex_at_birth, levels = c("Female", "Male", "Intersex"))
  ) %>%
  ungroup()
sex_donut_plot <- ggplot(sex_plot_df, aes(x = 2, y = prop, fill = sex_at_birth)) +
  geom_col(color = "white", linewidth = 0.8, width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~TRD_status) +
  xlim(0.5, 2.5) +
  geom_text(
    aes(label = percent_label),
    position = position_stack(vjust = 0.5),
    size = 5,
    fontface = "bold",
    color = "white"
  ) +
  scale_fill_manual(
    values = c(
      "Female" = "#CC6677",
      "Male" = "#0072B2",
      "Intersex" = "#009E73"
    )
  ) +
  labs(
    title = "Sex at Birth Distribution by TRD Status",
    fill = "Sex at birth"
  ) +
  theme_void(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5, color = "white"),
    strip.text = element_text(face = "bold", size = 18, color = "white"),
    legend.title = element_text(face = "bold", size = 15, color = "white"),
    legend.text = element_text(face = "bold", size = 13, color = "white"),
    legend.position = "right"
  )
sex_donut_plot
ggsave(
  "sex_at_birth_by_TRD_donut.png",
  sex_donut_plot,
  width = 10,
  height = 6,
  dpi = 300
)
# ---- Visits and unique medications plots ----
# Check visits distribution: dark-theme friendly plots
library(dplyr)
library(ggplot2)
hist_plot <- ggplot(
  final_df %>%
    filter(!is.na(n_psy_visits), !is.na(TRD), n_psy_visits <= 100),
  aes(x = n_psy_visits, fill = TRD)
) +
  geom_histogram(
    binwidth = 1,
    color = "black",
    alpha = 0.8
  ) +
  facet_wrap(~TRD, scales = "free_y") +
  scale_fill_manual(
    values = c("Non-TRD" = "#332288", "TRD" = "#CC6677")
  ) +
  labs(
    title = "Distribution of Medical Visits by TRD Status",
    x = "Number of visits (1 year)",
    y = "Number of participants",
    fill = "TRD status"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    strip.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "top",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank()
  )
hist_plot
ggsave(
  "hist_psy_visits_TRD_white.png",
  hist_plot,
  width = 10,
  height = 6,
  dpi = 300
)
# Summary table
visit_summary <- final_df %>%
  filter(!is.na(n_psy_visits), !is.na(TRD)) %>%
  group_by(TRD) %>%
  summarise(
    n = n(),
    mean = mean(n_psy_visits, na.rm = TRUE),
    sd = sd(n_psy_visits, na.rm = TRUE),
    median = median(n_psy_visits, na.rm = TRUE),
    q1 = quantile(n_psy_visits, 0.25, na.rm = TRUE),
    q3 = quantile(n_psy_visits, 0.75, na.rm = TRUE),
    max = max(n_psy_visits, na.rm = TRUE),
    .groups = "drop"
  )
visit_summary
# Boxplot of visits
box_plot <- ggplot(
  final_df %>%
    filter(!is.na(n_psy_visits), !is.na(TRD), n_psy_visits <= 30),
  aes(x = TRD, y = n_psy_visits, fill = TRD)
) +
  geom_boxplot(
    width = 0.6,
    outlier.alpha = 0.2,
    color = "black"
  ) +
  scale_fill_manual(values = c("Non-TRD" = "#332288", "TRD" = "#CC6677")) +
  labs(
    title = "Medical Visits by TRD Status (<= 30)",
    x = NULL,
    y = "Number of Medical visits (1 year)"
  ) +
  theme_bw(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text = element_text(face = "bold"),
    legend.position = "none"
  )
box_plot
ggsave(
  "boxplot_TRD.png",
  box_plot,
  width = 10,
  height = 6,
  dpi = 300
)
# Summary of visits to see the outliers and reasons of skewness
# Summary by group
final_df %>%
  group_by(TRD) %>%
  summarise(
    n = n(),
    median_visits = median(n_psy_visits, na.rm = TRUE),
    q1 = quantile(n_psy_visits, 0.25, na.rm = TRUE),
    q3 = quantile(n_psy_visits, 0.75, na.rm = TRUE),
    iqr = IQR(n_psy_visits, na.rm = TRUE),
    upper_outlier_cutoff = q3 + 1.5 * iqr,
    max_visits = max(n_psy_visits, na.rm = TRUE)
  )
# Count high outliers using the usual boxplot rule within each TRD group
high_visit_outliers <- final_df %>%
  filter(!is.na(n_psy_visits), !is.na(TRD)) %>%
  group_by(TRD) %>%
  mutate(
    q1 = quantile(n_psy_visits, 0.25, na.rm = TRUE),
    q3 = quantile(n_psy_visits, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    high_outlier_cutoff = q3 + 1.5 * iqr,
    high_visit_outlier = n_psy_visits > high_outlier_cutoff
  ) %>%
  ungroup()
high_visit_outliers %>%
  group_by(TRD) %>%
  summarise(
    cutoff = first(high_outlier_cutoff),
    n_high_outliers = sum(high_visit_outlier),
    percent_high_outliers = round(100 * mean(high_visit_outlier), 2),
    max_visits = max(n_psy_visits)
  )
final_df %>%
  filter(!is.na(n_psy_visits), !is.na(TRD)) %>%
  group_by(TRD) %>%
  summarise(
    n_over_30 = sum(n_psy_visits > 30),
    pct_over_30 = round(100 * mean(n_psy_visits > 30), 2),
    n_over_50 = sum(n_psy_visits > 50),
    pct_over_50 = round(100 * mean(n_psy_visits > 50), 2),
    n_over_100 = sum(n_psy_visits > 100),
    pct_over_100 = round(100 * mean(n_psy_visits > 100), 2),
    max_visits = max(n_psy_visits)
  )
# Unique medication plot
library(ggplot2)
library(dplyr)
box_plot <- ggplot(
  final_df %>% filter(!is.na(n_unique_drugs), !is.na(TRD)),
  aes(x = TRD, y = n_unique_drugs, fill = TRD)
) +
  geom_boxplot(
    width = 0.5,
    outlier.alpha = 0.2
  ) +
  scale_fill_manual(values = c("Non-TRD" = "#332288", "TRD" = "#CC6677")) +
  labs(
    title = "Medication Burden by TRD Status",
    x = NULL,
    y = "Number of unique medications"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  )
box_plot
# Save the plot
ggsave(
  "boxplot_medication_TRD.png",
  box_plot,
  width = 7,
  height = 5,
  dpi = 300
)
# ---- Step2: Compare measurements between groups ----
colnames(final_df)
# Final measurement variables to use
measure_vars <- c("bmi", "sbp", "dbp", "crp", "tsh", "vitamin_d", "phq9")
# ---- Create clean measurement dataset ----
library(dplyr)
library(ggplot2)
library(tidyr)
# Unify TRD and non-TRD windows into one column per variable
measure_df <- final_df %>%
  mutate(
    BMI = ifelse(TRD == "TRD", bmi_TRD_window, bmi_nonTRD_window),
    Sbp = ifelse(TRD == "TRD", sbp_TRD_window, sbp_nonTRD_window),
    Dbp = ifelse(TRD == "TRD", dbp_TRD_window, dbp_nonTRD_window),
    CRP = ifelse(TRD == "TRD", crp_TRD_window, crp_nonTRD_window),
    TSH = ifelse(TRD == "TRD", tsh_TRD_window, tsh_nonTRD_window),
    VitaminD = ifelse(TRD == "TRD", vitamin_d_TRD_window, vitamin_d_nonTRD_window),
    PHQ9 = ifelse(TRD == "TRD", phq9_TRD_window, phq9_nonTRD_window)
  ) %>%
  select(TRD, BMI, Sbp, Dbp, CRP, TSH, VitaminD, PHQ9)
# Check distribution
# Put all measurements in long format
measure_long <- measure_df %>%
  pivot_longer(
    cols = -TRD,
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value))
# Faceted histograms (ALL variables)
hist_all <- ggplot(measure_long, aes(x = Value, fill = TRD)) +
  geom_histogram(
    bins = 40,
    alpha = 0.7,
    color = "black"
  ) +
  facet_wrap(~Variable, scales = "free") +
  scale_fill_manual(values = c("Non-TRD" = "#332288", "TRD" = "#CC6677")) +
  labs(
    title = "Distribution of Clinical Measurements",
    x = NULL,
    y = "Count",
    fill = "TRD status"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    legend.position = "top"
  )
hist_all
ggsave("measurements_hist.png", hist_all,
  width = 12, height = 6, dpi = 300
)
# Boxplots for ALL variables
box_all <- ggplot(measure_long, aes(x = TRD, y = Value, fill = TRD)) +
  geom_boxplot(outlier.alpha = 0.2) +
  facet_wrap(~Variable, scales = "free") +
  scale_fill_manual(values = c("Non-TRD" = "#332288", "TRD" = "#CC6677")) +
  labs(
    title = "Clinical Measurements by TRD Status",
    x = NULL,
    y = NULL
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )
box_all
ggsave("measurements_boxplot.png", box_all,
  width = 12, height = 6, dpi = 300
)
# Summary of PHQ-9 by TRD
phq9_summary <- measure_df %>%
  filter(!is.na(PHQ9)) %>%
  group_by(TRD) %>%
  summarise(
    n = n(),
    mean = mean(PHQ9, na.rm = TRUE),
    sd = sd(PHQ9, na.rm = TRUE),
    median = median(PHQ9, na.rm = TRUE),
    q1 = quantile(PHQ9, 0.25, na.rm = TRUE),
    q3 = quantile(PHQ9, 0.75, na.rm = TRUE),
    min = min(PHQ9, na.rm = TRUE),
    max = max(PHQ9, na.rm = TRUE),
    .groups = "drop"
  )
phq9_summary
# ---- Clean summary table for measurements ----
library(dplyr)
library(gtsummary)
library(gt)
# Create clean measurement summary table
measure_table <- measure_df %>%
  tbl_summary(
    by = TRD,
    label = list(
      BMI ~ "BMI",
      Sbp ~ "Systolic blood pressure",
      Dbp ~ "Diastolic blood pressure",
      CRP ~ "CRP",
      TSH ~ "TSH",
      VitaminD ~ "Vitamin D",
      PHQ9 ~ "PHQ-9 score"
    ),
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})"
    ),
    missing = "no"
  ) %>%
  add_p(
    test = all_continuous() ~ "wilcox.test"
  ) %>%
  modify_header(
    label ~ "**Clinical measurement**",
    stat_1 ~ "**Non-TRD**",
    stat_2 ~ "**TRD**",
    p.value ~ "**p-value**"
  ) %>%
  modify_caption("**Clinical Measurements by TRD Status**") %>%
  bold_labels()
# Save as HTML and Word
gt_measure_table <- as_gt(measure_table)
gtsave(gt_measure_table, "clinical_measurements_by_TRD.html")
gtsave(gt_measure_table, "clinical_measurements_by_TRD.docx")
# ---- Step3: Association analysis ----
# ---- Make a new regression dataframe ----
# Create clean regression dataset
regression_df <- final_df %>%
  select(
    person_id,
    # Outcome
    TRD,
    # Demographics
    age,
    sex_at_birth,
    race,
    ethnicity,
    # Psychiatric and medical comorbidities
    has_anxiety,
    has_bipolar,
    has_oud,
    has_htn,
    # Family history
    fh_depression,
    fh_anxiety,
    fh_bipolar,
    fh_ptsd,
    fh_substance,
    # Healthcare utilization
    n_psy_visits
  ) %>%
  mutate(
    TRD = factor(TRD, levels = c("Non-TRD", "TRD")),
    sex_at_birth = factor(sex_at_birth),
    race = factor(race),
    ethnicity = factor(ethnicity),
    has_anxiety = factor(has_anxiety, levels = c(0, 1)),
    has_bipolar = factor(has_bipolar, levels = c(0, 1)),
    has_oud = factor(has_oud, levels = c(0, 1)),
    has_htn = factor(has_htn, levels = c(0, 1)),
    fh_depression = factor(fh_depression, levels = c(0, 1)),
    fh_anxiety = factor(fh_anxiety, levels = c(0, 1)),
    fh_bipolar = factor(fh_bipolar, levels = c(0, 1)),
    fh_ptsd = factor(fh_ptsd, levels = c(0, 1)),
    fh_substance = factor(fh_substance, levels = c(0, 1)),
    log_visits = log1p(n_psy_visits)
  ) %>%
  drop_na()
# ---- Run logistic regression ----
library(tidyverse)
library(broom)
library(gtsummary)
library(car)
library(pROC)
# Check sample size and outcome balance
nrow(regression_df)
table(regression_df$TRD)
prop.table(table(regression_df$TRD))
# Fit main adjusted logistic regression model
model_main <- glm(
  TRD ~ age +
    sex_at_birth +
    race +
    ethnicity +
    has_anxiety +
    has_bipolar +
    has_oud +
    has_htn +
    fh_depression +
    fh_anxiety +
    fh_bipolar +
    fh_ptsd +
    fh_substance +
    log_visits,
  data = regression_df,
  family = binomial()
)
summary(model_main)
# Check multicollinearity
vif(model_main)
# Odds ratio table with 95% confidence intervals
model_results <- tidy(
  model_main,
  exponentiate = TRUE,
  conf.int = TRUE
)
model_results
# Publication-style regression table
tbl_model_main <- tbl_regression(
  model_main,
  exponentiate = TRUE
)
# ---- save model results table in a publish-ready style ----
library(gtsummary)
library(dplyr)
tbl_model_main_clean <- tbl_regression(
  model_main,
  exponentiate = TRUE,
  label = list(
    has_anxiety ~ "Anxiety",
    has_bipolar ~ "Bipolar disorder",
    has_oud ~ "Opioid use disorder",
    has_htn ~ "Hypertension",
    fh_depression ~ "Family history: depression",
    fh_anxiety ~ "Family history: anxiety",
    fh_bipolar ~ "Family history: bipolar disorder",
    fh_ptsd ~ "Family history: PTSD",
    fh_substance ~ "Family history: substance use",
    log_visits ~ "Log-transformed visits"
  )
) %>%
  bold_labels() %>%
  modify_table_body(
    ~ .x %>%
      filter(
        !(variable %in% c(
          "has_anxiety", "has_bipolar", "has_oud", "has_htn",
          "fh_depression", "fh_anxiety", "fh_bipolar",
          "fh_ptsd", "fh_substance"
        ) & reference_row == TRUE)
      ) %>%
      mutate(
        label = case_when(
          variable == "has_anxiety" & label == "1" ~ "Anxiety",
          variable == "has_bipolar" & label == "1" ~ "Bipolar disorder",
          variable == "has_oud" & label == "1" ~ "Opioid use disorder",
          variable == "has_htn" & label == "1" ~ "Hypertension",
          variable == "fh_depression" & label == "1" ~ "Family history: depression",
          variable == "fh_anxiety" & label == "1" ~ "Family history: anxiety",
          variable == "fh_bipolar" & label == "1" ~ "Family history: bipolar disorder",
          variable == "fh_ptsd" & label == "1" ~ "Family history: PTSD",
          variable == "fh_substance" & label == "1" ~ "Family history: substance use",
          TRUE ~ label
        )
      )
  )
# Save clean table
tbl_model_main_clean %>%
  as_gt() %>%
  gt::gtsave("model_results.html")
# Model performance: ROC and AUC
library(ggplot2)

roc_main <- pROC::roc(
  response = regression_df$TRD,
  predictor = fitted(model_main),
  levels = c("Non-TRD", "TRD"),
  direction = "<",
  quiet = TRUE
)

# Extract ROC data
roc_df <- data.frame(
  specificity = roc_main$specificities,
  sensitivity = roc_main$sensitivities
)
auc_value <- auc(roc_main)
# Plot
roc_plot <- ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    title = "ROC Curve for TRD Prediction",
    subtitle = paste0("AUC = ", round(auc_value, 3))
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )
roc_plot
ggsave("ROC_TRD.png", roc_plot, width = 6, height = 6, dpi = 300)
# compare nested models
model_demo <- glm(
  TRD ~ age + sex_at_birth + race + ethnicity,
  data = regression_df,
  family = binomial()
)
model_clinical <- glm(
  TRD ~ age +
    sex_at_birth +
    race +
    ethnicity +
    has_anxiety +
    has_bipolar +
    has_oud +
    has_htn,
  data = regression_df,
  family = binomial()
)
model_full <- glm(
  TRD ~ age +
    sex_at_birth +
    race +
    ethnicity +
    has_anxiety +
    has_bipolar +
    has_oud +
    has_htn +
    fh_depression +
    fh_anxiety +
    fh_bipolar +
    fh_ptsd +
    fh_substance +
    log_visits,
  data = regression_df,
  family = binomial()
)
anova(model_demo, model_clinical, model_full, test = "Chisq")
