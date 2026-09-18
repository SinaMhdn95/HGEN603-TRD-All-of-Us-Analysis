# Analysis workflow

## 1. Cohort and source domains

The project starts with an MDD cohort defined in the All of Us Researcher Workbench and extracts six OMOP domains: person, condition occurrence, drug exposure, measurement, survey, and visit occurrence.

## 2. Demographic variables

The person table provides date of birth, race, ethnicity, and sex at birth. Non-informative survey responses are recoded as missing before analysis.

## 3. Clinical conditions

Condition records are deduplicated and grouped into MDD, anxiety-related conditions, bipolar disorder, opioid use disorder, hypertension, diabetes, PTSD or stress-related conditions, and other conditions. Person-level indicators and first diagnosis dates are then derived.

## 4. Medication timeline and treatment-escalation proxy

Medication records are standardized to ingredient-level names and ordered chronologically. The treatment-escalation proxy is positive when at least one of the following conditions is met:

- two or more switches among antidepressant classes;
- overlapping antidepressant and augmentation therapy; or
- esketamine exposure or repeated ketamine exposure consistent with treatment use.

The earliest qualifying date becomes the proxy TRD date.

## 5. Measurements and family history

Clinical measurements are harmonized across units, filtered using biologically plausible ranges, and summarized within prespecified windows around the index date. Survey responses provide family-history indicators for depression, anxiety, bipolar disorder, PTSD, and substance use disorders.

## 6. Healthcare utilization

Visit records are classified into psychiatric-related emergency, inpatient, and outpatient encounters. Utilization is summarized within one year of the participant-specific anchor date.

## 7. Statistical analysis

The project uses descriptive summaries, Pearson chi-squared tests, Wilcoxon rank-sum tests, and multivariable logistic regression. Visit counts are transformed with `log1p`. Model discrimination is summarized with a receiver operating characteristic curve and area under the curve.

## 8. Interpretation

The outcome is an EHR-based treatment-escalation proxy, not a confirmed clinical diagnosis of treatment-resistant depression. Associations should not be interpreted as causal effects or as evidence that the model is ready for clinical use.
