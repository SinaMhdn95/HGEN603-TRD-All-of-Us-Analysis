# Package checks for the HGEN 603 final project

required_packages <- c(
  "bigrquery",
  "broom",
  "car",
  "dplyr",
  "ggplot2",
  "gt",
  "gtsummary",
  "lubridate",
  "naniar",
  "patchwork",
  "pROC",
  "purrr",
  "readr",
  "scales",
  "stringr",
  "tidyr",
  "tidyverse"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages in the Researcher Workbench before running the analysis: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

required_environment_variables <- c(
  "WORKSPACE_BUCKET",
  "WORKSPACE_CDR",
  "GOOGLE_PROJECT",
  "OWNER_EMAIL"
)

missing_environment_variables <- required_environment_variables[
  !nzchar(Sys.getenv(required_environment_variables))
]

if (length(missing_environment_variables) > 0) {
  stop(
    "This workflow must run in an All of Us Researcher Workbench workspace. Missing environment variables: ",
    paste(missing_environment_variables, collapse = ", "),
    call. = FALSE
  )
}
