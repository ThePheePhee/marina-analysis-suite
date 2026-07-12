# Data Cleaning Decision Log

## Tentative age resolutions

The following rules are intentionally provisional. Affected records remain visible in the dashboard’s **Cleaning** pane and still require confirmation against the source data.

- A raw age of `8.5` is floored to `8` for analysis.
- `1st grade` is assigned the expected midpoint age of `6.5` years.
- `2nd grade` is assigned the expected midpoint age of `7.5` years.
- `4th grade` is assigned the expected midpoint age of `9.5` years.

The cleaned `age` field is the single value used by descriptive statistics, age groups, correlations, regression adjustment, interaction models, research-question outputs, and the static dashboard. The raw entry is retained alongside the tentative status and rationale; the source workbook is not overwritten.
