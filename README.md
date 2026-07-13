# Marina Analysis Suite

Reproducible R analysis and Shiny dashboard for the Epicenter 2023-2024 after-school and summer program dataset described in the analysis plan for IRB Protocol 7585.

## Project Structure

```text
marina-analysis-suite/
  app/                  Shiny dashboard
  data/raw/             Local raw data files; gitignored by default
  data/processed/       Cleaned analysis files; gitignored by default
  docs/                 Analysis-plan notes and cleaning log
  outputs/figures/      Generated plots
  outputs/manuscript/   Generated private Word manuscript-support files
  outputs/tables/       Generated CSV tables
  R/                    Reusable functions and metadata
  scripts/              Numbered analysis pipeline scripts
```

## Raw Data

Place the Excel workbook here before running:

```text
data/raw/Data for Marina.xlsx
```

The raw workbook is intentionally ignored by git because it appears to contain child participant data. If the team decides to archive data on OSF/GitHub, confirm the IRB data-sharing plan and de-identification requirements first.

## Install Packages

Run once from the project root:

```r
dir.create("r-lib", showWarnings = FALSE)
.libPaths(c(normalizePath("r-lib"), .libPaths()))

install.packages(c(
  "readxl", "writexl", "tidyverse", "janitor", "rstatix", "coin",
  "broom", "gt", "shiny", "shinydashboard", "plotly", "DT"
))
```

## Run the Analysis

Run scripts in order:

```r
source("scripts/00_setup.R")
source("scripts/01_import_clean.R")
source("scripts/02_descriptives.R")
source("scripts/03_primary_baseline.R")
source("scripts/04_prepost_change.R")
source("scripts/05_exploratory_age_type_verification.R")
source("scripts/06_sensitivity_ae_unknown.R")
source("scripts/07_key_differences.R")
source("scripts/08_observations.R")
source("scripts/09_ae_relationship_atlas.R")
source("scripts/10_research_questions.R")
source("scripts/11_composite_scores.R")
source("scripts/12_manuscript_support.R")
```

Or run everything:

```r
source("scripts/run_all.R")
```

On this Windows machine, R was found at:

```powershell
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' scripts/run_all.R
```

Outputs are written to `data/processed/`, `outputs/tables/`, `outputs/figures/`, and `outputs/manuscript/`.

The full runner also rebuilds the two Word manuscript-support documents when it can find Python with `python-docx`. It checks the Codex bundled runtime automatically. On another machine, install `python-docx` and optionally point `EPICENTER_PYTHON` to that Python executable.

## Launch the Dashboard

After running `01_import_clean.R` at minimum:

```r
shiny::runApp("app")
```

The app displays cleaning flags, sample characteristics, item distributions, primary baseline comparisons, direction-explicit AE change contrasts, a research-question page aligned to the original analysis plan with manuscript-ready exports, a 145-observation AE Relationship Atlas with expandable calculation details, an Overall Signal synthesis, AE type/verification summaries, sensitivity-analysis outputs, and manuscript support when available.

The Manuscript Support page provides:

- A dynamic Methods and Results outline covering the composite, item changes, PRE/POST/trajectory AE comparisons, age results, AE-classification sensitivity, and the three original research questions.
- A separate calculation audit with formulas, runnable R checks, expected values, and independent reconciliation against the main pipeline.
- Downloadable Word versions of both documents and CSV/Excel export controls for the underlying aggregate tables.
- A fail-fast audit: manuscript generation stops if a reconstructed value disagrees with the pipeline outside the stated tolerance.

The Research Questions page provides:

- Direct, data-dependent answers to each research question in the analysis plan.
- The four planned figures, the requested PRE Item 5/6 distributions, and two effect-summary figures, all generated with ggplot2.
- Per-figure 300-dpi PNG and vector PDF downloads.
- Copy, CSV, and Excel controls for every displayed results table.
- A single Excel workbook containing all manuscript tables.
- Prioritized recommendations for strengthening the analysis plan.

## Build the Static GitHub Pages Snapshot

The local Shiny app is still the most powerful private dashboard. For a public/static snapshot, build the sanitized site:

```powershell
& "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" scripts/run_all.R
& "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" scripts/build_static_dashboard.R
```

This writes a static site to:

```text
docs/index.html
```

The static builder publishes aggregate tables and aggregate charts only. It intentionally excludes raw data, processed participant-level CSVs, row-level cleaning flags, participant IDs, names, raw AE text, verification free text, and the original analysis-plan Word document.

To publish on GitHub Pages:

1. Push the repo to GitHub.
2. In GitHub, open `Settings -> Pages`.
3. Choose `Deploy from a branch`.
4. Select the repo branch and `/docs` as the publishing folder.

## Verification Notes

The pipeline has been run successfully with R 4.6.0. Generated outputs are gitignored by default because they include participant-level derived data.

Current run checks:

- Cleaned participant rows: 186.
- AE status counts: yes = 54, no = 87, unknown = 45.
- Missing participant IDs flagged exactly as planned: 59, 60, 115, 160.
- Shiny dashboard smoke test returned HTTP 200 on localhost.
- Manuscript audit checks: 119 PASS, 0 REVIEW, 0 FAIL.

## Human Review Required

Some cleaning decisions require project-team confirmation rather than automated resolution. See `docs/cleaning_log.md`, especially:

- Tentatively resolved age entries remain open for source confirmation: `8.5` is floored to `8`, while grade entries use expected midpoint ages (`1st grade = 6.5`, `2nd grade = 7.5`, `4th grade = 9.5`). These provisional values feed all age-based calculations.
- Whether skipped row numbers represent withdrawn participants or data-entry gaps.
- Final coding of `?` anomalous-experience records.
- AE type and verification method categorization for ambiguous free text.
