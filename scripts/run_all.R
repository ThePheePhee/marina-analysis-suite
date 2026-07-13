# Convenience runner for the full analysis pipeline.

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

python_candidates <- unique(c(
  Sys.getenv("EPICENTER_PYTHON", unset = NA_character_),
  file.path(
    Sys.getenv("USERPROFILE", unset = ""), ".cache", "codex-runtimes",
    "codex-primary-runtime", "dependencies", "python", "python.exe"
  ),
  Sys.which(c("python", "python3"))
))
python_candidates <- python_candidates[!is.na(python_candidates) & nzchar(python_candidates) & file.exists(python_candidates)]

docx_python <- NULL
for (candidate in python_candidates) {
  module_check <- suppressWarnings(system2(
    candidate,
    c("-c", shQuote("import docx")),
    stdout = FALSE,
    stderr = FALSE
  ))
  if (identical(module_check, 0L)) {
    docx_python <- candidate
    break
  }
}

if (!is.null(docx_python)) {
  document_status <- system2(docx_python, "scripts/build_manuscript_documents.py")
  if (!identical(document_status, 0L)) {
    warning("The statistical pipeline completed, but the Word manuscript documents could not be rebuilt.", call. = FALSE)
  }
} else {
  warning(
    "The statistical pipeline completed, but Word documents were not rebuilt because Python with python-docx was not found. Set EPICENTER_PYTHON to a suitable Python executable.",
    call. = FALSE
  )
}

message("Full analysis pipeline and manuscript support complete.")
