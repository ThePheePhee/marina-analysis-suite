# Project setup. Run this before the numbered analysis scripts.

source("R/utils.R")
use_project_library()
load_required_packages()
ensure_dirs()

source("R/metadata.R")
source("R/cleaning_functions.R")
source("R/stat_functions.R")

raw_workbook_path <- file.path("data", "raw", "Data for Marina.xlsx")

if (!file.exists(raw_workbook_path)) {
  stop(
    "Raw workbook not found at ", raw_workbook_path, ".\n",
    "Copy 'Data for Marina.xlsx' into data/raw/ before running the analysis.",
    call. = FALSE
  )
}

message("Setup complete. Raw workbook found: ", raw_workbook_path)
