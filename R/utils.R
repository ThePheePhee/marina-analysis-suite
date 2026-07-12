# Small output and dependency helpers shared across scripts.

required_packages <- c(
  "readxl", "writexl", "tidyverse", "janitor", "rstatix", "coin",
  "broom", "gt", "ggplot2"
)

use_project_library <- function(path = "r-lib") {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(normalizePath(path), .libPaths()))
  invisible(.libPaths())
}

load_required_packages <- function(packages = required_packages) {
  use_project_library()

  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Missing required R packages: ",
      paste(missing, collapse = ", "),
      ". Install them with install.packages() before running the pipeline.",
      call. = FALSE
    )
  }

  invisible(lapply(packages, library, character.only = TRUE))
}

ensure_dirs <- function() {
  dirs <- c("data/raw", "data/processed", "outputs/tables", "outputs/figures")
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

write_csv_safe <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(data, path, na = "")
  invisible(path)
}

save_plot_safe <- function(plot, path, width = 8, height = 5, dpi = 300) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, plot = plot, width = width, height = height, dpi = dpi)
  invisible(path)
}

read_processed <- function(name) {
  readr::read_csv(file.path("data", "processed", name), show_col_types = FALSE)
}
