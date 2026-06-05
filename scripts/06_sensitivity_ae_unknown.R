# Sensitivity analyses for the substantial AE-unknown group.
# Scenario 1: unknown -> no
# Scenario 2: unknown -> yes

source("scripts/00_setup.R")

participants <- read_processed("participant_analysis.csv")
item_long <- read_processed("item_long.csv")

make_sensitivity_status <- function(ae_status, scenario) {
  dplyr::case_when(
    ae_status == "yes" ~ "yes",
    ae_status == "no" ~ "no",
    ae_status == "unknown" & scenario == "unknown_as_no" ~ "no",
    ae_status == "unknown" & scenario == "unknown_as_yes" ~ "yes",
    TRUE ~ NA_character_
  )
}

run_baseline_sensitivity <- function(scenario) {
  status <- participants |>
    dplyr::transmute(
      participant_id,
      ae_status_sensitivity = factor(make_sensitivity_status(ae_status, scenario), levels = c("yes", "no"))
    )

  item_long |>
    dplyr::filter(timepoint == "pre", item <= 9) |>
    dplyr::left_join(status, by = "participant_id") |>
    dplyr::group_by(item, short_name, label, direction) |>
    dplyr::group_modify(~ mann_whitney_summary(.x, score, ae_status_sensitivity)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      scenario = scenario,
      p_fdr = stats::p.adjust(p_value, method = "BH")
    )
}

run_change_sensitivity <- function(scenario) {
  status <- participants |>
    dplyr::transmute(
      participant_id,
      ae_status_sensitivity = factor(make_sensitivity_status(ae_status, scenario), levels = c("yes", "no"))
    )

  item_long |>
    dplyr::filter(item <= 9) |>
    dplyr::mutate(score_improvement_direction = reverse_if_needed(score, item)) |>
    dplyr::select(participant_id, item, short_name, label, direction, timepoint, score_improvement_direction) |>
    tidyr::pivot_wider(names_from = timepoint, values_from = score_improvement_direction) |>
    dplyr::mutate(change = post - pre) |>
    dplyr::left_join(status, by = "participant_id") |>
    dplyr::group_by(item, short_name, label, direction) |>
    dplyr::group_modify(~ mann_whitney_summary(.x, change, ae_status_sensitivity)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      scenario = scenario,
      p_fdr = stats::p.adjust(p_value, method = "BH")
    )
}

baseline_sensitivity <- dplyr::bind_rows(
  run_baseline_sensitivity("unknown_as_no"),
  run_baseline_sensitivity("unknown_as_yes")
)

change_sensitivity <- dplyr::bind_rows(
  run_change_sensitivity("unknown_as_no"),
  run_change_sensitivity("unknown_as_yes")
)

write_csv_safe(baseline_sensitivity, "outputs/tables/sensitivity_baseline_pre_items_unknown_recode.csv")
write_csv_safe(change_sensitivity, "outputs/tables/sensitivity_prepost_change_unknown_recode.csv")

message("AE-unknown sensitivity analyses written to outputs/tables/.")
