# Secondary analyses: pre-post change in the full sample and by AE status.

source("scripts/00_setup.R")

participants <- read_processed("participant_analysis.csv") |>
  dplyr::mutate(ae_status_primary = factor(ae_status_primary, levels = c("yes", "no")))

item_long <- read_processed("item_long.csv")

change_scores <- item_long |>
  dplyr::filter(item <= 9) |>
  dplyr::mutate(score_improvement_direction = reverse_if_needed(score, item)) |>
  dplyr::select(participant_id, item, short_name, label, direction, timepoint, score_improvement_direction) |>
  tidyr::pivot_wider(names_from = timepoint, values_from = score_improvement_direction) |>
  dplyr::mutate(change = post - pre) |>
  dplyr::left_join(participants |> dplyr::select(participant_id, ae_status_primary), by = "participant_id")

prepost_full_sample_tests <- change_scores |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ wilcoxon_change_summary(.x, change)) |>
  dplyr::ungroup() |>
  dplyr::mutate(p_fdr = stats::p.adjust(p_value, method = "BH"))

prepost_change_by_ae_tests <- change_scores |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ mann_whitney_summary(.x, change, ae_status_primary)) |>
  dplyr::ungroup() |>
  dplyr::mutate(p_fdr = stats::p.adjust(p_value, method = "BH"))

change_descriptives_by_ae <- change_scores |>
  dplyr::filter(ae_status_primary %in% c("yes", "no")) |>
  dplyr::group_by(item, short_name, label, ae_status_primary) |>
  dplyr::summarise(
    n_complete_pairs = sum(!is.na(change)),
    median_iqr_change = median_iqr(change),
    .groups = "drop"
  )

p_spaghetti_item6 <- item_long |>
  dplyr::filter(item == 6) |>
  dplyr::mutate(score_improvement_direction = reverse_if_needed(score, item)) |>
  dplyr::left_join(participants |> dplyr::select(participant_id, ae_status_primary), by = "participant_id") |>
  dplyr::filter(ae_status_primary %in% c("yes", "no"), !is.na(score_improvement_direction)) |>
  ggplot2::ggplot(ggplot2::aes(x = timepoint, y = score_improvement_direction, group = participant_id)) +
  ggplot2::geom_line(alpha = 0.25) +
  ggplot2::geom_point(alpha = 0.55) +
  ggplot2::facet_wrap(~ ae_status_primary) +
  ggplot2::labs(
    x = NULL,
    y = "Item 6, reversed so higher = less anxiety/fear",
    title = "PRE to POST change in Item 6 by AE group"
  ) +
  ggplot2::theme_minimal(base_size = 12)

write_csv_safe(change_scores, "data/processed/change_scores.csv")
write_csv_safe(prepost_full_sample_tests, "outputs/tables/table_3_prepost_full_sample_tests.csv")
write_csv_safe(prepost_change_by_ae_tests, "outputs/tables/table_4_prepost_change_by_ae_tests.csv")
write_csv_safe(change_descriptives_by_ae, "outputs/tables/prepost_change_descriptives_by_ae.csv")
save_plot_safe(p_spaghetti_item6, "outputs/figures/figure_3_prepost_item_6_spaghetti_by_ae.png", width = 8, height = 5)

message("Pre-post change analyses written to outputs/.")
