# Primary analysis: AE and anxiety/distress at baseline.

source("scripts/00_setup.R")

participants <- read_processed("participant_analysis.csv") |>
  dplyr::mutate(ae_status_primary = factor(ae_status_primary, levels = c("yes", "no")))

item_long <- read_processed("item_long.csv")

pre_items <- item_long |>
  dplyr::filter(timepoint == "pre", item <= 9) |>
  dplyr::left_join(participants |> dplyr::select(participant_id, ae_status_primary), by = "participant_id")

baseline_item_tests <- pre_items |>
  dplyr::group_by(item, short_name, label, direction, primary_focus) |>
  dplyr::group_modify(~ mann_whitney_summary(.x, score, ae_status_primary)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    p_fdr = stats::p.adjust(p_value, method = "BH"),
    effect_size_interpretation = dplyr::case_when(
      is.na(rank_biserial_r) ~ NA_character_,
      abs(rank_biserial_r) < 0.1 ~ "negligible",
      abs(rank_biserial_r) < 0.3 ~ "small",
      abs(rank_biserial_r) < 0.5 ~ "medium",
      TRUE ~ "large"
    )
  )

pre_composites <- participants |>
  dplyr::select(
    participant_id, ae_status_primary,
    wellbeing_composite_pre, distress_subscale_pre
  )

baseline_composite_tests <- dplyr::bind_rows(
  mann_whitney_summary(pre_composites, wellbeing_composite_pre, ae_status_primary) |>
    dplyr::mutate(outcome = "PRE wellbeing composite"),
  mann_whitney_summary(pre_composites, distress_subscale_pre, ae_status_primary) |>
    dplyr::mutate(outcome = "PRE distress subscale: raw Items 5 and 6")
) |>
  dplyr::select(outcome, dplyr::everything())

p_anxiety <- pre_items |>
  dplyr::filter(item == 6, ae_status_primary %in% c("yes", "no"), !is.na(score)) |>
  ggplot2::ggplot(ggplot2::aes(x = ae_status_primary, y = score, fill = ae_status_primary)) +
  ggplot2::geom_violin(trim = FALSE, alpha = 0.55, color = NA) +
  ggplot2::geom_boxplot(width = 0.16, outlier.shape = NA, alpha = 0.85) +
  ggplot2::geom_jitter(width = 0.10, height = 0, alpha = 0.65, size = 1.8) +
  ggplot2::scale_fill_manual(values = c(yes = "#1f77b4", no = "#8c8c8c")) +
  ggplot2::labs(x = "AE group", y = "PRE Item 6 score", title = "PRE anxiety/fear by AE group") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "none")

p_wellbeing <- participants |>
  dplyr::filter(ae_status_primary %in% c("yes", "no"), !is.na(wellbeing_composite_pre)) |>
  ggplot2::ggplot(ggplot2::aes(x = ae_status_primary, y = wellbeing_composite_pre, fill = ae_status_primary)) +
  ggplot2::geom_violin(trim = FALSE, alpha = 0.55, color = NA) +
  ggplot2::geom_boxplot(width = 0.16, outlier.shape = NA, alpha = 0.85) +
  ggplot2::geom_jitter(width = 0.10, height = 0, alpha = 0.65, size = 1.8) +
  ggplot2::scale_fill_manual(values = c(yes = "#2a9d8f", no = "#8c8c8c")) +
  ggplot2::labs(x = "AE group", y = "PRE wellbeing composite", title = "PRE composite wellbeing by AE group") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "none")

write_csv_safe(baseline_item_tests, "outputs/tables/table_2_pre_item_tests_by_ae.csv")
write_csv_safe(baseline_composite_tests, "outputs/tables/pre_composite_tests_by_ae.csv")
save_plot_safe(p_anxiety, "outputs/figures/figure_1_pre_item_6_anxiety_by_ae.png")
save_plot_safe(p_wellbeing, "outputs/figures/figure_2_pre_wellbeing_composite_by_ae.png")

message("Primary baseline tests and figures written to outputs/.")
