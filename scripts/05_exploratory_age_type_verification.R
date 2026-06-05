# Exploratory age, AE type, and verification analyses.

source("scripts/00_setup.R")

participants <- read_processed("participant_analysis.csv") |>
  dplyr::mutate(
    ae_status_primary = factor(ae_status_primary, levels = c("yes", "no")),
    age_group = factor(age_group, levels = age_group_levels)
  )

age_correlations <- tibble::tibble(
  outcome = c("PRE Item 6 anxiety/fear", "PRE wellbeing composite"),
  x = list(participants$pre_item6, participants$wellbeing_composite_pre)
) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    n = sum(!is.na(participants$age) & !is.na(x)),
    rho = if (n >= 3) stats::cor(participants$age, x, method = "spearman", use = "complete.obs") else NA_real_,
    p_value = if (n >= 3) stats::cor.test(participants$age, x, method = "spearman", exact = FALSE)$p.value else NA_real_
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-x) |>
  dplyr::mutate(p_bonferroni = p.adjust(p_value, method = "bonferroni"))

age_prevalence_test <- choose_age_prevalence_test(participants)

ae_type_descriptives <- participants |>
  dplyr::filter(ae_status == "yes") |>
  dplyr::select(participant_id, pre_item6, distress_subscale_pre, dplyr::all_of(ae_type_columns)) |>
  tidyr::pivot_longer(dplyr::all_of(ae_type_columns), names_to = "ae_type", values_to = "present") |>
  dplyr::filter(present) |>
  dplyr::group_by(ae_type) |>
  dplyr::summarise(
    n = dplyr::n(),
    pre_item6_median_iqr = median_iqr(pre_item6),
    distress_pre_median_iqr = median_iqr(distress_subscale_pre),
    inferential_ok_by_plan = n >= 10,
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(n), ae_type)

verification_descriptives <- participants |>
  dplyr::filter(ae_status == "yes") |>
  dplyr::group_by(verification_method) |>
  dplyr::summarise(
    n = dplyr::n(),
    pre_item6_median_iqr = median_iqr(pre_item6),
    distress_pre_median_iqr = median_iqr(distress_subscale_pre),
    inferential_ok_by_plan = n >= 10,
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(n), verification_method)

p_ae_type <- participants |>
  dplyr::filter(ae_status == "yes") |>
  dplyr::select(participant_id, verification_method, dplyr::all_of(ae_type_columns)) |>
  tidyr::pivot_longer(dplyr::all_of(ae_type_columns), names_to = "ae_type", values_to = "present") |>
  dplyr::filter(present) |>
  dplyr::count(ae_type, verification_method, name = "n") |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(ae_type, n), y = n, fill = verification_method)) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "AE type", y = "Participant count", title = "AE type frequencies by verification method") +
  ggplot2::theme_minimal(base_size = 12)

write_csv_safe(age_correlations, "outputs/tables/age_correlations.csv")
write_csv_safe(age_prevalence_test, "outputs/tables/ae_prevalence_by_age_group_test.csv")
write_csv_safe(ae_type_descriptives, "outputs/tables/ae_type_anxiety_descriptives.csv")
write_csv_safe(verification_descriptives, "outputs/tables/verification_anxiety_descriptives.csv")
save_plot_safe(p_ae_type, "outputs/figures/figure_4_ae_type_frequencies_by_verification.png", width = 9, height = 6)

message("Exploratory age, AE type, and verification analyses written to outputs/.")
