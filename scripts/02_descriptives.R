# Descriptive statistics for the full sample and AE groups.

source("scripts/00_setup.R")

participants <- read_processed("participant_analysis.csv") |>
  dplyr::mutate(
    ae_status = factor(ae_status, levels = ae_levels),
    ae_status_primary = factor(ae_status_primary, levels = c("yes", "no")),
    age_group = factor(age_group, levels = age_group_levels)
  )

item_long <- read_processed("item_long.csv") |>
  dplyr::mutate(timepoint = factor(timepoint, levels = c("pre", "post")))

sample_characteristics <- participants |>
  dplyr::group_by(ae_status) |>
  dplyr::summarise(
    n = dplyr::n(),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = stats::sd(age, na.rm = TRUE),
    age_min = min(age, na.rm = TRUE),
    age_max = max(age, na.rm = TRUE),
    age_uncertain_n = sum(age_uncertain_flag, na.rm = TRUE),
    age_tentatively_resolved_n = sum(age_tentatively_resolved_flag, na.rm = TRUE),
    .groups = "drop"
  )

age_group_counts <- participants |>
  dplyr::count(ae_status, age_group, name = "n") |>
  dplyr::group_by(ae_status) |>
  dplyr::mutate(pct = n / sum(n)) |>
  dplyr::ungroup()

ae_prevalence <- participants |>
  dplyr::count(ae_status, name = "n") |>
  dplyr::mutate(pct = n / sum(n))

ae_type_frequencies <- participants |>
  dplyr::filter(ae_status == "yes") |>
  dplyr::select(participant_id, verification_method, dplyr::all_of(ae_type_columns)) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(ae_type_columns),
    names_to = "ae_type",
    values_to = "present"
  ) |>
  dplyr::filter(present) |>
  dplyr::count(ae_type, verification_method, name = "n") |>
  dplyr::arrange(dplyr::desc(n), ae_type)

verification_frequencies <- participants |>
  dplyr::filter(ae_status == "yes") |>
  dplyr::count(verification_method, name = "n") |>
  dplyr::mutate(pct = n / sum(n))

item_descriptives_by_ae <- item_long |>
  dplyr::left_join(participants |> dplyr::select(participant_id, ae_status_primary), by = "participant_id") |>
  dplyr::filter(ae_status_primary %in% c("yes", "no"), item <= 9) |>
  dplyr::group_by(timepoint, item, short_name, label, ae_status_primary) |>
  dplyr::summarise(
    n = sum(!is.na(score)),
    mean = mean(score, na.rm = TRUE),
    sd = stats::sd(score, na.rm = TRUE),
    median_iqr = median_iqr(score),
    .groups = "drop"
  )

p_item5_6 <- item_long |>
  dplyr::left_join(participants |> dplyr::select(participant_id, ae_status_primary), by = "participant_id") |>
  dplyr::filter(timepoint == "pre", item %in% c(5, 6), ae_status_primary %in% c("yes", "no"), !is.na(score)) |>
  ggplot2::ggplot(ggplot2::aes(x = ae_status_primary, y = score, fill = ae_status_primary)) +
  ggplot2::geom_violin(trim = FALSE, alpha = 0.55, color = NA) +
  ggplot2::geom_jitter(width = 0.12, height = 0, alpha = 0.65, size = 1.8) +
  ggplot2::facet_wrap(~ label, scales = "free_y") +
  ggplot2::scale_fill_manual(values = c(yes = "#1f77b4", no = "#8c8c8c")) +
  ggplot2::labs(x = "AE group", y = "PRE score", title = "PRE Item 5 and 6 distributions by AE group") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "none")

write_csv_safe(sample_characteristics, "outputs/tables/table_1_sample_characteristics.csv")
write_csv_safe(age_group_counts, "outputs/tables/age_group_counts_by_ae.csv")
write_csv_safe(ae_prevalence, "outputs/tables/ae_prevalence.csv")
write_csv_safe(ae_type_frequencies, "outputs/tables/ae_type_frequencies.csv")
write_csv_safe(verification_frequencies, "outputs/tables/verification_method_frequencies.csv")
write_csv_safe(item_descriptives_by_ae, "outputs/tables/item_descriptives_by_ae.csv")
save_plot_safe(p_item5_6, "outputs/figures/pre_item_5_6_by_ae.png", width = 9, height = 5)

message("Descriptive tables and PRE Item 5/6 figure written to outputs/.")
