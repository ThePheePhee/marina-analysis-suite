# Exploratory joint analysis of all nine paired questionnaire items.
# Items 5 and 6 are reversed so higher values consistently indicate a more
# favourable response. This is not presented as a validated psychometric scale.

source("scripts/00_setup.R")

minimum_items <- 7L

participants <- read_processed("participant_analysis.csv") |>
  dplyr::mutate(
    ae_status = factor(ae_status, levels = ae_levels),
    ae_status_primary = factor(ae_status_primary, levels = c("yes", "no"))
  )

item_long <- read_processed("item_long.csv") |>
  dplyr::filter(item <= 9) |>
  dplyr::mutate(score_favourable = reverse_if_needed(score, item))

composite_by_timepoint <- item_long |>
  dplyr::group_by(participant_id, timepoint) |>
  dplyr::summarise(
    n_items = sum(!is.na(score_favourable)),
    composite = if (n_items >= minimum_items) mean(score_favourable, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = timepoint,
    values_from = c(composite, n_items),
    names_glue = "{.value}_{timepoint}"
  )

paired_composite <- item_long |>
  dplyr::select(participant_id, item, timepoint, score_favourable) |>
  tidyr::pivot_wider(names_from = timepoint, values_from = score_favourable) |>
  dplyr::mutate(item_change = post - pre, complete_pair = !is.na(pre) & !is.na(post)) |>
  dplyr::group_by(participant_id) |>
  dplyr::summarise(
    n_paired_items = sum(complete_pair),
    composite_paired_pre = if (n_paired_items >= minimum_items) mean(pre[complete_pair]) else NA_real_,
    composite_paired_post = if (n_paired_items >= minimum_items) mean(post[complete_pair]) else NA_real_,
    composite_change = if (n_paired_items >= minimum_items) mean(item_change[complete_pair]) else NA_real_,
    .groups = "drop"
  )

composite_scores <- participants |>
  dplyr::select(participant_id, age, age_group, ae_status, ae_status_primary) |>
  dplyr::left_join(composite_by_timepoint, by = "participant_id") |>
  dplyr::left_join(paired_composite, by = "participant_id")

composite_ae_tests <- dplyr::bind_rows(
  mann_whitney_summary(composite_scores, composite_pre, ae_status_primary) |>
    dplyr::mutate(analysis = "Baseline joint composite", order = 1L),
  mann_whitney_summary(composite_scores, composite_change, ae_status_primary) |>
    dplyr::mutate(analysis = "PRE-to-POST joint composite change", order = 2L)
) |>
  dplyr::arrange(order) |>
  dplyr::mutate(
    p_fdr = stats::p.adjust(p_value, method = "BH"),
    effect_magnitude = dplyr::case_when(
      is.na(rank_biserial_r) ~ NA_character_,
      abs(rank_biserial_r) < 0.10 ~ "negligible",
      abs(rank_biserial_r) < 0.30 ~ "small",
      abs(rank_biserial_r) < 0.50 ~ "medium",
      TRUE ~ "large"
    )
  ) |>
  dplyr::select(-order)

composite_full_change <- wilcoxon_change_summary(composite_scores, composite_change) |>
  dplyr::mutate(analysis = "Full-sample PRE-to-POST joint composite change", .before = 1)

spearman_summary <- function(data, outcome, analysis, scope) {
  outcome <- rlang::enquo(outcome)
  dat <- data |>
    dplyr::transmute(age, outcome = !!outcome) |>
    dplyr::filter(!is.na(age), !is.na(outcome))

  test <- if (nrow(dat) >= 3) {
    suppressWarnings(stats::cor.test(dat$age, dat$outcome, method = "spearman", exact = FALSE))
  } else {
    NULL
  }

  tibble::tibble(
    analysis = analysis,
    group = scope,
    n = nrow(dat),
    spearman_rho = if (is.null(test)) NA_real_ else unname(test$estimate),
    p_value = if (is.null(test)) NA_real_ else test$p.value
  )
}

composite_age_correlations <- dplyr::bind_rows(
  spearman_summary(composite_scores, composite_pre, "Age vs baseline joint composite", "All participants"),
  spearman_summary(composite_scores, composite_change, "Age vs joint composite change", "All participants"),
  spearman_summary(dplyr::filter(composite_scores, ae_status_primary == "yes"), composite_pre, "Age vs baseline joint composite", "AE yes"),
  spearman_summary(dplyr::filter(composite_scores, ae_status_primary == "no"), composite_pre, "Age vs baseline joint composite", "AE no"),
  spearman_summary(dplyr::filter(composite_scores, ae_status_primary == "yes"), composite_change, "Age vs joint composite change", "AE yes"),
  spearman_summary(dplyr::filter(composite_scores, ae_status_primary == "no"), composite_change, "Age vs joint composite change", "AE no")
) |>
  dplyr::mutate(p_fdr = stats::p.adjust(p_value, method = "BH"))

tidy_model_terms <- function(model, analysis, terms) {
  broom::tidy(model, conf.int = TRUE) |>
    dplyr::filter(term %in% names(terms)) |>
    dplyr::mutate(
      analysis = analysis,
      term_label = unname(terms[term]),
      n = stats::nobs(model),
      adjusted_r_squared = summary(model)$adj.r.squared
    ) |>
    dplyr::select(analysis, term = term_label, estimate, conf.low, conf.high, std.error, statistic, p.value, n, adjusted_r_squared)
}

baseline_model_data <- composite_scores |>
  dplyr::filter(ae_status_primary %in% c("yes", "no")) |>
  dplyr::mutate(ae_yes = as.integer(ae_status_primary == "yes"))

baseline_model <- stats::lm(composite_pre ~ age + ae_yes, data = baseline_model_data)
change_model <- stats::lm(composite_change ~ composite_paired_pre + age + ae_yes, data = baseline_model_data)
interaction_model <- stats::lm(composite_change ~ composite_paired_pre + age * ae_yes, data = baseline_model_data)

composite_models <- dplyr::bind_rows(
  tidy_model_terms(
    baseline_model,
    "Baseline composite adjusted model",
    c(age = "Age (per year)", ae_yes = "AE yes vs AE no")
  ),
  tidy_model_terms(
    change_model,
    "Change model adjusted for paired baseline composite",
    c(age = "Age (per year)", ae_yes = "AE yes vs AE no")
  ),
  tidy_model_terms(
    interaction_model,
    "Exploratory age-by-AE interaction model",
    c(`age:ae_yes` = "Age x AE-yes interaction")
  )
) |>
  dplyr::group_by(analysis) |>
  dplyr::mutate(p_fdr = stats::p.adjust(p.value, method = "BH")) |>
  dplyr::ungroup()

cronbach_alpha_complete <- function(data, timepoint_value) {
  wide <- data |>
    dplyr::filter(timepoint == timepoint_value) |>
    dplyr::select(participant_id, item, score_favourable) |>
    tidyr::pivot_wider(names_from = item, values_from = score_favourable) |>
    dplyr::select(-participant_id) |>
    tidyr::drop_na()

  k <- ncol(wide)
  alpha <- if (nrow(wide) >= 3 && k >= 2) {
    item_variances <- vapply(wide, stats::var, numeric(1))
    total_variance <- stats::var(rowSums(wide))
    if (is.na(total_variance) || total_variance == 0) NA_real_ else k / (k - 1) * (1 - sum(item_variances) / total_variance)
  } else {
    NA_real_
  }

  tibble::tibble(
    timepoint = toupper(timepoint_value),
    n_complete_all_9 = nrow(wide),
    n_items = k,
    cronbach_alpha = alpha
  )
}

composite_reliability <- dplyr::bind_rows(
  cronbach_alpha_complete(item_long, "pre"),
  cronbach_alpha_complete(item_long, "post")
)

composite_descriptives <- composite_scores |>
  dplyr::filter(ae_status_primary %in% c("yes", "no")) |>
  dplyr::group_by(ae_status_primary) |>
  dplyr::summarise(
    n_baseline = sum(!is.na(composite_pre)),
    baseline_median_iqr = median_iqr(composite_pre),
    n_paired = sum(!is.na(composite_change)),
    paired_pre_median_iqr = median_iqr(composite_paired_pre),
    paired_post_median_iqr = median_iqr(composite_paired_post),
    change_median_iqr = median_iqr(composite_change),
    .groups = "drop"
  )

composite_definition <- tibble::tibble(
  component = c("Included items", "Direction", "Score range", "Minimum completeness", "Change definition", "Interpretation status"),
  specification = c(
    "All nine paired PRE/POST questionnaire items",
    "Items 5 and 6 reverse-coded as 11 - response; all other items unchanged",
    "Mean from 1 to 10; higher means a more favourable joint response",
    "At least 7 of 9 non-missing items at a timepoint",
    "Mean POST minus PRE using the same items for each participant; at least 7 complete item pairs",
    "Exploratory index, not a validated unidimensional psychometric scale"
  )
)

p_composite_baseline <- composite_scores |>
  dplyr::filter(ae_status_primary %in% c("yes", "no"), !is.na(composite_pre)) |>
  ggplot2::ggplot(ggplot2::aes(ae_status_primary, composite_pre, fill = ae_status_primary)) +
  ggplot2::geom_violin(trim = FALSE, alpha = 0.45, color = NA) +
  ggplot2::geom_boxplot(width = 0.16, outlier.shape = NA, alpha = 0.85) +
  ggplot2::geom_jitter(width = 0.10, alpha = 0.45, size = 1.5) +
  ggplot2::scale_fill_manual(values = c(yes = "#2878a8", no = "#7a8793")) +
  ggplot2::labs(x = "AE group", y = "Baseline joint composite (1-10)", title = "Baseline joint composite by AE group") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "none")

p_composite_change <- composite_scores |>
  dplyr::filter(ae_status_primary %in% c("yes", "no"), !is.na(composite_change)) |>
  ggplot2::ggplot(ggplot2::aes(ae_status_primary, composite_change, fill = ae_status_primary)) +
  ggplot2::geom_hline(yintercept = 0, color = "#475569", linetype = 2) +
  ggplot2::geom_violin(trim = FALSE, alpha = 0.45, color = NA) +
  ggplot2::geom_boxplot(width = 0.16, outlier.shape = NA, alpha = 0.85) +
  ggplot2::geom_jitter(width = 0.10, alpha = 0.45, size = 1.5) +
  ggplot2::scale_fill_manual(values = c(yes = "#2878a8", no = "#7a8793")) +
  ggplot2::labs(x = "AE group", y = "Joint composite change", title = "PRE-to-POST joint composite change by AE group") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "none")

p_composite_age_baseline <- composite_scores |>
  dplyr::filter(!is.na(age), !is.na(composite_pre)) |>
  ggplot2::ggplot(ggplot2::aes(age, composite_pre, color = ae_status)) +
  ggplot2::geom_point(alpha = 0.65) +
  ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "#263238") +
  ggplot2::scale_color_manual(values = c(yes = "#2878a8", no = "#7a8793", unknown = "#d28b28")) +
  ggplot2::labs(x = "Age (years)", y = "Baseline joint composite", color = "AE status", title = "Age and baseline joint composite") +
  ggplot2::theme_minimal(base_size = 12)

p_composite_age_change <- composite_scores |>
  dplyr::filter(!is.na(age), !is.na(composite_change)) |>
  ggplot2::ggplot(ggplot2::aes(age, composite_change, color = ae_status)) +
  ggplot2::geom_hline(yintercept = 0, color = "#475569", linetype = 2) +
  ggplot2::geom_point(alpha = 0.65) +
  ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "#263238") +
  ggplot2::scale_color_manual(values = c(yes = "#2878a8", no = "#7a8793", unknown = "#d28b28")) +
  ggplot2::labs(x = "Age (years)", y = "Joint composite change", color = "AE status", title = "Age and joint composite change") +
  ggplot2::theme_minimal(base_size = 12)

write_csv_safe(composite_scores, "data/processed/composite_scores_joint.csv")
write_csv_safe(composite_definition, "outputs/tables/composite_score_definition.csv")
write_csv_safe(composite_descriptives, "outputs/tables/composite_score_descriptives.csv")
write_csv_safe(composite_ae_tests, "outputs/tables/composite_score_ae_tests.csv")
write_csv_safe(composite_full_change, "outputs/tables/composite_score_full_change.csv")
write_csv_safe(composite_age_correlations, "outputs/tables/composite_score_age_correlations.csv")
write_csv_safe(composite_models, "outputs/tables/composite_score_models.csv")
write_csv_safe(composite_reliability, "outputs/tables/composite_score_reliability.csv")
save_plot_safe(p_composite_baseline, "outputs/figures/composite_baseline_by_ae.png", width = 7, height = 5)
save_plot_safe(p_composite_change, "outputs/figures/composite_change_by_ae.png", width = 7, height = 5)
save_plot_safe(p_composite_age_baseline, "outputs/figures/composite_age_baseline.png", width = 8, height = 5)
save_plot_safe(p_composite_age_change, "outputs/figures/composite_age_change.png", width = 8, height = 5)

message("Exploratory joint composite analyses written to outputs/.")
