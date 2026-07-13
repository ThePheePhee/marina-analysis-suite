# Generate manuscript-ready summaries and an independent calculation audit.

source("scripts/00_setup.R")

dir.create("outputs/manuscript", recursive = TRUE, showWarnings = FALSE)

participants <- read_processed("participant_analysis.csv") |>
  dplyr::mutate(
    ae_status = factor(ae_status, levels = ae_levels),
    ae_status_primary = factor(ae_status_primary, levels = c("yes", "no"))
  )
item_long <- read_processed("item_long.csv") |> dplyr::filter(item <= 9)
pipeline_pre <- readr::read_csv("outputs/tables/table_2_pre_item_tests_by_ae.csv", show_col_types = FALSE)
pipeline_change <- readr::read_csv("outputs/tables/table_3_prepost_full_sample_tests.csv", show_col_types = FALSE)
pipeline_change_ae <- readr::read_csv("outputs/tables/table_4_prepost_change_by_ae_tests.csv", show_col_types = FALSE)
pipeline_post_ancova <- readr::read_csv("outputs/tables/ae_atlas_post_ancova.csv", show_col_types = FALSE)
pipeline_age_change <- readr::read_csv("outputs/tables/observation_age_change_effects.csv", show_col_types = FALSE)
pipeline_ae_age <- readr::read_csv("outputs/tables/observation_ae_age_prevalence.csv", show_col_types = FALSE)
pipeline_composite <- readr::read_csv("outputs/tables/composite_score_ae_tests.csv", show_col_types = FALSE)
pipeline_composite_change <- readr::read_csv("outputs/tables/composite_score_full_change.csv", show_col_types = FALSE)
pipeline_composite_age <- readr::read_csv("outputs/tables/composite_score_age_correlations.csv", show_col_types = FALSE)
pipeline_composite_models <- readr::read_csv("outputs/tables/composite_score_models.csv", show_col_types = FALSE)
pipeline_reliability <- readr::read_csv("outputs/tables/composite_score_reliability.csv", show_col_types = FALSE)
sensitivity_baseline <- readr::read_csv("outputs/tables/sensitivity_baseline_pre_items_unknown_recode.csv", show_col_types = FALSE)
sensitivity_change <- readr::read_csv("outputs/tables/sensitivity_prepost_change_unknown_recode.csv", show_col_types = FALSE)
age_group_test <- readr::read_csv("outputs/tables/ae_prevalence_by_age_group_test.csv", show_col_types = FALSE)

fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits)
fmt_p <- function(x) {
  ifelse(is.na(x), "not estimable", ifelse(x < 0.001, format(x, scientific = TRUE, digits = 2), formatC(x, format = "f", digits = 3)))
}

rank_biserial_independent_manual <- function(x, group) {
  keep <- !is.na(x) & !is.na(group) & group %in% c("yes", "no")
  x <- x[keep]
  group <- factor(group[keep], levels = c("yes", "no"))
  n_yes <- sum(group == "yes")
  n_no <- sum(group == "no")
  u_yes <- sum(rank(x, ties.method = "average")[group == "yes"]) - n_yes * (n_yes + 1) / 2
  2 * u_yes / (n_yes * n_no) - 1
}

rank_biserial_paired_manual <- function(change) {
  change <- change[!is.na(change) & change != 0]
  ranks <- rank(abs(change), ties.method = "average")
  w_plus <- sum(ranks[change > 0])
  w_minus <- sum(ranks[change < 0])
  (w_plus - w_minus) / (w_plus + w_minus)
}

mann_whitney_manual <- function(value, group) {
  keep <- !is.na(value) & !is.na(group) & group %in% c("yes", "no")
  value <- value[keep]
  group <- factor(group[keep], levels = c("yes", "no"))
  n_yes <- sum(group == "yes")
  n_no <- sum(group == "no")

  if (n_yes == 0 || n_no == 0) {
    return(tibble::tibble(
      n_yes = n_yes, n_no = n_no,
      median_iqr_yes = median_iqr(value[group == "yes"]),
      median_iqr_no = median_iqr(value[group == "no"]),
      statistic = NA_real_, p_value = NA_real_, rank_biserial_r = NA_real_,
      test_note = "Insufficient data for AE yes/no comparison"
    ))
  }

  wt <- stats::wilcox.test(value ~ group, exact = FALSE, correct = TRUE)
  tibble::tibble(
    n_yes = n_yes,
    n_no = n_no,
    median_iqr_yes = median_iqr(value[group == "yes"]),
    median_iqr_no = median_iqr(value[group == "no"]),
    statistic = unname(wt$statistic),
    p_value = wt$p.value,
    rank_biserial_r = rank_biserial_independent_manual(value, group),
    test_note = "Independently reconstructed Wilcoxon rank-sum/Mann-Whitney test"
  )
}

item_pairs <- item_long |>
  dplyr::select(participant_id, item, short_name, label, direction, timepoint, score) |>
  tidyr::pivot_wider(names_from = timepoint, values_from = score) |>
  dplyr::filter(!is.na(pre), !is.na(post)) |>
  dplyr::mutate(
    pre_favourable = dplyr::if_else(direction == "higher_worse", 11 - pre, pre),
    post_favourable = dplyr::if_else(direction == "higher_worse", 11 - post, post),
    change_favourable = post_favourable - pre_favourable
  )

item_change_recomputed <- item_pairs |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ {
    wt <- stats::wilcox.test(.x$change_favourable, mu = 0, exact = FALSE, correct = TRUE)
    tibble::tibble(
      n_pairs = nrow(.x),
      pre_mean_raw = mean(.x$pre),
      pre_sd_raw = stats::sd(.x$pre),
      post_mean_raw = mean(.x$post),
      post_sd_raw = stats::sd(.x$post),
      mean_change_favourable = mean(.x$change_favourable),
      sd_change_favourable = stats::sd(.x$change_favourable),
      median_iqr_change = median_iqr(.x$change_favourable),
      n_improved = sum(.x$change_favourable > 0),
      n_unchanged = sum(.x$change_favourable == 0),
      n_worsened = sum(.x$change_favourable < 0),
      statistic = unname(wt$statistic),
      p_value = wt$p.value,
      rank_biserial_r = rank_biserial_paired_manual(.x$change_favourable)
    )
  }) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    p_fdr = stats::p.adjust(p_value, method = "BH"),
    conclusion = dplyr::case_when(
      p_fdr < 0.05 & rank_biserial_r > 0 ~ "FDR-supported improvement",
      p_fdr < 0.05 & rank_biserial_r < 0 ~ "FDR-supported worsening",
      rank_biserial_r > 0 ~ "Direction favours improvement; not FDR-supported",
      rank_biserial_r < 0 ~ "Direction favours worsening; not FDR-supported",
      TRUE ~ "No directional change"
    )
  )

pre_items <- item_long |>
  dplyr::filter(timepoint == "pre") |>
  dplyr::left_join(participants |> dplyr::select(participant_id, ae_status_primary), by = "participant_id")

pre_ae_recomputed <- pre_items |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ mann_whitney_manual(.x$score, .x$ae_status_primary)) |>
  dplyr::ungroup() |>
  dplyr::mutate(p_fdr = stats::p.adjust(p_value, method = "BH"))

post_items <- item_long |>
  dplyr::filter(timepoint == "post") |>
  dplyr::left_join(participants |> dplyr::select(participant_id, ae_status_primary), by = "participant_id")

post_ae_tests <- post_items |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ mann_whitney_manual(.x$score, .x$ae_status_primary)) |>
  dplyr::ungroup() |>
  dplyr::mutate(p_fdr = stats::p.adjust(p_value, method = "BH"))

change_with_ae <- item_pairs |>
  dplyr::left_join(participants |> dplyr::select(participant_id, ae_status_primary), by = "participant_id")

change_ae_recomputed <- change_with_ae |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ mann_whitney_manual(.x$change_favourable, .x$ae_status_primary)) |>
  dplyr::ungroup() |>
  dplyr::mutate(p_fdr = stats::p.adjust(p_value, method = "BH"))

post_ancova_recomputed <- item_pairs |>
  dplyr::left_join(participants |> dplyr::select(participant_id, age, ae_status_primary), by = "participant_id") |>
  dplyr::filter(ae_status_primary %in% c("yes", "no"), !is.na(age)) |>
  dplyr::mutate(ae_yes = as.integer(ae_status_primary == "yes")) |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ {
    fit <- stats::lm(post_favourable ~ pre_favourable + age + ae_yes, data = .x)
    td <- broom::tidy(fit, conf.int = TRUE) |> dplyr::filter(term == "ae_yes")
    tibble::tibble(
      n = stats::nobs(fit), estimate = td$estimate, conf_low = td$conf.low,
      conf_high = td$conf.high, statistic = td$statistic, p_value = td$p.value
    )
  }) |>
  dplyr::ungroup() |>
  dplyr::mutate(p_fdr = stats::p.adjust(p_value, method = "BH"))

composite_recomputed <- item_long |>
  dplyr::mutate(score_favourable = dplyr::if_else(item %in% c(5L, 6L), 11 - score, score)) |>
  dplyr::select(participant_id, item, timepoint, score_favourable) |>
  tidyr::pivot_wider(names_from = timepoint, values_from = score_favourable) |>
  dplyr::mutate(complete_pair = !is.na(pre) & !is.na(post)) |>
  dplyr::group_by(participant_id) |>
  dplyr::summarise(
    n_paired_items = sum(complete_pair),
    composite_pre = if (n_paired_items >= 7) mean(pre[complete_pair]) else NA_real_,
    composite_post = if (n_paired_items >= 7) mean(post[complete_pair]) else NA_real_,
    composite_change = if (n_paired_items >= 7) mean(post[complete_pair] - pre[complete_pair]) else NA_real_,
    .groups = "drop"
  ) |>
  dplyr::left_join(participants |> dplyr::select(participant_id, age, ae_status, ae_status_primary), by = "participant_id")

composite_timepoint_recomputed <- item_long |>
  dplyr::mutate(score_favourable = dplyr::if_else(item %in% c(5L, 6L), 11 - score, score)) |>
  dplyr::group_by(participant_id, timepoint) |>
  dplyr::summarise(
    n_items = sum(!is.na(score_favourable)),
    composite = if (n_items >= 7) mean(score_favourable, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = timepoint,
    values_from = c(composite, n_items),
    names_glue = "{.value}_{timepoint}"
  )

composite_all_recomputed <- participants |>
  dplyr::select(participant_id, age, ae_status, ae_status_primary) |>
  dplyr::left_join(composite_timepoint_recomputed, by = "participant_id") |>
  dplyr::left_join(
    composite_recomputed |>
      dplyr::select(participant_id, composite_paired_pre = composite_pre, composite_paired_post = composite_post, composite_change),
    by = "participant_id"
  )

composite_complete <- composite_recomputed |> dplyr::filter(!is.na(composite_change))
composite_wilcox <- stats::wilcox.test(composite_complete$composite_change, mu = 0, exact = FALSE, correct = TRUE)
composite_t <- stats::t.test(composite_complete$composite_change, mu = 0)
composite_summary <- composite_complete |>
  dplyr::summarise(
    n = dplyr::n(),
    pre_mean = mean(composite_pre), pre_sd = stats::sd(composite_pre),
    post_mean = mean(composite_post), post_sd = stats::sd(composite_post),
    mean_change = mean(composite_change), change_sd = stats::sd(composite_change),
    mean_change_ci_low = unname(composite_t$conf.int[1]),
    mean_change_ci_high = unname(composite_t$conf.int[2]),
    median_iqr_change = median_iqr(composite_change),
    wilcoxon_statistic = unname(composite_wilcox$statistic),
    wilcoxon_p = composite_wilcox$p.value,
    rank_biserial_r = rank_biserial_paired_manual(composite_change),
    paired_t_statistic = unname(composite_t$statistic),
    paired_t_df = unname(composite_t$parameter),
    paired_t_p = composite_t$p.value
  )

age_baseline_cor <- stats::cor.test(
  composite_all_recomputed$age, composite_all_recomputed$composite_pre,
  method = "spearman", exact = FALSE
)
age_change_cor <- stats::cor.test(
  composite_all_recomputed$age, composite_all_recomputed$composite_change,
  method = "spearman", exact = FALSE
)

composite_ae_recomputed <- dplyr::bind_rows(
  mann_whitney_manual(composite_all_recomputed$composite_pre, composite_all_recomputed$ae_status_primary) |>
    dplyr::mutate(analysis = "Baseline joint composite", .before = 1),
  mann_whitney_manual(composite_all_recomputed$composite_change, composite_all_recomputed$ae_status_primary) |>
    dplyr::mutate(analysis = "PRE-to-POST joint composite change", .before = 1)
) |>
  dplyr::mutate(p_fdr = stats::p.adjust(p_value, method = "BH"))

composite_model_data <- composite_all_recomputed |>
  dplyr::filter(ae_status_primary %in% c("yes", "no")) |>
  dplyr::mutate(ae_yes = as.integer(ae_status_primary == "yes"))
composite_change_model <- stats::lm(
  composite_change ~ composite_paired_pre + age + ae_yes,
  data = composite_model_data
)
composite_change_terms <- broom::tidy(composite_change_model) |>
  dplyr::filter(term %in% c("age", "ae_yes"))

cronbach_alpha_manual <- function(timepoint_value) {
  wide <- item_long |>
    dplyr::filter(timepoint == timepoint_value) |>
    dplyr::mutate(score_favourable = dplyr::if_else(item %in% c(5L, 6L), 11 - score, score)) |>
    dplyr::select(participant_id, item, score_favourable) |>
    tidyr::pivot_wider(names_from = item, values_from = score_favourable) |>
    dplyr::select(-participant_id) |>
    tidyr::drop_na()
  k <- ncol(wide)
  k / (k - 1) * (1 - sum(vapply(wide, stats::var, numeric(1))) / stats::var(rowSums(wide)))
}
alpha_pre <- cronbach_alpha_manual("pre")
alpha_post <- cronbach_alpha_manual("post")

age_change_data <- item_pairs |>
  dplyr::left_join(participants |> dplyr::select(participant_id, age, ae_status), by = "participant_id") |>
  dplyr::mutate(age_centered = age - mean(participants$age, na.rm = TRUE))

age_change_recomputed <- age_change_data |>
  dplyr::filter(!is.na(age_centered), !is.na(ae_status)) |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ {
    fit <- stats::lm(change_favourable ~ pre_favourable + age_centered + ae_status, data = .x)
    td <- broom::tidy(fit, conf.int = TRUE) |> dplyr::filter(term == "age_centered")
    tibble::tibble(
      n = stats::nobs(fit), estimate = td$estimate, conf_low = td$conf.low,
      conf_high = td$conf.high, statistic = td$statistic, p_value = td$p.value
    )
  }) |>
  dplyr::ungroup() |>
  dplyr::mutate(p_family_fdr = stats::p.adjust(p_value, method = "BH"))

ae_age_data <- participants |>
  dplyr::filter(ae_status_primary %in% c("yes", "no"), !is.na(age)) |>
  dplyr::mutate(ae_yes = as.integer(ae_status_primary == "yes"), age_centered = age - mean(participants$age, na.rm = TRUE))
ae_age_fit <- stats::glm(ae_yes ~ age_centered, data = ae_age_data, family = stats::binomial())
ae_age_term <- broom::tidy(ae_age_fit, conf.int = TRUE, exponentiate = TRUE) |> dplyr::filter(term == "age_centered")

check_rows <- list()
add_check <- function(family, result, metric, recomputed, pipeline, tolerance = 1e-10) {
  difference <- abs(recomputed - pipeline)
  check_rows[[length(check_rows) + 1]] <<- tibble::tibble(
    family = family, result = result, metric = metric,
    recomputed = recomputed, pipeline = pipeline,
    absolute_difference = difference,
    tolerance = tolerance,
    status = ifelse(is.na(difference), "REVIEW", ifelse(difference <= tolerance, "PASS", "FAIL"))
  )
}

for (i in seq_len(nrow(item_change_recomputed))) {
  item <- item_change_recomputed$item[i]
  expected <- pipeline_change |> dplyr::filter(item == !!item)
  add_check("Item PRE-to-POST", paste0("Item ", item), "p_value", item_change_recomputed$p_value[i], expected$p_value[1])
  add_check("Item PRE-to-POST", paste0("Item ", item), "p_fdr", item_change_recomputed$p_fdr[i], expected$p_fdr[1])
  add_check("Item PRE-to-POST", paste0("Item ", item), "rank_biserial_r", item_change_recomputed$rank_biserial_r[i], expected$rank_biserial_r[1])
}
for (i in seq_len(nrow(pre_ae_recomputed))) {
  item <- pre_ae_recomputed$item[i]
  expected <- pipeline_pre |> dplyr::filter(item == !!item)
  add_check("Baseline AE comparison", paste0("Item ", item), "p_value", pre_ae_recomputed$p_value[i], expected$p_value[1])
  add_check("Baseline AE comparison", paste0("Item ", item), "p_fdr", pre_ae_recomputed$p_fdr[i], expected$p_fdr[1])
}
for (i in seq_len(nrow(change_ae_recomputed))) {
  item <- change_ae_recomputed$item[i]
  expected <- pipeline_change_ae |> dplyr::filter(item == !!item)
  add_check("AE change comparison", paste0("Item ", item), "p_value", change_ae_recomputed$p_value[i], expected$p_value[1])
  add_check("AE change comparison", paste0("Item ", item), "p_fdr", change_ae_recomputed$p_fdr[i], expected$p_fdr[1])
}
for (i in seq_len(nrow(post_ancova_recomputed))) {
  item <- post_ancova_recomputed$item[i]
  expected <- pipeline_post_ancova |> dplyr::filter(item == !!item)
  add_check("Adjusted POST ANCOVA", paste0("Item ", item), "estimate", post_ancova_recomputed$estimate[i], expected$estimate[1])
  add_check("Adjusted POST ANCOVA", paste0("Item ", item), "p_value", post_ancova_recomputed$p_value[i], expected$p_value[1])
}
for (i in seq_len(nrow(age_change_recomputed))) {
  item <- age_change_recomputed$item[i]
  expected <- pipeline_age_change |> dplyr::filter(item == !!item)
  add_check("Adjusted age effect", paste0("Item ", item), "estimate", age_change_recomputed$estimate[i], expected$estimate[1])
  add_check("Adjusted age effect", paste0("Item ", item), "p_value", age_change_recomputed$p_value[i], expected$p_value[1])
}
add_check("Composite", "Full-sample change", "p_value", composite_wilcox$p.value, pipeline_composite_change$p_value[1])
add_check("Composite", "Full-sample change", "rank_biserial_r", rank_biserial_paired_manual(composite_complete$composite_change), pipeline_composite_change$rank_biserial_r[1])
for (analysis_name in composite_ae_recomputed$analysis) {
  recomputed <- composite_ae_recomputed |> dplyr::filter(analysis == analysis_name)
  expected <- pipeline_composite |> dplyr::filter(analysis == analysis_name)
  add_check("Composite AE comparison", analysis_name, "p_value", recomputed$p_value[1], expected$p_value[1])
  add_check("Composite AE comparison", analysis_name, "p_fdr", recomputed$p_fdr[1], expected$p_fdr[1])
  add_check("Composite AE comparison", analysis_name, "rank_biserial_r", recomputed$rank_biserial_r[1], expected$rank_biserial_r[1])
}
baseline_age_expected <- pipeline_composite_age |>
  dplyr::filter(analysis == "Age vs baseline joint composite", group == "All participants")
change_age_expected <- pipeline_composite_age |>
  dplyr::filter(analysis == "Age vs joint composite change", group == "All participants")
add_check("Composite age", "Baseline", "spearman_rho", unname(age_baseline_cor$estimate), baseline_age_expected$spearman_rho[1])
add_check("Composite age", "Baseline", "p_value", age_baseline_cor$p.value, baseline_age_expected$p_value[1])
add_check("Composite age", "Change", "spearman_rho", unname(age_change_cor$estimate), change_age_expected$spearman_rho[1])
add_check("Composite age", "Change", "p_value", age_change_cor$p.value, change_age_expected$p_value[1])
model_term_labels <- c(age = "Age (per year)", ae_yes = "AE yes vs AE no")
for (term_code in names(model_term_labels)) {
  term_name <- unname(model_term_labels[term_code])
  recomputed <- composite_change_terms |> dplyr::filter(term == term_code)
  expected <- pipeline_composite_models |>
    dplyr::filter(
      analysis == "Change model adjusted for paired baseline composite",
      term == term_name
    )
  add_check("Composite adjusted change", term_name, "estimate", recomputed$estimate[1], expected$estimate[1])
  add_check("Composite adjusted change", term_name, "p_value", recomputed$p.value[1], expected$p.value[1])
}
add_check("Composite reliability", "PRE", "cronbach_alpha", alpha_pre, pipeline_reliability$cronbach_alpha[pipeline_reliability$timepoint == "PRE"])
add_check("Composite reliability", "POST", "cronbach_alpha", alpha_post, pipeline_reliability$cronbach_alpha[pipeline_reliability$timepoint == "POST"])
add_check("Age and AE", "AE-yes odds per year", "odds_ratio", ae_age_term$estimate, pipeline_ae_age$estimate[1], tolerance = 1e-8)
add_check("Age and AE", "AE-yes odds per year", "p_value", ae_age_term$p.value, pipeline_ae_age$p_value[1], tolerance = 1e-8)

crosscheck <- dplyr::bind_rows(check_rows)
if (any(crosscheck$status == "FAIL")) {
  stop("Independent manuscript cross-check failed. Inspect outputs/tables/manuscript_crosscheck.csv.", call. = FALSE)
}

supported_items <- item_change_recomputed |> dplyr::filter(p_fdr < 0.05, rank_biserial_r > 0)
worsened_items <- item_change_recomputed |> dplyr::filter(p_fdr < 0.05, rank_biserial_r < 0)
unsupported_items <- item_change_recomputed |> dplyr::filter(is.na(p_fdr) | p_fdr >= 0.05)
pre_supported <- pre_ae_recomputed |> dplyr::filter(p_fdr < 0.05)
post_supported <- post_ae_tests |> dplyr::filter(p_fdr < 0.05)
post_ancova_supported <- post_ancova_recomputed |> dplyr::filter(p_fdr < 0.05)
change_ae_supported <- change_ae_recomputed |> dplyr::filter(p_fdr < 0.05)
age_supported <- age_change_recomputed |> dplyr::filter(p_family_fdr < 0.05)

item_list_text <- function(data) {
  if (nrow(data) == 0) return("none")
  paste(paste0("Item ", data$item), collapse = ", ")
}

unsupported_change_text <- if (nrow(unsupported_items) == 0) {
  "Every item showed an FDR-supported directional shift."
} else {
  paste0(item_list_text(unsupported_items), " did not show FDR-supported change.")
}
worsening_text <- if (nrow(worsened_items) == 0) {
  "No item showed an FDR-supported worsening."
} else {
  paste0(item_list_text(worsened_items), " showed FDR-supported worsening.")
}

primary_nominal <- pre_ae_recomputed |>
  dplyr::filter(p_value < 0.05, is.na(p_fdr) | p_fdr >= 0.05)
sensitivity_baseline_supported <- sensitivity_baseline |> dplyr::filter(p_fdr < 0.05)
sensitivity_change_supported <- sensitivity_change |> dplyr::filter(p_fdr < 0.05)
sensitivity_scenario_text <- function(data) {
  if (nrow(data) == 0) return("none")
  scenario_label <- dplyr::recode(
    data$scenario,
    unknown_as_no = "all unknown assigned AE-no",
    unknown_as_yes = "all unknown assigned AE-yes"
  )
  direction_label <- dplyr::if_else(data$rank_biserial_r < 0, "AE-yes lower", "AE-yes higher")
  paste0(data$label, " under ", scenario_label, " (", direction_label, "; q = ", fmt(data$p_fdr, 3), ")") |>
    paste(collapse = "; ")
}

sensitivity_narrative <- paste0(
  if (nrow(primary_nominal) == 0) {
    "With AE-unknown excluded, no baseline item had raw p < .05 without FDR support. "
  } else {
    paste0(
      "With AE-unknown excluded, ", paste(primary_nominal$label, collapse = "; "),
      " had raw p < .05 but did not survive FDR correction. "
    )
  },
  "Across the two extreme AE-unknown recodings, FDR-supported baseline results were: ",
  sensitivity_scenario_text(sensitivity_baseline_supported), ". FDR-supported change results were: ",
  sensitivity_scenario_text(sensitivity_change_supported),
  ". A result appearing under only one extreme assignment is classification-sensitive; persistence under both would be more robust. These scenarios are stress tests, not verified reclassifications."
)

reporting_position <- if (composite_summary$wilcoxon_p < 0.05) {
  "The strongest broad finding is observed improvement in the exploratory nine-item joint composite. It may be presented first in the Results, but it should not be described as a validated scale or as proof that the program caused the change. The design is an uncontrolled single-group PRE/POST study."
} else {
  "The exploratory nine-item joint composite does not show a statistically supported full-sample shift in the current data. Results should therefore lead with the prespecified item analyses and retain the uncontrolled single-group PRE/POST limitation."
}

count_statement <- function(n, none_text, some_text) {
  if (n == 0) none_text else paste0(n, some_text)
}

ae_comparison_narrative <- paste0(
  count_statement(nrow(pre_supported), "No PRE item survived", " PRE item(s) survived"),
  " the nine-test BH correction. ",
  count_statement(nrow(post_supported), "No raw POST item survived", " raw POST item(s) survived"),
  " its nine-test BH correction. ",
  count_statement(nrow(post_ancova_supported), "No baseline-adjusted POST AE coefficient survived", " baseline-adjusted POST AE coefficient(s) survived"),
  " correction. ",
  count_statement(nrow(change_ae_supported), "No improvement-coded change item differed", " improvement-coded change item(s) differed"),
  " between AE-yes and AE-no after correction. The joint composite had BH q = ",
  fmt_p(pipeline_composite$p_fdr[pipeline_composite$analysis == "Baseline joint composite"]),
  " at baseline and q = ",
  fmt_p(pipeline_composite$p_fdr[pipeline_composite$analysis == "PRE-to-POST joint composite change"]),
  " for change. These are absence-of-detected-difference results when q >= .05, not equivalence tests."
)

item6_baseline <- pre_ae_recomputed |> dplyr::filter(item == 6)
rq1_text <- if (nrow(item6_baseline) == 1 && !is.na(item6_baseline$p_fdr) && item6_baseline$p_fdr < 0.05) {
  "RQ1: The planned analysis detected an FDR-supported AE-group difference in baseline anxiety/fear."
} else {
  "RQ1: The planned analysis did not detect an FDR-supported AE-group difference in baseline anxiety/fear."
}
rq2_text <- paste0(
  "RQ2: ", nrow(supported_items), " of nine outcomes showed FDR-supported observed improvement and ",
  nrow(worsened_items), " showed FDR-supported worsening. The exploratory joint composite ",
  ifelse(composite_summary$wilcoxon_p < 0.05, "also improved", "did not show a supported shift"),
  ", but causal attribution to the program is not possible."
)
rq3_text <- if (nrow(change_ae_supported) == 0) {
  "RQ3: No item showed an FDR-supported difference in PRE-to-POST trajectory between AE-yes and AE-no."
} else {
  paste0("RQ3: ", nrow(change_ae_supported), " item(s) showed an FDR-supported difference in PRE-to-POST trajectory between AE-yes and AE-no.")
}

item_result_lines <- paste0(
  "Item ", supported_items$item, " (", supported_items$label, "): raw PRE M = ", fmt(supported_items$pre_mean_raw),
  " (SD = ", fmt(supported_items$pre_sd_raw), "), raw POST M = ", fmt(supported_items$post_mean_raw),
  " (SD = ", fmt(supported_items$post_sd_raw), "), favourable mean change = ", fmt(supported_items$mean_change_favourable),
  ", Wilcoxon p = ", fmt_p(supported_items$p_value), ", BH q = ", fmt_p(supported_items$p_fdr),
  ", paired rank-biserial r = ", fmt(supported_items$rank_biserial_r), "."
)

age_result_lines <- paste0(
  "Item ", age_supported$item, " (", age_supported$label, "): ", fmt(abs(age_supported$estimate)),
  " points less favourable adjusted change per year (95% CI ", fmt(age_supported$conf_low), " to ",
  fmt(age_supported$conf_high), "; raw p = ", fmt_p(age_supported$p_value), "; within-family BH q = ",
  fmt_p(age_supported$p_family_fdr), ")."
)

methods_sections <- tibble::tribble(
  ~document, ~section_order, ~level, ~kind, ~heading, ~body,
  "methods", 1, 1, "callout", "Reporting position", reporting_position,
  "methods", 2, 1, "prose", "Study design and sample", paste0("The analysis used questionnaire records from ", nrow(participants), " children. There were ", sum(participants$ae_status == "yes", na.rm = TRUE), " AE-yes records, ", sum(participants$ae_status == "no", na.rm = TRUE), " AE-no records, and ", sum(participants$ae_status == "unknown", na.rm = TRUE), " AE-unknown records. The AE-unknown group was excluded from primary yes/no comparisons but retained in sensitivity analyses. The label AE-no means that no AE had been recorded or identified; it must not be interpreted as confirmed absence of anomalous experience."),
  "methods", 3, 1, "prose", "Questionnaire outcomes and scoring", "Nine questionnaire items were available at PRE and POST on a 1-10 response scale. Items 1-4 and 7-9 were scored so that higher values were more favourable. Items 5 (pressure to act differently) and 6 (anxiety/fear) were reverse-scored for change and composite analyses using 11 - response, so that positive change consistently represented improvement. Item-specific tables retain the original wording and identify the direction of the raw scale.",
  "methods", 4, 1, "prose", "Exploratory joint composite", "For each timepoint, the joint composite was the arithmetic mean of the nine favourably oriented items when at least 7 items were observed. For PRE-to-POST change, each participant's PRE and POST means were calculated over the same set of complete item pairs, with at least 7 paired items required; composite change was POST minus PRE. Internal consistency was assessed with raw Cronbach alpha among participants complete on all nine items. Because the items cover several constructs, the composite was treated as exploratory rather than as a validated unidimensional scale.",
  "methods", 5, 1, "prose", "Item-level PRE-to-POST analyses", "For each item, analyses included only participants with non-missing PRE and POST responses for that item. Improvement-coded change was POST minus PRE. A two-sided Wilcoxon signed-rank test with the normal approximation and continuity correction tested whether the change distribution was centred on zero. Paired rank-biserial correlation quantified effect size. Benjamini-Hochberg false-discovery-rate correction was applied across the nine item tests.",
  "methods", 6, 1, "prose", "AE-group comparisons", "Primary AE analyses compared AE-yes with AE-no using two-sided Mann-Whitney tests with average ranks for ties and the asymptotic p-value. Separate nine-test BH corrections were applied to PRE scores, raw POST scores, and improvement-coded change scores. Adjusted POST models used favourably oriented POST score as the outcome and included the matching PRE score, age, and an AE-yes indicator. These observational comparisons do not identify an effect caused by AE status.",
  "methods", 7, 1, "prose", "Age analyses", "Spearman correlations described age associations with the baseline joint composite and raw joint-composite change. Exploratory linear models estimated item-level change as a function of the matching PRE score, centred age, and three-level AE status; BH correction was applied across the nine age coefficients. A separate logistic regression among classifiable AE-yes/AE-no records estimated the odds of AE-yes status per additional year of age. The age association with AE status may reflect reporting or ascertainment as well as any underlying age pattern.",
  "methods", 8, 1, "prose", "Missing data and uncertainty", "Analyses used available-case or complete-pair data; no missing questionnaire responses were imputed. POST missingness was higher than PRE missingness, so denominators are reported for every result. Ambiguous AE records remained unknown. Several age entries required documented provisional recoding and remain flagged for source confirmation.",
  "methods", 9, 1, "prose", "Composite result", paste0("Among ", composite_summary$n, " participants with at least seven matched item pairs, the favourably scored joint composite increased from M = ", fmt(composite_summary$pre_mean), " (SD = ", fmt(composite_summary$pre_sd), ") at PRE to M = ", fmt(composite_summary$post_mean), " (SD = ", fmt(composite_summary$post_sd), ") at POST. Mean change was ", fmt(composite_summary$mean_change), " points (95% CI ", fmt(composite_summary$mean_change_ci_low), " to ", fmt(composite_summary$mean_change_ci_high), "); median change was ", composite_summary$median_iqr_change, ". The Wilcoxon test gave V = ", fmt(composite_summary$wilcoxon_statistic, 1), ", p = ", fmt_p(composite_summary$wilcoxon_p), ", paired rank-biserial r = ", fmt(composite_summary$rank_biserial_r), ". A paired t-test sensitivity check gave t(", fmt(composite_summary$paired_t_df, 0), ") = ", fmt(composite_summary$paired_t_statistic), ", p = ", fmt_p(composite_summary$paired_t_p), ". Raw Cronbach alpha was ", fmt(alpha_pre), " at PRE and ", fmt(alpha_post), " at POST. The modest PRE reliability reinforces that this joint score should remain exploratory."),
  "methods", 10, 1, "bullets", "FDR-supported item improvements", paste(item_result_lines, collapse = "\n"),
  "methods", 11, 1, "prose", "Items without supported improvement or worsening", paste(unsupported_change_text, worsening_text, "A non-significant population shift does not mean no participant worsened: individual improved/unchanged/worsened counts are reported for every item in the audit table."),
  "methods", 12, 1, "prose", "PRE, POST, and change comparisons by AE status", ae_comparison_narrative,
  "methods", 13, 1, "prose", "AE classification sensitivity", sensitivity_narrative,
  "methods", 14, 1, "prose", "Age-related results", paste0("Age correlated negatively with the baseline joint composite (Spearman rho = ", fmt(unname(age_baseline_cor$estimate)), ", p = ", fmt_p(age_baseline_cor$p.value), "), whereas age was not associated with raw composite change (rho = ", fmt(unname(age_change_cor$estimate)), ", p = ", fmt_p(age_change_cor$p.value), "). In the baseline-adjusted composite change model, each additional year was associated with ", fmt(abs(pipeline_composite_models$estimate[pipeline_composite_models$analysis == "Change model adjusted for paired baseline composite" & pipeline_composite_models$term == "Age (per year)"])), " points less favourable change, p = ", fmt_p(pipeline_composite_models$p.value[pipeline_composite_models$analysis == "Change model adjusted for paired baseline composite" & pipeline_composite_models$term == "Age (per year)"]), ". The item-level adjusted age models supported age associations for ", nrow(age_supported), " outcomes within that nine-test family."),
  "methods", 15, 1, "bullets", "Adjusted item-level age findings", paste(age_result_lines, collapse = "\n"),
  "methods", 16, 1, "prose", "Age and AE reporting", paste0("Among the ", nrow(ae_age_data), " participants classifiable as AE-yes or AE-no with age data, each additional year was associated with ", fmt(ae_age_term$estimate), " times the odds of being classified AE-yes (95% CI ", fmt(ae_age_term$conf.low), " to ", fmt(ae_age_term$conf.high), "; p = ", fmt_p(ae_age_term$p.value), "). However, the categorical age-group Fisher test was not significant (p = ", fmt_p(age_group_test$p_value[1]), "). The continuous-age logistic result is exploratory and could reflect older children's greater ability or opportunity to report AE, differential researcher ascertainment, or a genuine age pattern; these explanations cannot be separated here."),
  "methods", 17, 1, "prose", "Responses to the original research questions", paste(rq1_text, rq2_text, rq3_text),
  "methods", 18, 1, "bullets", "Additional material worth reporting", "A participant-flow or missing-data table showing the denominator for each PRE/POST analysis.\nEffect sizes and confidence intervals alongside p-values, rather than significance alone.\nA clear distinction between prespecified item analyses and the exploratory joint composite and age models.\nDescriptive results for the five POST-only program-evaluation questions, kept separate because they have no PRE comparator.\nA limitation that no equivalence or non-inferiority test was conducted, so non-significant AE comparisons do not establish that the groups are the same.\nA statement that no untreated comparison group was available, preventing causal claims about program effectiveness."
)

audit_sections <- tibble::tribble(
  ~document, ~section_order, ~level, ~kind, ~heading, ~body,
  "audit", 1, 1, "callout", "Audit outcome", paste0("The independent reconstruction produced ", sum(crosscheck$status == "PASS"), " PASS checks, ", sum(crosscheck$status == "REVIEW"), " REVIEW checks, and ", sum(crosscheck$status == "FAIL"), " FAIL checks at the stated numerical tolerances. The audit recomputes key statistics from the processed participant/item files rather than copying the dashboard tables."),
  "audit", 2, 1, "prose", "1. Recreate the analysis dataset", "Start with the supplied workbook. Skip the first two descriptive header rows and retain participant rows from row 3 onward. Map participant number, age, nine PRE/POST item pairs, AE status, AE type, verification route, and valence exactly as documented in scripts/01_import_clean.R. Convert numeric-looking cells to numbers; treat blanks and '-' as missing. Flag rather than silently discard out-of-range values. Preserve names only in the private cleaned file and never in public output." ,
  "audit", 3, 1, "code", "2. Direction coding", "For Items 1-4 and 7-9: favourable_score = raw_score\nFor Items 5 and 6: favourable_score = 11 - raw_score\nFor each item: change = favourable_POST - favourable_PRE\nPositive change therefore always means improvement.",
  "audit", 4, 1, "prose", "3. Item-level paired test", "For each item separately, keep only rows with both PRE and POST. Remove zero changes, let n be the remaining count, rank absolute changes using average ranks for ties, and sum ranks for positive (W+) and negative (W-) changes. R reports V = W+. Under the null, E(V) = n(n+1)/4. Its normal-approximation variance is n(n+1)(2n+1)/24 minus sum(t^3-t)/48 across tie blocks of absolute change; R then applies a 0.5 continuity correction toward the null. The two-sided p-value is 2*Phi(-abs(z)). The paired rank-biserial effect is (W+ - W-) / (W+ + W-). Apply BH correction to the nine p-values as one family." ,
  "audit", 4, 1, "code", "Exact R check for each item", "x <- change_favourable[!is.na(change_favourable)]\nwt <- wilcox.test(x, mu = 0, exact = FALSE, correct = TRUE)\nranks <- rank(abs(x[x != 0]), ties.method = 'average')\nw_plus <- sum(ranks[x[x != 0] > 0])\nw_minus <- sum(ranks[x[x != 0] < 0])\nr_paired <- (w_plus - w_minus) / (w_plus + w_minus)\nq_values <- p.adjust(the_nine_item_p_values, method = 'BH')",
  "audit", 5, 1, "code", "4. Benjamini-Hochberg calculation", "Sort the m p-values from smallest to largest: p(1) ... p(m).\nFor rank i calculate p(i) * m / i.\nWorking from largest rank to smallest, replace each value by the minimum of itself and every value at a larger rank; cap at 1.\nReturn the adjusted values to the original item order. Here m = 9 for each item family." ,
  "audit", 6, 1, "prose", "5. Independent AE-group test", "For PRE, raw POST, and improvement-coded change separately, retain AE-yes and AE-no and exclude AE-unknown. Rank all N observations together with average ranks for ties. Let U_yes = rank-sum_yes - n_yes(n_yes+1)/2. Under the null, E(U) = n_yes*n_no/2. The tie-corrected variance is n_yes*n_no/12 * [(N+1) - sum(t^3-t)/(N(N-1))]. R uses a 0.5 continuity correction for the asymptotic two-sided p-value. The independent rank-biserial effect is 2*U_yes/(n_yes*n_no) - 1. Apply BH across the nine items separately for PRE, POST, and change." ,
  "audit", 6, 1, "code", "Exact R check for each AE contrast", "keep <- !is.na(value) & ae_status_primary %in% c('yes', 'no')\ngroup <- factor(ae_status_primary[keep], levels = c('yes', 'no'))\nx <- value[keep]\nwt <- wilcox.test(x ~ group, exact = FALSE, correct = TRUE)\nn_yes <- sum(group == 'yes'); n_no <- sum(group == 'no')\nu_yes <- sum(rank(x, ties.method = 'average')[group == 'yes']) - n_yes*(n_yes+1)/2\nr_independent <- 2*u_yes/(n_yes*n_no) - 1",
  "audit", 7, 1, "prose", "6. Joint composite", "Reverse Items 5 and 6, then average all available favourable scores only when at least 7 of 9 are present. For change, identify items having both PRE and POST for the participant; require at least 7 such pairs; average PRE over those items, average POST over those same items, and subtract. Re-run the signed-rank steps above on the participant-level composite changes. The paired t-test shown in the outline is a sensitivity calculation on the same change vector: t = mean(change)/(SD(change)/sqrt(n)), df = n-1, and the mean-change interval is mean(change) +/- t(.975, df)*SE. Cronbach alpha is k/(k-1) * [1 - sum(item variances)/variance(row total)] among complete nine-item records." ,
  "audit", 7, 1, "code", "Exact R check for the composite", "paired <- composite_change[!is.na(composite_change)]\nwilcox.test(paired, mu = 0, exact = FALSE, correct = TRUE)\nt.test(paired, mu = 0)\n# PRE/POST means reported in the paper use the same matched participants and item pairs.\nmean(composite_paired_pre, na.rm = TRUE)\nmean(composite_paired_post, na.rm = TRUE)",
  "audit", 8, 1, "prose", "7. Age calculations", "For Spearman correlation, replace age and outcome values by their average ranks and calculate Pearson correlation between the ranks using complete cases. The baseline correlation uses all participants with age and at least 7 PRE items; the change correlation uses participants with age and at least 7 matched item pairs. For adjusted item change, ordinary least squares estimates beta = (X'X)^(-1)X'y for change ~ matching PRE + centred age + AE status (no/yes/unknown); extract the age coefficient and BH-adjust the nine age p-values. For AE prevalence, exclude AE-unknown, code yes=1/no=0, maximize the binomial log-likelihood for logit[P(AE yes)] = intercept + beta*centred_age, and report exp(beta) with R's profile-likelihood 95% interval." ,
  "audit", 8, 1, "code", "Exact R checks for age", "cor.test(age, composite_pre, method = 'spearman', exact = FALSE)\ncor.test(age, composite_change, method = 'spearman', exact = FALSE)\nlm(change_favourable ~ pre_favourable + age_centered + ae_status, data = item_data)\nlm(composite_change ~ composite_paired_pre + age + ae_yes, data = classifiable_data)\nglm(ae_yes ~ age_centered, family = binomial(), data = classifiable_data)\nexp(cbind(OR = coef(logistic_fit), confint(logistic_fit)))",
  "audit", 9, 1, "prose", "8. Interpretation checks", "Do not translate a non-significant test into evidence that groups are equivalent. Do not describe observed PRE-to-POST change as a causal treatment effect. Keep raw and favourably oriented directions explicit for Items 5 and 6. Identify the composite and adjusted age models as exploratory. Treat the age-AE reporting explanation as one hypothesis among several." ,
  "audit", 10, 1, "prose", "9. Expected key values", paste0("Composite: n = ", composite_summary$n, ", PRE M = ", fmt(composite_summary$pre_mean), ", POST M = ", fmt(composite_summary$post_mean), ", mean change = ", fmt(composite_summary$mean_change), ", Wilcoxon p = ", fmt_p(composite_summary$wilcoxon_p), ", rank-biserial r = ", fmt(composite_summary$rank_biserial_r), ". Baseline composite age correlation: rho = ", fmt(unname(age_baseline_cor$estimate)), ". AE odds ratio per year: ", fmt(ae_age_term$estimate), ". ", nrow(supported_items), " improvement item q-values and ", nrow(worsened_items), " worsening item q-values are below .05; ", nrow(change_ae_supported), " AE trajectory q-values are below .05."),
  "audit", 11, 1, "prose", "10. Files to inspect", "The detailed expected values are in outputs/tables/manuscript_item_change_summary.csv, manuscript_post_ae_tests.csv, manuscript_post_ancova.csv, manuscript_age_change_models.csv, and manuscript_crosscheck.csv. The authoritative implementation is scripts/12_manuscript_support.R; upstream cleaning and analysis logic is in scripts/01_import_clean.R, scripts/03_primary_baseline.R, scripts/04_prepost_change.R, scripts/08_observations.R, and scripts/11_composite_scores.R."
)

document_sections <- dplyr::bind_rows(methods_sections, audit_sections)

write_csv_safe(item_change_recomputed, "outputs/tables/manuscript_item_change_summary.csv")
write_csv_safe(post_ae_tests, "outputs/tables/manuscript_post_ae_tests.csv")
write_csv_safe(post_ancova_recomputed, "outputs/tables/manuscript_post_ancova.csv")
write_csv_safe(age_change_recomputed, "outputs/tables/manuscript_age_change_models.csv")
write_csv_safe(composite_summary, "outputs/tables/manuscript_composite_summary.csv")
write_csv_safe(crosscheck, "outputs/tables/manuscript_crosscheck.csv")
write_csv_safe(document_sections, "outputs/tables/manuscript_document_sections.csv")

message("Manuscript outline, calculation audit, and independent cross-checks written to outputs/.")
