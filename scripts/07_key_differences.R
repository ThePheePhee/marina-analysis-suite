# Summarise the largest observed AE vs non-AE differences.
#
# This script is interpretive: it combines the primary nonparametric results
# with exploratory adjusted models so the dashboard can explain what changed,
# what was controlled for, and how cautiously to read each observation.

source("scripts/00_setup.R")

participants <- read_processed("participant_analysis.csv") |>
  dplyr::mutate(
    ae_status_primary = factor(ae_status_primary, levels = c("yes", "no")),
    ae_yes_indicator = dplyr::if_else(ae_status_primary == "yes", 1, 0, missing = NA_real_)
  )

item_long <- read_processed("item_long.csv")
change_scores <- read_processed("change_scores.csv")

baseline_tests <- readr::read_csv("outputs/tables/table_2_pre_item_tests_by_ae.csv", show_col_types = FALSE)
composite_tests <- readr::read_csv("outputs/tables/pre_composite_tests_by_ae.csv", show_col_types = FALSE)
change_tests <- readr::read_csv("outputs/tables/table_4_prepost_change_by_ae_tests.csv", show_col_types = FALSE)

p_text <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "not estimated",
    p < 0.001 ~ "< .001",
    TRUE ~ paste0("= ", formatC(p, format = "f", digits = 3))
  )
}

effect_label <- function(r) {
  dplyr::case_when(
    is.na(r) ~ "not estimated",
    abs(r) < 0.1 ~ "negligible",
    abs(r) < 0.3 ~ "small",
    abs(r) < 0.5 ~ "medium",
    TRUE ~ "large"
  )
}

safe_lm_ae <- function(data, formula) {
  dat <- data |>
    dplyr::filter(!is.na(ae_yes_indicator))

  if (nrow(dat) < 10 || length(unique(dat$ae_yes_indicator)) < 2) {
    return(tibble::tibble(adjusted_estimate = NA_real_, adjusted_p = NA_real_, adjusted_n = nrow(dat)))
  }

  fit <- tryCatch(stats::lm(formula, data = dat), error = function(e) NULL)
  if (is.null(fit)) {
    return(tibble::tibble(adjusted_estimate = NA_real_, adjusted_p = NA_real_, adjusted_n = nrow(dat)))
  }

  coefs <- summary(fit)$coefficients
  if (!"ae_yes_indicator" %in% rownames(coefs)) {
    return(tibble::tibble(adjusted_estimate = NA_real_, adjusted_p = NA_real_, adjusted_n = nrow(dat)))
  }

  tibble::tibble(
    adjusted_estimate = unname(coefs["ae_yes_indicator", "Estimate"]),
    adjusted_p = unname(coefs["ae_yes_indicator", "Pr(>|t|)"]),
    adjusted_n = stats::nobs(fit)
  )
}

interpret_direction <- function(family, direction, median_difference) {
  if (is.na(median_difference)) {
    return("Difference could not be estimated.")
  }

  ae_higher <- median_difference > 0
  same <- median_difference == 0

  if (same) {
    return("Median scores were the same; any difference is distributional rather than a median shift.")
  }

  if (family == "Pre-post change") {
    return(if (ae_higher) {
      "AE-yes participants showed more improvement-coded change than AE-no participants."
    } else {
      "AE-yes participants showed less improvement-coded change than AE-no participants."
    })
  }

  if (direction == "higher_worse") {
    return(if (ae_higher) {
      "AE-yes participants scored higher on a negatively valenced item, suggesting more distress/pressure."
    } else {
      "AE-yes participants scored lower on a negatively valenced item, suggesting less distress/pressure."
    })
  }

  if (direction == "higher_better") {
    return(if (ae_higher) {
      "AE-yes participants scored higher on a positively valenced item, suggesting stronger wellbeing/confidence."
    } else {
      "AE-yes participants scored lower on a positively valenced item, suggesting weaker wellbeing/confidence."
    })
  }

  if (direction == "higher_distress") {
    return(if (ae_higher) {
      "AE-yes participants had higher distress-subscale scores."
    } else {
      "AE-yes participants had lower distress-subscale scores."
    })
  }

  if (direction == "higher_better_composite") {
    return(if (ae_higher) {
      "AE-yes participants had higher composite wellbeing."
    } else {
      "AE-yes participants had lower composite wellbeing."
    })
  }

  "Direction should be interpreted from the outcome label."
}

make_interpretation <- function(family, outcome, direction, median_difference, p_value, p_fdr, rank_biserial_r, adjusted_p, controls) {
  signal <- dplyr::case_when(
    !is.na(p_fdr) & p_fdr < 0.05 ~ "This is one of the clearest differences after FDR correction.",
    !is.na(p_value) & p_value < 0.05 ~ "This is an uncorrected signal; it does not necessarily survive multiple-comparison correction.",
    abs(rank_biserial_r) >= 0.3 ~ "The effect-size estimate is notable even though statistical evidence may be limited.",
    TRUE ~ "This is best treated as descriptive/exploratory rather than strong evidence."
  )

  adjusted <- if (is.na(adjusted_p)) {
    "No adjusted model was estimated."
  } else {
    paste0("Exploratory adjusted model (", controls, ") p ", p_text(adjusted_p), ".")
  }

  paste(
    interpret_direction(family, direction, median_difference),
    signal,
    adjusted,
    "Because AE status was identified informally and the sample is observational, interpret this as an association, not causation."
  )
}

baseline_long <- item_long |>
  dplyr::filter(timepoint == "pre", item <= 9) |>
  dplyr::left_join(
    participants |> dplyr::select(participant_id, ae_status_primary, ae_yes_indicator, age),
    by = "participant_id"
  )

baseline_descriptives <- baseline_long |>
  dplyr::filter(ae_status_primary %in% c("yes", "no")) |>
  dplyr::group_by(item, short_name, label, direction, ae_status_primary) |>
  dplyr::summarise(
    n = sum(!is.na(score)),
    median = stats::median(score, na.rm = TRUE),
    mean = mean(score, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = ae_status_primary,
    values_from = c(n, median, mean),
    names_glue = "{.value}_{ae_status_primary}"
  )

baseline_adjusted <- baseline_long |>
  dplyr::filter(ae_status_primary %in% c("yes", "no"), !is.na(score), !is.na(age)) |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ safe_lm_ae(.x, score ~ ae_yes_indicator + age)) |>
  dplyr::ungroup()

baseline_records <- baseline_tests |>
  dplyr::left_join(
    baseline_descriptives |> dplyr::select(item, median_yes, median_no, mean_yes, mean_no),
    by = "item"
  ) |>
  dplyr::left_join(baseline_adjusted |> dplyr::select(item, adjusted_estimate, adjusted_p, adjusted_n), by = "item") |>
  dplyr::mutate(
    family = "Baseline PRE item",
    outcome = label,
    field_analyzed = paste0("PRE Item ", item, ": ", label),
    controls = "Age only",
    median_difference = median_yes - median_no,
    mean_difference = mean_yes - mean_no,
    scale_note = dplyr::case_when(
      direction == "higher_worse" ~ "Higher score is worse/more distress.",
      TRUE ~ "Higher score is better/more wellbeing."
    )
  ) |>
  dplyr::select(
    family, field_analyzed, outcome, direction, scale_note, controls,
    n_yes, n_no, median_yes, median_no, median_difference, mean_difference,
    p_value, p_fdr, rank_biserial_r, adjusted_estimate, adjusted_p, adjusted_n
  )

composite_data <- participants |>
  dplyr::filter(ae_status_primary %in% c("yes", "no")) |>
  dplyr::select(participant_id, ae_status_primary, ae_yes_indicator, age, wellbeing_composite_pre, distress_subscale_pre) |>
  tidyr::pivot_longer(
    cols = c(wellbeing_composite_pre, distress_subscale_pre),
    names_to = "outcome_key",
    values_to = "score"
  ) |>
  dplyr::mutate(
    outcome = dplyr::case_when(
      outcome_key == "wellbeing_composite_pre" ~ "PRE wellbeing composite",
      outcome_key == "distress_subscale_pre" ~ "PRE distress subscale"
    ),
    direction = dplyr::case_when(
      outcome_key == "wellbeing_composite_pre" ~ "higher_better_composite",
      outcome_key == "distress_subscale_pre" ~ "higher_distress"
    )
  )

composite_descriptives <- composite_data |>
  dplyr::group_by(outcome_key, outcome, direction, ae_status_primary) |>
  dplyr::summarise(
    n = sum(!is.na(score)),
    median = stats::median(score, na.rm = TRUE),
    mean = mean(score, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = ae_status_primary,
    values_from = c(n, median, mean),
    names_glue = "{.value}_{ae_status_primary}"
  )

composite_adjusted <- composite_data |>
  dplyr::filter(!is.na(score), !is.na(age)) |>
  dplyr::group_by(outcome_key, outcome, direction) |>
  dplyr::group_modify(~ safe_lm_ae(.x, score ~ ae_yes_indicator + age)) |>
  dplyr::ungroup()

composite_records <- composite_tests |>
  dplyr::mutate(
    outcome_key = dplyr::case_when(
      outcome == "PRE wellbeing composite" ~ "wellbeing_composite_pre",
      TRUE ~ "distress_subscale_pre"
    )
  ) |>
  dplyr::left_join(
    composite_descriptives |> dplyr::select(outcome_key, direction, median_yes, median_no, mean_yes, mean_no),
    by = "outcome_key"
  ) |>
  dplyr::left_join(composite_adjusted |> dplyr::select(outcome_key, adjusted_estimate, adjusted_p, adjusted_n), by = "outcome_key") |>
  dplyr::mutate(
    family = "Baseline composite",
    field_analyzed = outcome,
    controls = "Age only",
    median_difference = median_yes - median_no,
    mean_difference = mean_yes - mean_no,
    p_fdr = NA_real_,
    scale_note = dplyr::case_when(
      outcome_key == "distress_subscale_pre" ~ "Higher score is more distress.",
      TRUE ~ "Higher score is better composite wellbeing."
    )
  ) |>
  dplyr::select(
    family, field_analyzed, outcome, direction, scale_note, controls,
    n_yes, n_no, median_yes, median_no, median_difference, mean_difference,
    p_value, p_fdr, rank_biserial_r, adjusted_estimate, adjusted_p, adjusted_n
  )

change_descriptives <- change_scores |>
  dplyr::filter(ae_status_primary %in% c("yes", "no")) |>
  dplyr::group_by(item, short_name, label, direction, ae_status_primary) |>
  dplyr::summarise(
    n = sum(!is.na(change)),
    median = stats::median(change, na.rm = TRUE),
    mean = mean(change, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = ae_status_primary,
    values_from = c(n, median, mean),
    names_glue = "{.value}_{ae_status_primary}"
  )

change_adjusted <- change_scores |>
  dplyr::left_join(
    participants |> dplyr::select(participant_id, age),
    by = "participant_id"
  ) |>
  dplyr::mutate(ae_yes_indicator = dplyr::if_else(ae_status_primary == "yes", 1, 0, missing = NA_real_)) |>
  dplyr::filter(ae_status_primary %in% c("yes", "no"), !is.na(change), !is.na(pre), !is.na(age)) |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ safe_lm_ae(.x, change ~ ae_yes_indicator + age + pre)) |>
  dplyr::ungroup()

change_records <- change_tests |>
  dplyr::left_join(
    change_descriptives |> dplyr::select(item, median_yes, median_no, mean_yes, mean_no),
    by = "item"
  ) |>
  dplyr::left_join(change_adjusted |> dplyr::select(item, adjusted_estimate, adjusted_p, adjusted_n), by = "item") |>
  dplyr::mutate(
    family = "Pre-post change",
    outcome = label,
    field_analyzed = paste0("Change Item ", item, ": ", label),
    controls = "Age and baseline score",
    median_difference = median_yes - median_no,
    mean_difference = mean_yes - mean_no,
    scale_note = "Change is scored so positive values indicate improvement."
  ) |>
  dplyr::select(
    family, field_analyzed, outcome, direction, scale_note, controls,
    n_yes, n_no, median_yes, median_no, median_difference, mean_difference,
    p_value, p_fdr, rank_biserial_r, adjusted_estimate, adjusted_p, adjusted_n
  )

key_differences <- dplyr::bind_rows(baseline_records, composite_records, change_records) |>
  dplyr::mutate(
    effect_size_label = effect_label(rank_biserial_r),
    absolute_effect_size = abs(rank_biserial_r),
    adjusted_p_text = p_text(adjusted_p),
    p_text = p_text(p_value),
    fdr_p_text = p_text(p_fdr),
    interpretation = purrr::pmap_chr(
      list(family, outcome, direction, median_difference, p_value, p_fdr, rank_biserial_r, adjusted_p, controls),
      make_interpretation
    )
  ) |>
  dplyr::arrange(dplyr::desc(absolute_effect_size), p_value)

fields_analyzed <- tibble::tibble(
  analysis_block = c(
    "Baseline PRE item differences",
    "Baseline composite differences",
    "Pre-post change differences",
    "Sensitivity analyses",
    "Not controlled because unavailable"
  ),
  fields_analyzed = c(
    "All 9 paired PRE Likert items, including anxiety/fear and social pressure.",
    "PRE wellbeing composite and PRE distress subscale.",
    "All 9 paired PRE-to-POST change scores, reverse-scored where needed so positive means improvement.",
    "AE unknown recoded once as AE no and once as AE yes.",
    "Sex, site/program cohort, grade/classroom, intervention dose, and standardized baseline mental-health diagnosis were not available in the workbook."
  ),
  comparison_or_model = c(
    "AE yes vs AE no; Mann-Whitney U plus exploratory linear model.",
    "AE yes vs AE no; Mann-Whitney U plus exploratory linear model.",
    "AE yes vs AE no on improvement-coded change; Mann-Whitney U plus exploratory linear model.",
    "Same item-wise comparisons under two alternate AE-unknown assumptions.",
    "No model includes these fields."
  ),
  controls = c(
    "Age in exploratory adjusted model.",
    "Age in exploratory adjusted model.",
    "Age and baseline score in exploratory adjusted model.",
    "No additional covariates; this tests AE-unknown coding assumptions.",
    "Not applicable."
  ),
  interpretation_guardrail = c(
    "Associational baseline differences only; AE status was not randomly assigned.",
    "Composite scores are exploratory and not a validated diagnostic scale.",
    "Pre-post differences cannot prove program causality without a control group.",
    "If conclusions change across sensitivity scenarios, emphasize uncertainty.",
    "Residual confounding is possible."
  )
)

write_csv_safe(key_differences, "outputs/tables/key_ae_differences_ranked.csv")
write_csv_safe(fields_analyzed, "outputs/tables/key_difference_fields_and_controls.csv")

message("Key AE difference summaries written to outputs/tables/.")
