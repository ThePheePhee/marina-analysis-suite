# Broad exploratory scan for patterns that may merit follow-up.
#
# These models are intentionally labelled exploratory. They report raw p-values,
# within-family FDR, and a global FDR across the complete scan so that interesting
# patterns remain distinguishable from robust evidence.

source("scripts/00_setup.R")

participants <- read_processed("participant_analysis.csv") |>
  dplyr::mutate(
    ae_status = factor(ae_status, levels = c("no", "yes", "unknown")),
    ae_status_primary = factor(ae_status_primary, levels = c("no", "yes")),
    ae_yes_indicator = dplyr::if_else(ae_status_primary == "yes", 1, 0, missing = NA_real_),
    age_centered = age - mean(age, na.rm = TRUE)
  )

change_scores <- read_processed("change_scores.csv") |>
  dplyr::left_join(
    participants |>
      dplyr::select(
        participant_id, age, age_centered, age_group, ae_status, ae_status_primary,
        ae_yes_indicator, verification_method, dplyr::all_of(ae_type_columns)
      ),
    by = c("participant_id", "ae_status_primary")
  )

p_text <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "not estimated",
    p < 0.001 ~ "< .001",
    TRUE ~ paste0("= ", formatC(p, format = "f", digits = 3))
  )
}

extract_lm_term <- function(data, formula, term) {
  fit <- tryCatch(stats::lm(formula, data = data), error = function(e) NULL)
  if (is.null(fit) || stats::nobs(fit) < 15) {
    return(tibble::tibble(
      estimate = NA_real_, std_error = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
      statistic = NA_real_, p_value = NA_real_, partial_r = NA_real_, n = if (is.null(fit)) 0 else stats::nobs(fit)
    ))
  }

  coefs <- summary(fit)$coefficients
  if (!term %in% rownames(coefs)) {
    return(tibble::tibble(
      estimate = NA_real_, std_error = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
      statistic = NA_real_, p_value = NA_real_, partial_r = NA_real_, n = stats::nobs(fit)
    ))
  }

  ci <- tryCatch(stats::confint(fit, term), error = function(e) c(NA_real_, NA_real_))
  t_value <- unname(coefs[term, "t value"])
  df_residual <- stats::df.residual(fit)

  tibble::tibble(
    estimate = unname(coefs[term, "Estimate"]),
    std_error = unname(coefs[term, "Std. Error"]),
    conf_low = unname(ci[1]),
    conf_high = unname(ci[2]),
    statistic = t_value,
    p_value = unname(coefs[term, "Pr(>|t|)"]),
    partial_r = t_value / sqrt(t_value^2 + df_residual),
    n = stats::nobs(fit)
  )
}

extract_factor_test <- function(data, formula, term) {
  fit <- tryCatch(stats::lm(formula, data = data), error = function(e) NULL)
  if (is.null(fit) || stats::nobs(fit) < 20) {
    return(tibble::tibble(estimate = NA_real_, statistic = NA_real_, p_value = NA_real_, partial_r = NA_real_, n = 0))
  }

  test <- tryCatch(stats::drop1(fit, test = "F"), error = function(e) NULL)
  if (is.null(test) || !term %in% rownames(test)) {
    return(tibble::tibble(estimate = NA_real_, statistic = NA_real_, p_value = NA_real_, partial_r = NA_real_, n = stats::nobs(fit)))
  }

  f_value <- unname(test[term, "F value"])
  df_term <- unname(test[term, "Df"])
  df_residual <- stats::df.residual(fit)
  partial_eta_sq <- (f_value * df_term) / (f_value * df_term + df_residual)

  tibble::tibble(
    estimate = NA_real_,
    statistic = f_value,
    p_value = unname(test[term, "Pr(>F)"]),
    partial_r = sqrt(partial_eta_sq),
    n = stats::nobs(fit)
  )
}

add_family_fdr <- function(data) {
  data |>
    dplyr::mutate(p_family_fdr = stats::p.adjust(p_value, method = "BH"))
}

age_change_effects <- change_scores |>
  dplyr::filter(!is.na(change), !is.na(pre), !is.na(age_centered)) |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ extract_lm_term(.x, change ~ pre + age_centered + ae_status, "age_centered")) |>
  dplyr::ungroup() |>
  add_family_fdr() |>
  dplyr::mutate(
    analysis_family = "Age effect on observed change",
    field_analyzed = paste0("Change Item ", item, ": ", label),
    effect_name = "Per one-year increase in age",
    effect_scale = "Improvement-coded change points",
    controls = "Baseline answer and AE status (yes/no/unknown)",
    interpretation = dplyr::case_when(
      estimate > 0 ~ paste0("Older participants showed more positive observed PRE-to-POST change: ", round(estimate, 2), " points per year, adjusting for baseline and AE status."),
      estimate < 0 ~ paste0("Older participants showed less positive observed PRE-to-POST change: ", round(abs(estimate), 2), " points less per year, adjusting for baseline and AE status."),
      TRUE ~ "No age slope could be estimated."
    )
  )

age_ae_interactions <- change_scores |>
  dplyr::filter(!is.na(change), !is.na(pre), !is.na(age_centered), !is.na(ae_yes_indicator)) |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ extract_lm_term(.x, change ~ pre + age_centered * ae_yes_indicator, "age_centered:ae_yes_indicator")) |>
  dplyr::ungroup() |>
  add_family_fdr() |>
  dplyr::mutate(
    analysis_family = "AE-by-age interaction",
    field_analyzed = paste0("Change Item ", item, ": ", label),
    effect_name = "Difference in age slope: AE yes minus AE no",
    effect_scale = "Improvement-coded change points per year",
    controls = "Baseline answer, age, AE status, and age-by-AE interaction",
    interpretation = dplyr::case_when(
      estimate > 0 ~ paste0("The age gradient was more positive in AE-yes participants by ", round(estimate, 2), " change points per year."),
      estimate < 0 ~ paste0("The age gradient was less positive in AE-yes participants by ", round(abs(estimate), 2), " change points per year."),
      TRUE ~ "No AE-by-age interaction could be estimated."
    )
  )

baseline_ae_interactions <- change_scores |>
  dplyr::filter(!is.na(change), !is.na(pre), !is.na(age_centered), !is.na(ae_yes_indicator)) |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ extract_lm_term(.x, change ~ age_centered + pre * ae_yes_indicator, "pre:ae_yes_indicator")) |>
  dplyr::ungroup() |>
  add_family_fdr() |>
  dplyr::mutate(
    analysis_family = "AE-by-baseline interaction",
    field_analyzed = paste0("Change Item ", item, ": ", label),
    effect_name = "Difference in baseline-response slope: AE yes minus AE no",
    effect_scale = "Improvement-coded change points per baseline point",
    controls = "Age, baseline answer, AE status, and baseline-by-AE interaction",
    interpretation = dplyr::case_when(
      estimate > 0 ~ paste0("The relationship between starting score and later change was more positive in AE-yes participants by ", round(estimate, 2), " points. Because change contains the baseline score, this pattern is especially vulnerable to regression-to-the-mean and ceiling effects."),
      estimate < 0 ~ paste0("The relationship between starting score and later change was more negative in AE-yes participants by ", round(abs(estimate), 2), " points. Because change contains the baseline score, this pattern is especially vulnerable to regression-to-the-mean and ceiling effects."),
      TRUE ~ "No AE-by-baseline interaction could be estimated."
    )
  )

ae_type_counts <- participants |>
  dplyr::filter(ae_status == "yes") |>
  dplyr::summarise(dplyr::across(dplyr::all_of(ae_type_columns), ~ sum(.x, na.rm = TRUE))) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "ae_type", values_to = "n_present") |>
  dplyr::mutate(n_absent = sum(participants$ae_status == "yes", na.rm = TRUE) - n_present) |>
  dplyr::filter(n_present >= 5, n_absent >= 5)

ae_type_change_models <- purrr::map_dfr(ae_type_counts$ae_type, function(type_name) {
  purrr::map_dfr(sort(unique(change_scores$item)), function(item_number) {
    dat <- change_scores |>
      dplyr::filter(ae_status == "yes", item == item_number, !is.na(change), !is.na(pre), !is.na(age_centered)) |>
      dplyr::mutate(type_present = as.numeric(.data[[type_name]]))
    meta <- dat |> dplyr::slice_head(n = 1)
    n_present_complete <- sum(dat$type_present == 1, na.rm = TRUE)
    n_absent_complete <- sum(dat$type_present == 0, na.rm = TRUE)
    result <- if (n_present_complete < 5 || n_absent_complete < 5) {
      tibble::tibble(
        estimate = NA_real_, std_error = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
        statistic = NA_real_, p_value = NA_real_, partial_r = NA_real_, n = nrow(dat)
      )
    } else {
      extract_lm_term(dat, change ~ pre + age_centered + type_present, "type_present")
    }
    dplyr::bind_cols(
      tibble::tibble(
        item = item_number,
        short_name = dplyr::first(meta$short_name),
        label = dplyr::first(meta$label),
        direction = dplyr::first(meta$direction),
        ae_type = type_name,
        n_present = n_present_complete,
        n_absent = n_absent_complete
      ),
      result
    )
  })
}) |>
  add_family_fdr() |>
  dplyr::mutate(
    analysis_family = "AE-type difference within AE yes",
    field_analyzed = paste0("Change Item ", item, ": ", label, " | ", stringr::str_remove(ae_type, "^ae_")),
    effect_name = "AE type present minus absent",
    effect_scale = "Improvement-coded change points",
    controls = "Age and baseline answer; AE-yes participants only",
    interpretation = dplyr::case_when(
      estimate > 0 ~ paste0("Within AE yes, participants with this AE type showed ", round(estimate, 2), " points more positive adjusted change."),
      estimate < 0 ~ paste0("Within AE yes, participants with this AE type showed ", round(abs(estimate), 2), " points less positive adjusted change."),
      n_present < 5 | n_absent < 5 ~ "Descriptive only: fewer than five complete cases were available in one comparison group.",
      TRUE ~ "No AE-type coefficient could be estimated."
    )
  )

verification_levels <- participants |>
  dplyr::filter(ae_status == "yes", !is.na(verification_method)) |>
  dplyr::count(verification_method) |>
  dplyr::filter(n >= 10) |>
  dplyr::pull(verification_method)

verification_change_models <- change_scores |>
  dplyr::filter(
    ae_status == "yes", verification_method %in% verification_levels,
    !is.na(change), !is.na(pre), !is.na(age_centered)
  ) |>
  dplyr::mutate(verification_method = droplevels(factor(verification_method))) |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ extract_factor_test(.x, change ~ pre + age_centered + verification_method, "verification_method")) |>
  dplyr::ungroup() |>
  add_family_fdr() |>
  dplyr::mutate(
    analysis_family = "Verification-method difference within AE yes",
    field_analyzed = paste0("Change Item ", item, ": ", label),
    effect_name = "Omnibus verification-method difference",
    effect_scale = "Partial eta-squared square root",
    controls = "Age and baseline answer; AE-yes participants; methods with at least 10 participants",
    interpretation = "Tests whether adjusted observed change differs across conversation-only, artwork-only, and combined verification groups."
  )

standardize_scan <- function(data) {
  data |>
    dplyr::select(
      analysis_family, field_analyzed, effect_name, effect_scale, controls,
      estimate, conf_low = dplyr::any_of("conf_low"), conf_high = dplyr::any_of("conf_high"),
      statistic, partial_r, n, p_value, p_family_fdr, interpretation
    )
}

exploratory_scan <- dplyr::bind_rows(
  standardize_scan(age_change_effects),
  standardize_scan(age_ae_interactions),
  standardize_scan(baseline_ae_interactions),
  standardize_scan(ae_type_change_models),
  standardize_scan(verification_change_models)
) |>
  dplyr::mutate(
    p_global_fdr = stats::p.adjust(p_value, method = "BH"),
    evidence = dplyr::case_when(
      !is.na(p_global_fdr) & p_global_fdr < 0.05 ~ "FDR-supported across the complete exploratory scan",
      !is.na(p_family_fdr) & p_family_fdr < 0.05 ~ "FDR-supported within this model family only",
      !is.na(p_value) & p_value < 0.05 ~ "Nominal p < .05 only",
      TRUE ~ "No nominal signal"
    ),
    p_text = p_text(p_value),
    family_fdr_text = p_text(p_family_fdr),
    global_fdr_text = p_text(p_global_fdr)
  ) |>
  dplyr::arrange(p_value)

ae_age_data <- participants |>
  dplyr::filter(!is.na(ae_yes_indicator), !is.na(age_centered))

ae_age_prevalence <- stats::glm(
  ae_yes_indicator ~ age_centered,
  data = ae_age_data,
  family = stats::binomial()
) |>
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) |>
  dplyr::filter(term == "age_centered") |>
  dplyr::transmute(
    observation = "AE prevalence by age",
    n = sum(!is.na(participants$ae_yes_indicator) & !is.na(participants$age_centered)),
    estimate = estimate,
    conf_low = conf.low,
    conf_high = conf.high,
    p_value = p.value,
    interpretation = paste0(
      "Each additional year of age was associated with ", round(estimate, 2),
      " times the odds of AE-yes status (95% CI ", round(conf.low, 2), " to ", round(conf.high, 2), ")."
    )
  )

write_csv_safe(age_change_effects, "outputs/tables/observation_age_change_effects.csv")
write_csv_safe(age_ae_interactions, "outputs/tables/observation_age_ae_interactions.csv")
write_csv_safe(baseline_ae_interactions, "outputs/tables/observation_baseline_ae_interactions.csv")
write_csv_safe(ae_type_change_models, "outputs/tables/observation_ae_type_change_models.csv")
write_csv_safe(verification_change_models, "outputs/tables/observation_verification_change_models.csv")
write_csv_safe(exploratory_scan, "outputs/tables/observation_exploratory_scan.csv")
write_csv_safe(ae_age_prevalence, "outputs/tables/observation_ae_age_prevalence.csv")

message("Exploratory observation scan written to outputs/tables/.")
