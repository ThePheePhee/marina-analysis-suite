# Comprehensive atlas of associations between anomalous experiences (AE) and
# every analyzable field in the workbook.
#
# The dataset is observational and has no untreated comparison group. These
# analyses therefore describe associations and possible moderation of observed
# PRE-to-POST change; they cannot establish that AE experiences caused outcomes.

source("scripts/00_setup.R")

participants <- read_processed("participant_analysis.csv") |>
  dplyr::mutate(
    ae_status = factor(ae_status, levels = c("no", "yes", "unknown")),
    ae_status_primary = factor(ae_status_primary, levels = c("no", "yes")),
    ae_yes_indicator = dplyr::if_else(ae_status_primary == "yes", 1, 0, missing = NA_real_),
    age_centered = age - mean(age, na.rm = TRUE),
    valence_group = dplyr::case_when(
      stringr::str_detect(valence_raw, "positive") & !stringr::str_detect(valence_raw, "negative") ~ "positive",
      stringr::str_detect(valence_raw, "negative") ~ "negative",
      valence_raw == "neutral" ~ "neutral",
      valence_raw == "unsure" ~ "unsure",
      TRUE ~ NA_character_
    )
  )

change_scores <- read_processed("change_scores.csv") |>
  dplyr::left_join(
    participants |>
      dplyr::select(
        participant_id, age, age_centered, ae_status, ae_status_primary,
        ae_yes_indicator, valence_group, verification_method,
        dplyr::all_of(ae_type_columns)
      ),
    by = c("participant_id", "ae_status_primary")
  )

baseline_tests <- readr::read_csv("outputs/tables/table_2_pre_item_tests_by_ae.csv", show_col_types = FALSE)
change_tests <- readr::read_csv("outputs/tables/table_4_prepost_change_by_ae_tests.csv", show_col_types = FALSE)
full_prepost <- readr::read_csv("outputs/tables/table_3_prepost_full_sample_tests.csv", show_col_types = FALSE)
second_order <- readr::read_csv("outputs/tables/second_order_change_differences.csv", show_col_types = FALSE)
key_differences <- readr::read_csv("outputs/tables/key_ae_differences_ranked.csv", show_col_types = FALSE)
age_ae_interactions <- readr::read_csv("outputs/tables/observation_age_ae_interactions.csv", show_col_types = FALSE)
baseline_ae_interactions <- readr::read_csv("outputs/tables/observation_baseline_ae_interactions.csv", show_col_types = FALSE)
age_effects <- readr::read_csv("outputs/tables/observation_age_change_effects.csv", show_col_types = FALSE)
ae_age_prevalence <- readr::read_csv("outputs/tables/observation_ae_age_prevalence.csv", show_col_types = FALSE)
observation_scan <- readr::read_csv("outputs/tables/observation_exploratory_scan.csv", show_col_types = FALSE)
sensitivity_baseline <- readr::read_csv("outputs/tables/sensitivity_baseline_pre_items_unknown_recode.csv", show_col_types = FALSE)
sensitivity_change <- readr::read_csv("outputs/tables/sensitivity_prepost_change_unknown_recode.csv", show_col_types = FALSE)
completeness <- readr::read_csv("outputs/tables/pre_post_completeness.csv", show_col_types = FALSE)

p_display <- function(p) {
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
      estimate = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
      statistic = NA_real_, p_value = NA_real_, effect_size = NA_real_, n = 0
    ))
  }

  coefs <- summary(fit)$coefficients
  if (!term %in% rownames(coefs)) {
    return(tibble::tibble(
      estimate = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
      statistic = NA_real_, p_value = NA_real_, effect_size = NA_real_, n = stats::nobs(fit)
    ))
  }

  ci <- tryCatch(stats::confint(fit, term), error = function(e) c(NA_real_, NA_real_))
  t_value <- unname(coefs[term, "t value"])
  df_residual <- stats::df.residual(fit)
  tibble::tibble(
    estimate = unname(coefs[term, "Estimate"]),
    conf_low = unname(ci[1]),
    conf_high = unname(ci[2]),
    statistic = t_value,
    p_value = unname(coefs[term, "Pr(>|t|)"]),
    effect_size = t_value / sqrt(t_value^2 + df_residual),
    n = stats::nobs(fit)
  )
}

extract_glm_term <- function(data, formula, term) {
  fit <- tryCatch(
    suppressWarnings(stats::glm(formula, data = data, family = stats::binomial())),
    error = function(e) NULL
  )
  if (is.null(fit) || stats::nobs(fit) < 20) {
    return(tibble::tibble(
      estimate = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
      statistic = NA_real_, p_value = NA_real_, effect_size = NA_real_, n = 0
    ))
  }

  coefs <- summary(fit)$coefficients
  if (!term %in% rownames(coefs)) {
    return(tibble::tibble(
      estimate = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
      statistic = NA_real_, p_value = NA_real_, effect_size = NA_real_, n = stats::nobs(fit)
    ))
  }

  beta <- unname(coefs[term, "Estimate"])
  se <- unname(coefs[term, "Std. Error"])
  tibble::tibble(
    estimate = exp(beta),
    conf_low = exp(beta - 1.96 * se),
    conf_high = exp(beta + 1.96 * se),
    statistic = unname(coefs[term, "z value"]),
    p_value = unname(coefs[term, "Pr(>|z|)"]),
    effect_size = beta,
    n = stats::nobs(fit)
  )
}

extract_factor_test <- function(data, formula, term) {
  fit <- tryCatch(stats::lm(formula, data = data), error = function(e) NULL)
  if (is.null(fit) || stats::nobs(fit) < 20) {
    return(tibble::tibble(
      estimate = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
      statistic = NA_real_, p_value = NA_real_, effect_size = NA_real_, n = 0
    ))
  }

  test <- tryCatch(stats::drop1(fit, test = "F"), error = function(e) NULL)
  if (is.null(test) || !term %in% rownames(test)) {
    return(tibble::tibble(
      estimate = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
      statistic = NA_real_, p_value = NA_real_, effect_size = NA_real_, n = stats::nobs(fit)
    ))
  }

  f_value <- unname(test[term, "F value"])
  df_term <- unname(test[term, "Df"])
  df_residual <- stats::df.residual(fit)
  eta_sq <- (f_value * df_term) / (f_value * df_term + df_residual)
  tibble::tibble(
    estimate = eta_sq,
    conf_low = NA_real_,
    conf_high = NA_real_,
    statistic = f_value,
    p_value = unname(test[term, "Pr(>F)"]),
    effect_size = eta_sq,
    n = stats::nobs(fit)
  )
}

adjust_family <- function(data) {
  estimable <- !is.na(data$p_value)
  data$p_family_fdr <- NA_real_
  data$p_family_fdr[estimable] <- stats::p.adjust(data$p_value[estimable], method = "BH")
  data
}

# Adjusted POST differences answer whether AE status predicts endpoint scores
# after accounting for where participants started and for age.
post_ancova <- change_scores |>
  dplyr::filter(
    ae_status_primary %in% c("yes", "no"),
    !is.na(post), !is.na(pre), !is.na(age), !is.na(ae_yes_indicator)
  ) |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ extract_lm_term(.x, post ~ pre + age + ae_yes_indicator, "ae_yes_indicator")) |>
  dplyr::ungroup() |>
  adjust_family() |>
  dplyr::mutate(
    family = "Adjusted POST outcome",
    outcome = paste0("POST Item ", item, ": ", label),
    method = "Linear ANCOVA: improvement-coded POST ~ improvement-coded PRE + age + AE-yes indicator",
    controls = "Age and the same item's PRE score"
  )

# A binary view of change can reveal shifts hidden by means: did the participant
# improve at all? Odds ratios are adjusted for age and baseline score.
change_transitions <- change_scores |>
  dplyr::filter(ae_status_primary %in% c("yes", "no"), !is.na(change)) |>
  dplyr::mutate(
    transition = dplyr::case_when(change > 0 ~ "improved", change < 0 ~ "worsened", TRUE ~ "unchanged")
  ) |>
  dplyr::count(item, short_name, label, direction, ae_status_primary, transition) |>
  dplyr::group_by(item, ae_status_primary) |>
  dplyr::mutate(proportion = n / sum(n)) |>
  dplyr::ungroup()

improvement_models <- change_scores |>
  dplyr::filter(
    ae_status_primary %in% c("yes", "no"),
    !is.na(change), !is.na(pre), !is.na(age), !is.na(ae_yes_indicator)
  ) |>
  dplyr::mutate(improved = as.integer(change > 0)) |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ extract_glm_term(.x, improved ~ pre + age + ae_yes_indicator, "ae_yes_indicator")) |>
  dplyr::ungroup() |>
  adjust_family() |>
  dplyr::mutate(
    family = "Probability of any improvement",
    outcome = paste0("Change Item ", item, ": ", label),
    method = "Binary logistic regression: any positive improvement-coded change ~ PRE + age + AE-yes indicator",
    controls = "Age and the same item's PRE score"
  )

# Valence is complete for AE-yes participants. Positive versus negative is the
# only contrast with enough observations for a reasonably interpretable model.
valence_contrast_data <- change_scores |>
  dplyr::filter(
    ae_status == "yes", valence_group %in% c("positive", "negative"),
    !is.na(pre), !is.na(post), !is.na(change), !is.na(age)
  ) |>
  dplyr::mutate(positive_valence = as.integer(valence_group == "positive"))

valence_baseline <- valence_contrast_data |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ extract_lm_term(.x, pre ~ age + positive_valence, "positive_valence")) |>
  dplyr::ungroup() |>
  adjust_family() |>
  dplyr::mutate(
    family = "AE valence at baseline",
    outcome = paste0("PRE Item ", item, ": ", label),
    method = "Within AE yes, linear model: improvement-coded PRE ~ age + positive-versus-negative valence",
    controls = "Age; neutral and unsure valence excluded"
  )

valence_change <- valence_contrast_data |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ extract_lm_term(.x, change ~ pre + age + positive_valence, "positive_valence")) |>
  dplyr::ungroup() |>
  adjust_family() |>
  dplyr::mutate(
    family = "AE valence and observed change",
    outcome = paste0("Change Item ", item, ": ", label),
    method = "Within AE yes, linear model: improvement-coded change ~ PRE + age + positive-versus-negative valence",
    controls = "Age and the same item's PRE score; neutral and unsure valence excluded"
  )

valence_omnibus <- change_scores |>
  dplyr::filter(ae_status == "yes", !is.na(valence_group), !is.na(pre), !is.na(change), !is.na(age)) |>
  dplyr::mutate(valence_group = factor(valence_group)) |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ extract_factor_test(.x, change ~ pre + age + valence_group, "valence_group")) |>
  dplyr::ungroup() |>
  adjust_family() |>
  dplyr::mutate(
    family = "AE valence omnibus",
    outcome = paste0("Change Item ", item, ": ", label),
    method = "Within AE yes, omnibus linear-model F test across positive, negative, neutral, and unsure valence",
    controls = "Age and the same item's PRE score",
    caveat = "Neutral valence has only 3 participants, so this omnibus test is fragile."
  )

# AE subtype models are only estimated when both present and absent groups have
# at least five complete observations for the item.
eligible_types <- participants |>
  dplyr::filter(ae_status == "yes") |>
  dplyr::summarise(dplyr::across(dplyr::all_of(ae_type_columns), ~ sum(.x, na.rm = TRUE))) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "ae_type", values_to = "n_present") |>
  dplyr::filter(n_present >= 5)

type_baseline <- purrr::map_dfr(eligible_types$ae_type, function(type_name) {
  purrr::map_dfr(1:9, function(item_number) {
    dat <- change_scores |>
      dplyr::filter(ae_status == "yes", item == item_number, !is.na(pre), !is.na(age)) |>
      dplyr::mutate(type_present = as.integer(.data[[type_name]]))
    meta <- dat |> dplyr::slice_head(n = 1)
    n_present <- sum(dat$type_present == 1, na.rm = TRUE)
    n_absent <- sum(dat$type_present == 0, na.rm = TRUE)
    result <- if (n_present >= 5 && n_absent >= 5) {
      extract_lm_term(dat, pre ~ age + type_present, "type_present")
    } else {
      tibble::tibble(
        estimate = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
        statistic = NA_real_, p_value = NA_real_, effect_size = NA_real_, n = nrow(dat)
      )
    }
    dplyr::bind_cols(
      tibble::tibble(
        item = item_number,
        short_name = dplyr::first(meta$short_name),
        label = dplyr::first(meta$label),
        direction = dplyr::first(meta$direction),
        ae_type = type_name,
        n_present = n_present,
        n_absent = n_absent
      ),
      result
    )
  })
}) |>
  adjust_family() |>
  dplyr::mutate(
    family = "AE subtype at baseline",
    outcome = paste0("PRE Item ", item, ": ", label, " | ", stringr::str_remove(ae_type, "^ae_")),
    method = "Within AE yes, improvement-coded PRE ~ age + subtype-present indicator",
    controls = "Age",
    caveat = dplyr::if_else(n_present < 10, "Sparse subtype: interpret descriptively.", "Subtype categories overlap and are not mutually exclusive.")
  )

# Verification is analyzed only for the three routes with at least 10 AE-yes
# participants: conversation, artwork, and both.
verification_levels <- participants |>
  dplyr::filter(ae_status == "yes", !is.na(verification_method)) |>
  dplyr::count(verification_method) |>
  dplyr::filter(n >= 10) |>
  dplyr::pull(verification_method)

verification_baseline <- change_scores |>
  dplyr::filter(
    ae_status == "yes", verification_method %in% verification_levels,
    !is.na(pre), !is.na(age)
  ) |>
  dplyr::mutate(verification_method = droplevels(factor(verification_method))) |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ extract_factor_test(.x, pre ~ age + verification_method, "verification_method")) |>
  dplyr::ungroup() |>
  adjust_family() |>
  dplyr::mutate(
    family = "Verification route at baseline",
    outcome = paste0("PRE Item ", item, ": ", label),
    method = "Within AE yes, omnibus linear-model F test across conversation-only, artwork-only, and both",
    controls = "Age"
  )

verification_change <- change_scores |>
  dplyr::filter(
    ae_status == "yes", verification_method %in% verification_levels,
    !is.na(pre), !is.na(change), !is.na(age)
  ) |>
  dplyr::mutate(verification_method = droplevels(factor(verification_method))) |>
  dplyr::group_by(item, short_name, label, direction) |>
  dplyr::group_modify(~ extract_factor_test(.x, change ~ pre + age + verification_method, "verification_method")) |>
  dplyr::ungroup() |>
  adjust_family() |>
  dplyr::mutate(
    family = "Verification route and observed change",
    outcome = paste0("Change Item ", item, ": ", label),
    method = "Within AE yes, omnibus linear-model F test across conversation-only, artwork-only, and both",
    controls = "Age and the same item's PRE score"
  )

# Test whether complete follow-up differs by AE status. This checks whether the
# outcome analyses could be distorted by differential missingness.
missingness_data <- completeness |>
  dplyr::left_join(
    participants |> dplyr::select(participant_id, age, ae_status_primary, ae_yes_indicator),
    by = "participant_id"
  ) |>
  dplyr::filter(ae_status_primary %in% c("yes", "no"), !is.na(age)) |>
  dplyr::mutate(all_nine_pairs = as.integer(n_complete_pairs == 9))

missingness_model <- extract_glm_term(
  missingness_data,
  all_nine_pairs ~ age + ae_yes_indicator,
  "ae_yes_indicator"
) |>
  dplyr::mutate(
    family = "Outcome completeness",
    outcome = "All 9 PRE/POST item pairs complete",
    method = "Binary logistic regression: all nine complete pairs ~ age + AE-yes indicator",
    controls = "Age",
    p_family_fdr = p_value
  )

# Global multivariate tests ask whether the nine outcomes move together as an
# AE-related profile, reducing reliance on cherry-picked item-level tests.
wide_change <- change_scores |>
  dplyr::select(participant_id, item, pre, post, change) |>
  tidyr::pivot_wider(
    names_from = item,
    values_from = c(pre, post, change),
    names_glue = "{.value}_{item}"
  ) |>
  dplyr::left_join(
    participants |> dplyr::select(participant_id, age, ae_status_primary, ae_yes_indicator),
    by = "participant_id"
  ) |>
  dplyr::filter(ae_status_primary %in% c("yes", "no"))

extract_manova <- function(data, response_prefix, include_baselines = FALSE, label) {
  response_names <- paste0(response_prefix, "_", 1:9)
  predictor_names <- c("age", "ae_yes_indicator")
  if (include_baselines) predictor_names <- c(paste0("pre_", 1:9), predictor_names)
  dat <- data |>
    dplyr::select(dplyr::all_of(c(response_names, predictor_names))) |>
    tidyr::drop_na()
  formula <- stats::as.formula(
    paste0("cbind(", paste(response_names, collapse = ","), ") ~ ", paste(predictor_names, collapse = " + "))
  )
  fit <- tryCatch(stats::manova(formula, data = dat), error = function(e) NULL)
  if (is.null(fit)) {
    return(tibble::tibble(
      family = "Global multivariate AE profile", outcome = label, n = nrow(dat),
      estimate = NA_real_, statistic = NA_real_, p_value = NA_real_, p_family_fdr = NA_real_
    ))
  }
  stats_table <- summary(fit, test = "Pillai")$stats
  tibble::tibble(
    family = "Global multivariate AE profile",
    outcome = label,
    n = nrow(dat),
    estimate = unname(stats_table["ae_yes_indicator", "Pillai"]),
    statistic = unname(stats_table["ae_yes_indicator", "approx F"]),
    p_value = unname(stats_table["ae_yes_indicator", "Pr(>F)"]),
    p_family_fdr = NA_real_,
    method = "Nine-outcome MANOVA using Pillai's trace",
    controls = if (include_baselines) "Age and all nine corresponding PRE item scores" else "Age",
    caveat = "Linear multivariate model on ordinal item scores; use as a global robustness check."
  )
}

global_tests <- dplyr::bind_rows(
  extract_manova(wide_change, "pre", FALSE, "Global PRE outcome profile by AE status"),
  extract_manova(wide_change, "post", TRUE, "Global adjusted POST profile by AE status"),
  extract_manova(wide_change, "change", TRUE, "Global change profile by AE status")
) |>
  adjust_family()

# Can the baseline response profile distinguish AE yes from AE no? This is a
# descriptive prediction check, not a diagnostic model. Ten-fold predictions
# are generated out of sample to reduce optimism.
prediction_data <- wide_change |>
  dplyr::select(participant_id, ae_yes_indicator, age, dplyr::all_of(paste0("pre_", 1:9))) |>
  tidyr::drop_na() |>
  dplyr::arrange(ae_yes_indicator, participant_id) |>
  dplyr::group_by(ae_yes_indicator) |>
  dplyr::mutate(fold = (dplyr::row_number() - 1L) %% 10L + 1L) |>
  dplyr::ungroup()

prediction_formula <- stats::as.formula(
  paste0("ae_yes_indicator ~ age + ", paste0("pre_", 1:9, collapse = " + "))
)

prediction_probs <- purrr::map_dbl(seq_len(nrow(prediction_data)), function(i) {
  fold_i <- prediction_data$fold[i]
  train <- prediction_data |> dplyr::filter(fold != fold_i)
  test <- prediction_data[i, , drop = FALSE]
  fit <- tryCatch(
    suppressWarnings(stats::glm(prediction_formula, data = train, family = stats::binomial())),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NA_real_)
  as.numeric(stats::predict(fit, newdata = test, type = "response"))
})

auc_rank <- function(truth, probability) {
  keep <- !is.na(truth) & !is.na(probability)
  truth <- truth[keep]
  probability <- probability[keep]
  n_pos <- sum(truth == 1)
  n_neg <- sum(truth == 0)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  (sum(rank(probability)[truth == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

prediction_full <- suppressWarnings(stats::glm(prediction_formula, data = prediction_data, family = stats::binomial()))
prediction_age <- suppressWarnings(stats::glm(ae_yes_indicator ~ age, data = prediction_data, family = stats::binomial()))
prediction_lrt <- stats::anova(prediction_age, prediction_full, test = "Chisq")

prediction_summary <- tibble::tibble(
  family = "Baseline profile prediction",
  outcome = "Can age plus all nine PRE answers distinguish AE yes from AE no?",
  n = nrow(prediction_data),
  estimate = auc_rank(prediction_data$ae_yes_indicator, prediction_probs),
  statistic = unname(prediction_lrt$Deviance[2]),
  p_value = unname(prediction_lrt$`Pr(>Chi)`[2]),
  p_family_fdr = unname(prediction_lrt$`Pr(>Chi)`[2]),
  method = "Ten-fold out-of-sample logistic predictions; likelihood-ratio test compares the full baseline profile with age alone",
  controls = "Age",
  caveat = "Exploratory discrimination only; no external validation, and AE status is not a clinical diagnosis."
)

# Standardized inventory used by the dashboard and for atlas-wide FDR.
standardize <- function(data, family_name = NULL, effect_metric = "Adjusted coefficient") {
  defaults <- list(
    item = NA_integer_, conf_low = NA_real_, conf_high = NA_real_,
    effect_size = NA_real_, caveat = NA_character_, statistic = NA_real_
  )
  for (column_name in names(defaults)) {
    if (!column_name %in% names(data)) data[[column_name]] <- defaults[[column_name]]
  }
  if (!is.null(family_name)) data$family <- family_name

  data |>
    dplyr::transmute(
      family,
      outcome,
      item,
      estimate,
      conf_low,
      conf_high,
      statistic,
      effect_size,
      n,
      p_value,
      p_family_fdr,
      effect_metric,
      method,
      controls,
      caveat
    )
}

baseline_inventory <- key_differences |>
  dplyr::filter(family == "Baseline PRE item") |>
  dplyr::transmute(
    family = "AE status at baseline",
    outcome = field_analyzed,
    item = readr::parse_number(field_analyzed),
    estimate = adjusted_estimate,
    conf_low = NA_real_, conf_high = NA_real_, statistic = NA_real_,
    effect_size = rank_biserial_r, n = adjusted_n,
    p_value, p_family_fdr = p_fdr,
    effect_metric = "Adjusted AE-yes coefficient; rank-biserial effect also shown",
    method = "Mann-Whitney AE yes vs no plus linear model PRE ~ age + AE-yes indicator",
    controls = "Age",
    caveat = "Baseline association; temporality between AE identification and questionnaire response is uncertain."
  )

change_inventory <- second_order |>
  dplyr::transmute(
    family = "AE status and observed change",
    outcome = field_analyzed,
    item = readr::parse_number(field_analyzed),
    estimate = adjusted_estimate,
    conf_low = NA_real_, conf_high = NA_real_, statistic = NA_real_,
    effect_size = rank_biserial_r, n = adjusted_n,
    p_value, p_family_fdr = p_fdr,
    effect_metric = "Adjusted AE-yes change coefficient; rank-biserial effect also shown",
    method = "Mann-Whitney AE yes vs no on improvement-coded change plus change ~ PRE + age + AE-yes indicator",
    controls = "Age and the same item's PRE score",
    caveat = "Difference in observed change, not a causal intervention effect."
  )

moderation_inventory <- dplyr::bind_rows(
  age_ae_interactions |>
    dplyr::transmute(
      family = "AE-by-age moderation", outcome = field_analyzed,
      item, estimate, conf_low, conf_high, statistic,
      effect_size = partial_r, n, p_value, p_family_fdr,
      effect_metric = "Difference in age slope for AE yes versus AE no",
      method = "Change ~ PRE + age * AE-yes indicator",
      controls = controls,
      caveat = "Exploratory interaction; power is lower than for main effects."
    ),
  baseline_ae_interactions |>
    dplyr::transmute(
      family = "AE-by-baseline moderation", outcome = field_analyzed,
      item, estimate, conf_low, conf_high, statistic,
      effect_size = partial_r, n, p_value, p_family_fdr,
      effect_metric = "Difference in baseline-change slope for AE yes versus AE no",
      method = "Change ~ age + PRE * AE-yes indicator",
      controls = controls,
      caveat = "Change contains PRE, making this vulnerable to regression-to-the-mean and ceiling effects."
    )
)

atlas_inventory <- dplyr::bind_rows(
  baseline_inventory,
  standardize(post_ancova),
  change_inventory,
  standardize(improvement_models, effect_metric = "Adjusted odds ratio for any improvement"),
  moderation_inventory,
  standardize(valence_baseline, effect_metric = "Positive-minus-negative valence coefficient"),
  standardize(valence_change, effect_metric = "Positive-minus-negative valence coefficient"),
  standardize(valence_omnibus, effect_metric = "Partial eta-squared for valence"),
  standardize(type_baseline, effect_metric = "Subtype-present coefficient"),
  standardize(verification_baseline, effect_metric = "Partial eta-squared for verification route"),
  standardize(verification_change, effect_metric = "Partial eta-squared for verification route"),
  standardize(missingness_model, effect_metric = "Adjusted odds ratio for complete data"),
  standardize(global_tests, effect_metric = "Pillai trace"),
  standardize(prediction_summary, effect_metric = "Ten-fold cross-validated AUC")
)

estimable <- !is.na(atlas_inventory$p_value)
atlas_inventory$p_atlas_fdr <- NA_real_
atlas_inventory$p_atlas_fdr[estimable] <- stats::p.adjust(atlas_inventory$p_value[estimable], method = "BH")
atlas_inventory <- atlas_inventory |>
  dplyr::mutate(
    evidence = dplyr::case_when(
      !is.na(p_atlas_fdr) & p_atlas_fdr < 0.05 ~ "Atlas-wide FDR < .05",
      !is.na(p_family_fdr) & p_family_fdr < 0.05 ~ "Family FDR < .05",
      !is.na(p_value) & p_value < 0.05 ~ "Nominal p < .05 only",
      !is.na(p_value) ~ "No statistical signal",
      TRUE ~ "Descriptive / insufficient data"
    ),
    p_text = p_display(p_value),
    family_fdr_text = p_display(p_family_fdr),
    atlas_fdr_text = p_display(p_atlas_fdr)
  ) |>
  dplyr::arrange(factor(evidence, levels = c(
    "Atlas-wide FDR < .05", "Family FDR < .05", "Nominal p < .05 only",
    "No statistical signal", "Descriptive / insufficient data"
  )), p_value)

transition_text <- function(item_number, group) {
  rows <- change_transitions |>
    dplyr::filter(item == item_number, ae_status_primary == group)
  get_prop <- function(level) {
    value <- rows$proportion[match(level, rows$transition)]
    ifelse(is.na(value), 0, value)
  }
  paste0(
    stringr::str_to_upper(group), ": ",
    round(100 * get_prop("improved")), "% improved, ",
    round(100 * get_prop("unchanged")), "% unchanged, ",
    round(100 * get_prop("worsened")), "% worsened"
  )
}

make_plain_description <- function(family, outcome, estimate, effect_metric, p_family_fdr) {
  signal <- ifelse(!is.na(p_family_fdr) && p_family_fdr < 0.05, "The pattern survives correction within this family.", "The pattern does not survive correction within this family.")
  direction <- dplyr::case_when(
    is.na(estimate) ~ "No stable estimate was available.",
    stringr::str_detect(family, "AE valence omnibus") ~ "This tests whether the four recorded valence groups differ overall.",
    stringr::str_detect(family, "AE valence") & estimate > 0 ~ "Positive-valence AE was associated with a higher improvement-coded score than negative-valence AE.",
    stringr::str_detect(family, "AE valence") & estimate < 0 ~ "Positive-valence AE was associated with a lower improvement-coded score than negative-valence AE.",
    stringr::str_detect(family, "subtype") & estimate > 0 ~ "Participants with this subtype had a higher improvement-coded score than other AE-yes participants.",
    stringr::str_detect(family, "subtype") & estimate < 0 ~ "Participants with this subtype had a lower improvement-coded score than other AE-yes participants.",
    stringr::str_detect(family, "Verification") ~ "This tests whether the three adequately sized verification routes differ overall.",
    stringr::str_detect(family, "moderation") & estimate > 0 ~ "The AE-yes slope was more positive than the AE-no slope.",
    stringr::str_detect(family, "moderation") & estimate < 0 ~ "The AE-yes slope was more negative than the AE-no slope.",
    family == "Global multivariate AE profile" ~ "This tests AE status across all nine outcomes jointly.",
    family == "Baseline profile prediction" ~ "This measures how well the complete PRE profile distinguishes AE yes from AE no out of sample.",
    stringr::str_detect(effect_metric, "odds ratio") & estimate > 1 ~ "AE yes was associated with higher odds.",
    stringr::str_detect(effect_metric, "odds ratio") & estimate < 1 ~ "AE yes was associated with lower odds.",
    estimate > 0 ~ "AE yes was associated with a higher improvement-coded outcome.",
    estimate < 0 ~ "AE yes was associated with a lower improvement-coded outcome.",
    TRUE ~ "The estimated association was approximately zero."
  )
  paste(family, "for", outcome, direction, signal)
}

atlas_observations <- atlas_inventory |>
  dplyr::rowwise() |>
  dplyr::mutate(
    section = dplyr::case_when(
      family == "AE status at baseline" ~ "1. Baseline relationships",
      family == "Adjusted POST outcome" ~ "2. Adjusted POST relationships",
      family %in% c("AE status and observed change", "Probability of any improvement") ~ "3. PRE-to-POST relationships",
      stringr::str_detect(family, "moderation") ~ "4. Moderation by age or baseline",
      stringr::str_detect(family, "valence") ~ "5. AE valence",
      stringr::str_detect(family, "subtype") ~ "6. AE subtypes",
      stringr::str_detect(family, "Verification") ~ "7. Verification route",
      family %in% c("Outcome completeness", "Global multivariate AE profile", "Baseline profile prediction") ~ "8. Robustness and global checks",
      TRUE ~ "9. Other"
    ),
    title = outcome,
    description = make_plain_description(family, outcome, estimate, effect_metric, p_family_fdr),
    calculation = paste0(
      effect_metric, ": ", ifelse(is.na(estimate), "NA", round(estimate, 3)),
      ifelse(is.na(conf_low), "", paste0("; 95% CI ", round(conf_low, 3), " to ", round(conf_high, 3))),
      "; n=", n, "; raw p ", p_text,
      "; family FDR ", family_fdr_text,
      "; atlas FDR ", atlas_fdr_text, "."
    ),
    related_data = dplyr::case_when(
      family %in% c("AE status and observed change", "Probability of any improvement") & !is.na(item) ~ paste(
        transition_text(item, "yes"), transition_text(item, "no"), sep = "; "
      ),
      family == "AE status at baseline" & !is.na(item) ~ paste0(
        "Unadjusted rank-biserial r=", round(effect_size, 3),
        ". AE-unknown sensitivity results are available in the Sensitivity tab."
      ),
      family == "Global multivariate AE profile" ~ "This global test examines all nine outcomes jointly and is less vulnerable to selecting a single favorable item.",
      family == "Baseline profile prediction" ~ "AUC 0.50 is chance; values nearer 1.00 indicate stronger discrimination.",
      TRUE ~ "See the complete inventory table below for related rows in the same analysis family."
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    section, family, title, description, evidence, method, calculation,
    controls, related_data, caveat, item, estimate, conf_low, conf_high,
    effect_size, n, p_value, p_family_fdr, p_atlas_fdr
  )

manual_atlas_observations <- tibble::tribble(
  ~section, ~family, ~title, ~description, ~evidence, ~method, ~calculation, ~controls, ~related_data, ~caveat,
  "0. Scope and coding",
  "AE status definition",
  "Who is included in the primary AE comparison?",
  "Primary AE analyses compare the 54 AE-yes participants with the 87 AE-no participants; 45 unknown records are excluded from that primary contrast.",
  "Descriptive / data definition",
  "AE status is standardized from y/yes, n/no, and ?/uncertain source values.",
  "AE yes=54; AE no=87; AE unknown=45; total=186.",
  "Not applicable.",
  "Sensitivity tables separately recode unknown as no and as yes.",
  "The project team should confirm what '?' and other unknown records operationally mean.",
  "0. Scope and coding",
  "AE valence definition",
  "How was AE valence grouped?",
  "All 54 AE-yes records have a valence label: 12 positive, 23 negative, 16 unsure, and 3 neutral after grouping.",
  "Descriptive / data definition",
  "Positive and mostly-positive are grouped as positive; labels containing negative are grouped as negative; unsure and neutral remain separate.",
  "Positive=12; negative=23; unsure=16; neutral=3.",
  "Age is controlled in baseline valence contrasts; age and PRE score are controlled in change contrasts.",
  "The positive-versus-negative model excludes unsure and neutral records.",
  "The source label 'negative (scary) and neutral' is classified as negative; this judgment should receive human confirmation.",
  "0. Scope and coding",
  "AE subtype definition",
  "What AE subtypes are represented?",
  "Subtype coding is non-exclusive: a participant can contribute to more than one subtype.",
  "Descriptive / data definition",
  "Keyword-based coding of the source AE-type text.",
  "Entities/beings=42; auditory=6; visual=5; cosmological/mystical=5; deceased=3; precognitive/dream=1.",
  "Age is controlled in subtype baseline models.",
  "Inferential rows require at least five complete present and absent cases for the specific item.",
  "Most subtype cells are too small for reliable inference, and final text coding requires project-team review.",
  "0. Scope and coding",
  "Verification definition",
  "How were AE reports verified or identified?",
  "Among AE-yes records, 24 are conversation-only, 16 combine conversation and artwork, 12 are artwork-only, 1 was directly witnessed, and 1 has no coded route.",
  "Descriptive / data definition",
  "Keyword-based grouping of verification notes.",
  "Only the three routes with at least 10 participants enter omnibus inferential models.",
  "Age is controlled at baseline; age and PRE score are controlled for change.",
  "Direct-witness and missing-route records remain visible descriptively but are excluded from route comparisons.",
  "Verification categories require human review where conversation or artwork was only implied.",
  "0. Scope and coding",
  "Unavailable covariates",
  "What potentially important information is absent?",
  "The workbook does not provide sex, race/ethnicity, site or cohort, intervention dose, AE timing, diagnostic status, standardized depression measures, or an untreated comparison group.",
  "Descriptive / data definition",
  "Field inventory of the 26 workbook columns and the supplied analysis plan.",
  "No calculation; these fields are absent.",
  "Not applicable.",
  "Age is the only broadly available adjustment variable beyond each outcome's PRE score.",
  "Residual confounding is unavoidable, and causal claims about AE or the intervention are not supported."
) |>
  dplyr::mutate(
    item = NA_integer_, estimate = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
    effect_size = NA_real_, n = NA_real_, p_value = NA_real_,
    p_family_fdr = NA_real_, p_atlas_fdr = NA_real_
  ) |>
  dplyr::select(dplyr::all_of(names(atlas_observations)))

atlas_observations <- dplyr::bind_rows(manual_atlas_observations, atlas_observations)

# Human-readable top-level synthesis. These rows intentionally include null
# findings because the central question is whether AE meaningfully relates to
# outcomes, not merely which isolated p-values are smallest.
best_change <- second_order |> dplyr::arrange(p_value) |> dplyr::slice_head(n = 1)
anxiety_change <- second_order |> dplyr::filter(stringr::str_detect(field_analyzed, "anxious")) |> dplyr::slice_head(n = 1)
global_change <- global_tests |> dplyr::filter(stringr::str_detect(outcome, "change profile")) |> dplyr::slice_head(n = 1)
global_post <- global_tests |> dplyr::filter(stringr::str_detect(outcome, "POST profile")) |> dplyr::slice_head(n = 1)
valence_anxiety <- valence_baseline |> dplyr::filter(item == 6) |> dplyr::slice_head(n = 1)
valence_confidence <- valence_baseline |> dplyr::filter(item == 1) |> dplyr::slice_head(n = 1)
atlas_supported <- atlas_inventory |> dplyr::filter(evidence == "Atlas-wide FDR < .05")
family_supported <- atlas_inventory |> dplyr::filter(evidence == "Family FDR < .05")
nominal_only <- atlas_inventory |> dplyr::filter(evidence == "Nominal p < .05 only")

overall_signals <- tibble::tribble(
  ~priority, ~title, ~description, ~method, ~calculation, ~related_data, ~caveat,
  1L,
  "Bottom line: the data do not show robust evidence that AE status changed program outcomes differently",
  "Across item-level, adjusted endpoint, improvement-probability, and global multivariate analyses, AE-related change differences are mostly small and do not survive multiplicity correction.",
  "Synthesis of the AE atlas with family-wise and atlas-wide Benjamini-Hochberg correction.",
  paste0("Global adjusted change profile: Pillai=", round(global_change$estimate, 3), ", p ", p_display(global_change$p_value), ". Atlas-wide supported AE rows: ", nrow(atlas_supported), "."),
  paste0("The smallest item-level change p-value was for ", best_change$field_analyzed, " (raw p ", p_display(best_change$p_value), ", family FDR ", p_display(best_change$p_fdr), ")."),
  "Absence of strong evidence is not proof of no AE relationship; subgroup power is limited and AE identification was informal.",
  2L,
  "There are meaningful PRE-to-POST improvements in the sample overall",
  paste0(sum(full_prepost$p_fdr < 0.05, na.rm = TRUE), " of 9 outcomes improve after item-wise FDR correction, irrespective of AE status."),
  "Paired Wilcoxon signed-rank tests on complete pairs; positive change is improvement-coded.",
  paste(full_prepost$label[full_prepost$p_fdr < 0.05], collapse = "; "),
  "The clearest gains are art confidence, self-confidence, reduced social pressure, self-expression, and reduced anxiety/fear.",
  "Without an untreated control group these changes cannot be attributed solely to the intervention.",
  3L,
  "Age is a clearer modifier of observed change than AE status",
  "Older participants generally show smaller improvements on several confidence and expression outcomes after adjustment for baseline and AE status.",
  "Item-level linear models: change ~ PRE + age + AE status, with family and global FDR.",
  paste0(
    sum(age_effects$p_family_fdr < 0.05, na.rm = TRUE), " age slopes survive the age-family FDR; ",
    sum(observation_scan$analysis_family == "Age effect on observed change" & observation_scan$p_global_fdr < 0.05, na.rm = TRUE),
    " survive the previous global exploratory FDR."
  ),
  paste(age_effects$field_analyzed[age_effects$p_family_fdr < 0.05], collapse = "; "),
  "Age may proxy developmental stage, program cohort, response style, or unmeasured exposure; it is not necessarily a causal moderator.",
  4L,
  "AE prevalence rises with age in the primary yes-versus-no sample",
  ae_age_prevalence$interpretation[1],
  "Binary logistic model of AE yes versus no by continuous age.",
  paste0("n=", ae_age_prevalence$n[1], "; raw p ", p_display(ae_age_prevalence$p_value[1]), "."),
  "The categorical age-group Fisher test is weaker, illustrating sensitivity to how age is represented.",
  "This is an association with recorded AE status, not evidence that age causes anomalous experiences.",
  5L,
  "The largest AE change contrast is self-expression, but it remains uncertain",
  paste0("AE-yes participants show a larger mean gain in self-expression than AE-no participants, but the result is just above the conventional raw threshold and far above the family-FDR threshold."),
  best_change$controls,
  paste0("AE-yes mean change=", round(best_change$mean_yes, 2), "; AE-no=", round(best_change$mean_no, 2), "; contrast=", round(best_change$change_contrast, 2), "; raw p ", best_change$p_text, "; FDR ", best_change$fdr_p_text, "."),
  paste(best_change$ae_yes_answer_change, best_change$ae_no_answer_change),
  "This is a promising descriptive lead, not a confirmed AE-specific program response.",
  6L,
  "Anxiety improves in both AE groups; the larger AE-yes reduction is not statistically secure",
  paste(anxiety_change$ae_yes_answer_change, anxiety_change$ae_no_answer_change),
  anxiety_change$controls,
  paste0("Difference-in-change=", round(anxiety_change$change_contrast, 2), "; raw p ", anxiety_change$p_text, "; FDR ", anxiety_change$fdr_p_text, "; adjusted p ", anxiety_change$adjusted_p_text, "."),
  "The original answers decrease, so this is less reported anxiety/fear in both groups.",
  "The direction is encouraging, but uncertainty is substantial.",
  7L,
  "AE valence is the strongest within-AE signal",
  paste0(
    "Compared with negative-valence AE, positive-valence AE is associated with ",
    round(valence_anxiety$estimate, 2), " points better baseline anxiety/fear score and ",
    round(valence_confidence$estimate, 2), " points higher baseline self-confidence after age adjustment."
  ),
  "Within AE yes, improvement-coded PRE ~ age + positive-versus-negative valence; BH-FDR across 9 items.",
  paste0(
    "Anxiety family FDR ", p_display(valence_anxiety$p_family_fdr),
    "; self-confidence family FDR ", p_display(valence_confidence$p_family_fdr),
    "; neither survives atlas-wide FDR."
  ),
  "Positive-valence AE also has nominally more favorable change in self-expression and social confidence, but these change contrasts miss family FDR.",
  "Positive n=12 and negative n=23 before item-specific missingness; unsure and neutral groups are excluded from this contrast.",
  8L,
  "The global adjusted POST profile does not provide a simple AE separation",
  "When all nine POST outcomes are considered jointly and all nine PRE scores plus age are controlled, the AE coefficient is evaluated as one multivariate profile.",
  global_post$method,
  paste0("Pillai=", round(global_post$estimate, 3), "; p ", p_display(global_post$p_value), "; n=", global_post$n, "."),
  "This complements the item-by-item adjusted POST analyses.",
  global_post$caveat,
  9L,
  "The most defensible presentation is a restrained one",
  "Lead with overall sample improvement and the age gradient; present AE-specific self-expression and anxiety contrasts as exploratory; explicitly report that no AE outcome contrast survives family FDR.",
  "Evidence hierarchy: global tests, multiplicity-corrected families, effect sizes and confidence intervals, then nominal leads.",
  "Do not claim that AE experiences caused better or worse outcomes from this dataset.",
  "Report the 45 unknown AE records and the alternate recoding sensitivity analyses.",
  "Sex, cohort/site, intervention dose, AE timing, diagnostic status, and standardized depression measures are unavailable."
)

write_csv_safe(post_ancova, "outputs/tables/ae_atlas_post_ancova.csv")
write_csv_safe(change_transitions, "outputs/tables/ae_atlas_change_transitions.csv")
write_csv_safe(improvement_models, "outputs/tables/ae_atlas_improvement_models.csv")
write_csv_safe(valence_baseline, "outputs/tables/ae_atlas_valence_baseline.csv")
write_csv_safe(valence_change, "outputs/tables/ae_atlas_valence_change.csv")
write_csv_safe(valence_omnibus, "outputs/tables/ae_atlas_valence_omnibus.csv")
write_csv_safe(type_baseline, "outputs/tables/ae_atlas_type_baseline.csv")
write_csv_safe(verification_baseline, "outputs/tables/ae_atlas_verification_baseline.csv")
write_csv_safe(verification_change, "outputs/tables/ae_atlas_verification_change.csv")
write_csv_safe(missingness_model, "outputs/tables/ae_atlas_missingness.csv")
write_csv_safe(global_tests, "outputs/tables/ae_atlas_global_tests.csv")
write_csv_safe(prediction_summary, "outputs/tables/ae_atlas_prediction_summary.csv")
write_csv_safe(atlas_inventory, "outputs/tables/ae_atlas_inventory.csv")
write_csv_safe(atlas_observations, "outputs/tables/ae_atlas_observations.csv")
write_csv_safe(overall_signals, "outputs/tables/overall_signal_observations.csv")

message("Comprehensive AE relationship atlas written to outputs/tables/.")
