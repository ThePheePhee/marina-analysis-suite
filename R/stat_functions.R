# Statistical helpers matching the supplied analysis plan.

median_iqr <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return("NA")
  }

  q <- stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 2)
  sprintf("%.2f (%.2f, %.2f)", q[2], q[1], q[3])
}

rank_biserial_independent <- function(x, group) {
  dat <- tibble::tibble(x = x, group = group) |>
    dplyr::filter(!is.na(x), !is.na(group)) |>
    dplyr::filter(group %in% c("yes", "no")) |>
    dplyr::mutate(group = factor(group, levels = c("yes", "no")))

  if (dplyr::n_distinct(dat$group) < 2) {
    return(NA_real_)
  }

  n_yes <- sum(dat$group == "yes")
  n_no <- sum(dat$group == "no")
  w_yes <- sum(rank(dat$x)[dat$group == "yes"])
  u_yes <- w_yes - n_yes * (n_yes + 1) / 2

  2 * u_yes / (n_yes * n_no) - 1
}

rank_biserial_paired <- function(change) {
  change <- change[!is.na(change) & change != 0]
  if (length(change) == 0) {
    return(NA_real_)
  }

  ranks <- rank(abs(change))
  w_pos <- sum(ranks[change > 0])
  w_neg <- sum(ranks[change < 0])
  (w_pos - w_neg) / (w_pos + w_neg)
}

mann_whitney_summary <- function(data, value, group = ae_status_primary) {
  value <- rlang::enquo(value)
  group <- rlang::enquo(group)

  dat <- data |>
    dplyr::select(value = !!value, group = !!group) |>
    dplyr::filter(!is.na(value), group %in% c("yes", "no")) |>
    dplyr::mutate(group = factor(group, levels = c("yes", "no")))

  if (nrow(dat) == 0 || dplyr::n_distinct(dat$group) < 2) {
    return(tibble::tibble(
      n_yes = sum(dat$group == "yes"),
      n_no = sum(dat$group == "no"),
      median_iqr_yes = median_iqr(dat$value[dat$group == "yes"]),
      median_iqr_no = median_iqr(dat$value[dat$group == "no"]),
      statistic = NA_real_,
      p_value = NA_real_,
      rank_biserial_r = NA_real_,
      test_note = "Insufficient data for AE yes/no comparison"
    ))
  }

  wt <- tryCatch(
    stats::wilcox.test(value ~ group, data = dat, exact = FALSE),
    error = function(e) NULL
  )

  tibble::tibble(
    n_yes = sum(dat$group == "yes"),
    n_no = sum(dat$group == "no"),
    median_iqr_yes = median_iqr(dat$value[dat$group == "yes"]),
    median_iqr_no = median_iqr(dat$value[dat$group == "no"]),
    statistic = if (is.null(wt)) NA_real_ else unname(wt$statistic),
    p_value = if (is.null(wt)) NA_real_ else wt$p.value,
    rank_biserial_r = rank_biserial_independent(dat$value, dat$group),
    test_note = "Wilcoxon rank-sum/Mann-Whitney test; exact p may be unavailable with ties"
  )
}

wilcoxon_change_summary <- function(data, change) {
  change <- rlang::enquo(change)

  dat <- data |>
    dplyr::select(change = !!change) |>
    dplyr::filter(!is.na(change))

  if (nrow(dat) == 0) {
    return(tibble::tibble(
      n_pairs = 0,
      median_iqr_change = "NA",
      statistic = NA_real_,
      p_value = NA_real_,
      rank_biserial_r = NA_real_,
      test_note = "No complete pairs"
    ))
  }

  wt <- tryCatch(
    stats::wilcox.test(dat$change, mu = 0, exact = FALSE),
    error = function(e) NULL
  )

  tibble::tibble(
    n_pairs = nrow(dat),
    median_iqr_change = median_iqr(dat$change),
    statistic = if (is.null(wt)) NA_real_ else unname(wt$statistic),
    p_value = if (is.null(wt)) NA_real_ else wt$p.value,
    rank_biserial_r = rank_biserial_paired(dat$change),
    test_note = "Wilcoxon signed-rank test on complete non-missing pairs"
  )
}

choose_age_prevalence_test <- function(data) {
  tab <- table(data$age_group, data$ae_status_primary, useNA = "no")
  tab <- tab[, intersect(colnames(tab), c("yes", "no")), drop = FALSE]

  if (nrow(tab) < 2 || ncol(tab) < 2) {
    return(tibble::tibble(
      test = "not_run",
      statistic = NA_real_,
      p_value = NA_real_,
      note = "Insufficient non-empty age group by AE yes/no cells"
    ))
  }

  chi <- suppressWarnings(stats::chisq.test(tab))
  if (any(chi$expected < 5)) {
    ft <- stats::fisher.test(tab)
    tibble::tibble(
      test = "Fisher exact",
      statistic = NA_real_,
      p_value = ft$p.value,
      note = "Used Fisher exact because at least one expected cell count was < 5"
    )
  } else {
    tibble::tibble(
      test = "Chi-square",
      statistic = unname(chi$statistic),
      p_value = chi$p.value,
      note = "All expected cell counts >= 5"
    )
  }
}
