# Shared manuscript-output helpers for scripts/10_research_questions.R and Shiny.

rq_p_text <- function(p) {
  if (length(p) == 0 || is.na(p)) return("not estimable")
  if (p < 0.001) return("< .001")
  paste0("= ", formatC(p, format = "f", digits = 3))
}

rq_effect_label <- function(r) {
  if (is.na(r)) return("not estimable")
  magnitude <- dplyr::case_when(
    abs(r) < 0.1 ~ "negligible",
    abs(r) < 0.3 ~ "small",
    abs(r) < 0.5 ~ "medium",
    TRUE ~ "large"
  )
  paste0(magnitude, " (r = ", formatC(r, format = "f", digits = 2), ")")
}

rq_theme <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle = ggplot2::element_text(color = "#475467", margin = ggplot2::margin(b = 8)),
      plot.caption = ggplot2::element_text(color = "#667085", hjust = 0, size = base_size - 2),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(10, 14, 10, 10)
    )
}

rq_group_scale <- c("AE: no" = "#6B7280", "AE: yes" = "#007C91")

rq_plot_pre_anxiety <- function(participants) {
  participants |>
    dplyr::filter(ae_status_primary %in% c("yes", "no"), !is.na(pre_item6)) |>
    dplyr::mutate(ae_group = factor(dplyr::recode(ae_status_primary, no = "AE: no", yes = "AE: yes"), levels = names(rq_group_scale))) |>
    ggplot2::ggplot(ggplot2::aes(ae_group, pre_item6, fill = ae_group)) +
    ggplot2::geom_violin(trim = FALSE, alpha = 0.42, color = NA, width = 0.88) +
    ggplot2::geom_boxplot(width = 0.16, outlier.shape = NA, alpha = 0.85, color = "#344054") +
    ggplot2::geom_jitter(width = 0.09, height = 0, alpha = 0.48, size = 1.5, color = "#243746") +
    ggplot2::scale_fill_manual(values = rq_group_scale) +
    ggplot2::scale_y_continuous(breaks = 1:10) +
    ggplot2::coord_cartesian(ylim = c(0.75, 10.25)) +
    ggplot2::labs(
      x = NULL, y = "PRE Item 6 score (higher = more anxiety/fear)",
      title = "Figure 1. Baseline anxiety/fear by AE status",
      subtitle = "Distribution, median, interquartile range, and individual observations",
      caption = "AE-unknown participants are excluded from the primary comparison."
    ) +
    rq_theme() +
    ggplot2::theme(legend.position = "none")
}

rq_plot_pre_wellbeing <- function(participants) {
  participants |>
    dplyr::filter(ae_status_primary %in% c("yes", "no"), !is.na(wellbeing_composite_pre)) |>
    dplyr::mutate(ae_group = factor(dplyr::recode(ae_status_primary, no = "AE: no", yes = "AE: yes"), levels = names(rq_group_scale))) |>
    ggplot2::ggplot(ggplot2::aes(ae_group, wellbeing_composite_pre, fill = ae_group)) +
    ggplot2::geom_violin(trim = FALSE, alpha = 0.42, color = NA, width = 0.88) +
    ggplot2::geom_boxplot(width = 0.16, outlier.shape = NA, alpha = 0.85, color = "#344054") +
    ggplot2::geom_jitter(width = 0.09, height = 0, alpha = 0.48, size = 1.5, color = "#243746") +
    ggplot2::scale_fill_manual(values = rq_group_scale) +
    ggplot2::scale_y_continuous(breaks = 1:10) +
    ggplot2::coord_cartesian(ylim = c(0.75, 10.25)) +
    ggplot2::labs(
      x = NULL, y = "PRE wellbeing composite (higher = better)",
      title = "Figure 2. Baseline composite wellbeing by AE status",
      subtitle = "Nine-item mean after reversing Items 5 and 6",
      caption = "Composite validity and reliability have not been established for this questionnaire."
    ) +
    rq_theme() +
    ggplot2::theme(legend.position = "none")
}

rq_plot_item6_paired <- function(item_long, participants) {
  plot_data <- item_long |>
    dplyr::filter(item == 6, !is.na(score)) |>
    dplyr::left_join(participants |> dplyr::select(participant_id, ae_status_primary), by = "participant_id") |>
    dplyr::filter(ae_status_primary %in% c("yes", "no")) |>
    dplyr::mutate(
      ae_group = factor(dplyr::recode(ae_status_primary, no = "AE: no", yes = "AE: yes"), levels = names(rq_group_scale)),
      timepoint = factor(timepoint, levels = c("pre", "post"), labels = c("PRE", "POST"))
    )

  medians <- plot_data |>
    dplyr::group_by(ae_group, timepoint) |>
    dplyr::summarise(median = stats::median(score, na.rm = TRUE), .groups = "drop")

  ggplot2::ggplot(plot_data, ggplot2::aes(timepoint, score, group = participant_id)) +
    ggplot2::geom_line(color = "#98A2B3", alpha = 0.24, linewidth = 0.45) +
    ggplot2::geom_point(color = "#667085", alpha = 0.35, size = 1.25, position = ggplot2::position_jitter(width = 0.025, height = 0)) +
    ggplot2::geom_line(data = medians, ggplot2::aes(timepoint, median, group = ae_group, color = ae_group), linewidth = 1.25, inherit.aes = FALSE) +
    ggplot2::geom_point(data = medians, ggplot2::aes(timepoint, median, color = ae_group), size = 3, inherit.aes = FALSE) +
    ggplot2::facet_wrap(~ ae_group) +
    ggplot2::scale_color_manual(values = rq_group_scale) +
    ggplot2::scale_y_continuous(breaks = 1:10) +
    ggplot2::coord_cartesian(ylim = c(0.75, 10.25)) +
    ggplot2::labs(
      x = NULL, y = "Item 6 score (higher = more anxiety/fear)",
      title = "Figure 3. PRE-to-POST anxiety/fear trajectories",
      subtitle = "Thin lines are participants; the heavy line connects group medians",
      caption = "Only observed scores are drawn. Inferential analyses use complete PRE-POST pairs per item."
    ) +
    rq_theme() +
    ggplot2::theme(legend.position = "none")
}

rq_plot_ae_types <- function(participants) {
  participants |>
    dplyr::filter(ae_status == "yes") |>
    dplyr::select(participant_id, verification_method, dplyr::all_of(ae_type_columns)) |>
    tidyr::pivot_longer(dplyr::all_of(ae_type_columns), names_to = "ae_type", values_to = "present") |>
    dplyr::filter(present) |>
    dplyr::mutate(
      ae_type = stringr::str_replace(ae_type, "^ae_", ""),
      ae_type = stringr::str_replace_all(ae_type, "_", " "),
      verification_method = dplyr::coalesce(verification_method, "not recorded"),
      verification_method = stringr::str_replace_all(verification_method, "_", " ")
    ) |>
    dplyr::count(ae_type, verification_method, name = "n") |>
    dplyr::group_by(ae_type) |>
    dplyr::mutate(total = sum(n)) |>
    dplyr::ungroup() |>
    ggplot2::ggplot(ggplot2::aes(stats::reorder(ae_type, total), n, fill = verification_method)) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::labs(
      x = NULL, y = "Number of coded AE types",
      title = "Figure 4. AE type frequencies and verification routes",
      subtitle = "Categories are non-exclusive; one participant may contribute to multiple bars",
      caption = "Free-text coding requires final human verification before manuscript reporting."
    ) +
    rq_theme()
}

rq_plot_item_5_6_distributions <- function(item_long, participants) {
  item_long |>
    dplyr::filter(timepoint == "pre", item %in% c(5, 6), !is.na(score)) |>
    dplyr::left_join(participants |> dplyr::select(participant_id, ae_status_primary), by = "participant_id") |>
    dplyr::filter(ae_status_primary %in% c("yes", "no")) |>
    dplyr::mutate(
      ae_group = factor(dplyr::recode(ae_status_primary, no = "AE: no", yes = "AE: yes"), levels = names(rq_group_scale)),
      outcome = dplyr::recode(as.character(item), `5` = "Item 5: social pressure", `6` = "Item 6: anxiety/fear")
    ) |>
    ggplot2::ggplot(ggplot2::aes(score, fill = ae_group, color = ae_group)) +
    ggplot2::geom_histogram(binwidth = 1, boundary = 0.5, position = "identity", alpha = 0.30) +
    ggplot2::facet_grid(outcome ~ ae_group) +
    ggplot2::scale_fill_manual(values = rq_group_scale) +
    ggplot2::scale_color_manual(values = rq_group_scale) +
    ggplot2::scale_x_continuous(breaks = 1:10) +
    ggplot2::labs(
      x = "PRE score (higher = more distress)", y = "Count",
      title = "Supplementary Figure 1. Baseline Item 5 and 6 distributions",
      subtitle = "One-point bins preserve the observed 1-10 response scale",
      caption = "Non-integer values are retained as valid responses under the analysis plan."
    ) +
    rq_theme() +
    ggplot2::theme(legend.position = "none")
}

rq_plot_full_sample_change <- function(prepost_tests) {
  prepost_tests |>
    dplyr::mutate(
      outcome = paste0("Item ", item, ": ", label),
      supported = dplyr::if_else(p_fdr < 0.05, "BH FDR < .05", "Not FDR-supported")
    ) |>
    ggplot2::ggplot(ggplot2::aes(stats::reorder(outcome, rank_biserial_r), rank_biserial_r, color = supported)) +
    ggplot2::geom_hline(yintercept = 0, color = "#98A2B3") +
    ggplot2::geom_segment(ggplot2::aes(xend = outcome, y = 0, yend = rank_biserial_r), linewidth = 0.7) +
    ggplot2::geom_point(size = 2.8) +
    ggplot2::coord_flip() +
    ggplot2::scale_color_manual(values = c("BH FDR < .05" = "#007A5E", "Not FDR-supported" = "#7C8795")) +
    ggplot2::labs(
      x = NULL, y = "Paired rank-biserial correlation",
      title = "Full-sample PRE-to-POST change across all outcomes",
      subtitle = "Positive effects indicate improvement after direction-coding Items 5 and 6",
      caption = "BH correction is applied across the nine planned item tests."
    ) +
    rq_theme()
}

rq_plot_ae_change_contrasts <- function(change_by_ae_tests) {
  change_by_ae_tests |>
    dplyr::mutate(
      outcome = paste0("Item ", item, ": ", label),
      supported = dplyr::if_else(p_fdr < 0.05, "BH FDR < .05", "Not FDR-supported")
    ) |>
    ggplot2::ggplot(ggplot2::aes(stats::reorder(outcome, rank_biserial_r), rank_biserial_r, color = supported)) +
    ggplot2::geom_hline(yintercept = 0, color = "#98A2B3") +
    ggplot2::geom_segment(ggplot2::aes(xend = outcome, y = 0, yend = rank_biserial_r), linewidth = 0.7) +
    ggplot2::geom_point(size = 2.8) +
    ggplot2::coord_flip() +
    ggplot2::scale_color_manual(values = c("BH FDR < .05" = "#007A5E", "Not FDR-supported" = "#7C8795")) +
    ggplot2::labs(
      x = NULL, y = "Rank-biserial correlation (AE yes minus AE no)",
      title = "Do AE groups change differently?",
      subtitle = "Positive values indicate relatively greater improvement in the AE-yes group",
      caption = "BH correction is applied across the nine planned change-contrast tests."
    ) +
    rq_theme()
}

rq_make_answers <- function(baseline_tests, composite_tests, prepost_tests, change_by_ae_tests) {
  anxiety <- baseline_tests |> dplyr::filter(item == 6) |> dplyr::slice_head(n = 1)
  wellbeing <- composite_tests |> dplyr::filter(stringr::str_detect(outcome, "wellbeing")) |> dplyr::slice_head(n = 1)
  supported_change <- prepost_tests |> dplyr::filter(!is.na(p_fdr), p_fdr < 0.05) |> dplyr::arrange(p_fdr)
  strongest_change <- prepost_tests |> dplyr::arrange(p_fdr) |> dplyr::slice_head(n = 1)
  supported_ae_change <- change_by_ae_tests |> dplyr::filter(!is.na(p_fdr), p_fdr < 0.05)
  strongest_ae_change <- change_by_ae_tests |> dplyr::arrange(p_value) |> dplyr::slice_head(n = 1)

  tibble::tibble(
    question_id = c("RQ1", "RQ2", "RQ3"),
    research_question = c(
      "Do children with anomalous experiences report different levels of anxiety and related distress at baseline?",
      "Did the Epicenter program improve wellbeing outcomes overall?",
      "Do children with anomalous experiences show different trajectories of change across the program?"
    ),
    conclusion = c(
      if (nrow(anxiety) && anxiety$p_fdr < 0.05) "The planned analysis detects an AE-group difference in baseline anxiety/fear." else "The planned analysis does not detect a statistically reliable AE-group difference in baseline anxiety/fear after FDR correction.",
      paste0("Observed scores improved on ", nrow(supported_change), " of 9 outcomes after BH correction, but the design cannot establish that the program caused those changes."),
      if (nrow(supported_ae_change) > 0) paste0(nrow(supported_ae_change), " outcome(s) show FDR-supported differences in change by AE status.") else "No outcome shows an FDR-supported difference in PRE-to-POST change between AE-yes and AE-no groups."
    ),
    primary_result = c(
      paste0(
        "Item 6: AE yes ", anxiety$median_iqr_yes, "; AE no ", anxiety$median_iqr_no,
        "; Mann-Whitney p ", rq_p_text(anxiety$p_value), ", BH q ", rq_p_text(anxiety$p_fdr),
        ", effect ", rq_effect_label(anxiety$rank_biserial_r), ". PRE wellbeing composite: p ",
        rq_p_text(wellbeing$p_value), ", effect ", rq_effect_label(wellbeing$rank_biserial_r), "."
      ),
      paste0(
        "FDR-supported improvement: ", if (nrow(supported_change)) paste(supported_change$label, collapse = "; ") else "none",
        ". Strongest corrected result: ", strongest_change$label, " (BH q ", rq_p_text(strongest_change$p_fdr),
        ", paired effect ", rq_effect_label(strongest_change$rank_biserial_r), ")."
      ),
      paste0(
        "The strongest unadjusted AE change contrast is ", strongest_ae_change$label,
        " (raw p ", rq_p_text(strongest_ae_change$p_value), ", BH q ", rq_p_text(strongest_ae_change$p_fdr),
        ", effect ", rq_effect_label(strongest_ae_change$rank_biserial_r), ")."
      )
    ),
    method = c(
      "AE-yes versus AE-no Mann-Whitney tests for nine PRE items; BH FDR across the nine-item family. Composite wellbeing and distress are separate prespecified single tests.",
      "Complete-pair, improvement-coded change scores; Wilcoxon signed-rank tests for nine items; BH FDR across the nine-item family.",
      "AE-yes versus AE-no Mann-Whitney tests on complete-pair, improvement-coded change scores; BH FDR across the nine-item family."
    ),
    interpretation_limit = c(
      "AE ascertainment was informal and may have occurred during the program. Item 6 is a single non-standardized anxiety/fear item, not a clinical anxiety diagnosis.",
      "There is no untreated control group, so maturation, regression to the mean, repeated measurement, and secular effects remain alternative explanations.",
      "AE was not randomized, the AE-unknown group is excluded from the primary contrast, and subgroup power is limited. Results describe association, not moderation caused by AE."
    )
  )
}

rq_plan_recommendations <- function() {
  tibble::tribble(
    ~priority, ~topic, ~recommendation, ~reason,
    "Critical", "Primary estimand", "Name one confirmatory primary endpoint and contrast, ideally PRE Item 6 AE-yes versus AE-no, with alpha and a fixed decision rule.", "The current plan calls Item 6 primary but also treats nine-item families and several composites as parallel analyses.",
    "Critical", "Causal language", "Define the PRE-to-POST analysis as observed within-person change, not program impact, unless a credible comparison group becomes available.", "Without a control group, change cannot be attributed uniquely to the program.",
    "Critical", "AE ascertainment", "Prespecify when and how AE is assessed, how '?' is resolved, who codes free text, and an inter-rater agreement procedure.", "AE was identified informally and may be discovered during participation, creating ascertainment and temporal ambiguity.",
    "Critical", "Data sharing", "Replace 'upload cleaned dataset' with an IRB-approved de-identified sharing specification and a disclosure-risk review.", "The dataset concerns children and contains potentially identifying free text; a cleaned participant-level file is not automatically safe for public release.",
    "High", "Multiplicity", "Define separate multiplicity families, which analyses are confirmatory versus exploratory, and whether BH uses alpha = .05.", "The plan names BH and Bonferroni but does not fully define the family boundaries or hierarchy.",
    "High", "Wilcoxon p-values", "Prespecify asymptotic or conditional-exact inference and the handling of ties and zero changes.", "The 1-10 items contain many ties; the usual exact Wilcoxon calculation is unavailable or method-dependent.",
    "High", "Composite score", "Specify the minimum number of completed items, report internal consistency, and justify the one-factor wellbeing interpretation.", "A nine-item average is easy to compute but its measurement validity is not established.",
    "High", "Missing data", "Add a missingness estimand, attrition comparison, and sensitivity strategy; state when multiple imputation would be justified.", "Complete-pair analyses can be biased when missingness relates to outcomes or AE status.",
    "High", "Age", "Use continuous age as the main age adjustment, assess non-linearity, and reserve broad age groups for description.", "Categorization loses information and the developmental range is wide.",
    "High", "Clustering", "Add program cohort, site, facilitator, and attendance variables if available and consider clustered or multilevel models.", "Participants may not be independent if they share program contexts.",
    "Medium", "Depression framing", "Remove or qualify 'depression' in the title and manuscript claims unless a validated depression measure is added.", "The questionnaire has no direct depression measure.",
    "Medium", "Unavailable variables", "Reconcile planned sex and POST-only program-evaluation fields with the analysis workbook before data lock.", "Sex and Items 10-14 are named in the plan but are not present in the supplied analysis sheet.",
    "Medium", "Sample accounting", "Resolve the plan's N = 190 against 186 imported records and document the four participant-number gaps in a CONSORT-style flow.", "Transparent denominators are required for every table and paired test."
  )
}
