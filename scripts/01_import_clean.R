# Import and clean the Excel workbook.
# This is deliberately explicit because the workbook contains descriptive header
# rows rather than a conventional one-row variable header.

source("scripts/00_setup.R")

raw_sheet <- suppressMessages(
  readxl::read_excel(
    raw_workbook_path,
    sheet = "before and after analysis",
    col_names = FALSE,
    .name_repair = "unique"
  )
)

# Workbook layout observed in the supplied file:
# row 1: BEFORE/AFTER labels over paired item columns
# row 2: participant fields, item labels, and AE metadata
# row 3 onward: participant rows
data_rows <- raw_sheet |>
  dplyr::slice(3:dplyr::n()) |>
  dplyr::select(
    participant_id = 1,
    participant_name = 2,
    age_raw = 3,
    pre_item_1 = 5,
    post_item_1 = 6,
    pre_item_2 = 7,
    post_item_2 = 8,
    pre_item_3 = 9,
    post_item_3 = 10,
    pre_item_4 = 11,
    post_item_4 = 12,
    pre_item_5 = 13,
    post_item_5 = 14,
    pre_item_6 = 15,
    post_item_6 = 16,
    pre_item_7 = 17,
    post_item_7 = 18,
    pre_item_8 = 19,
    post_item_8 = 20,
    pre_item_9 = 21,
    post_item_9 = 22,
    ae_raw = 23,
    ae_type_raw = 24,
    verified_by_raw = 25,
    valence_raw = 26
  ) |>
  dplyr::filter(!is.na(participant_id))

participant_clean_base <- data_rows |>
  dplyr::mutate(
    participant_id = as.integer(clean_numeric_score(participant_id)),
    age = clean_age_value(age_raw),
    age_uncertain_flag = flag_uncertain_age(age_raw),
    age_group = factor(make_age_group(age), levels = age_group_levels),
    ae_status = factor(clean_ae_status(ae_raw), levels = ae_levels),
    ae_status_primary = dplyr::if_else(ae_status == "unknown", NA_character_, as.character(ae_status)),
    ae_status_primary = factor(ae_status_primary, levels = c("yes", "no")),
    verification_method = code_verification_method(verified_by_raw)
  )

participant_clean <- participant_clean_base |>
  dplyr::bind_cols(code_ae_types(participant_clean_base$ae_type_raw)) |>
  dplyr::select(
    participant_id, participant_name, age_raw, age, age_uncertain_flag, age_group,
    ae_raw, ae_status, ae_status_primary, ae_type_raw, dplyr::all_of(ae_type_columns),
    verified_by_raw, verification_method, valence_raw
  )

item_long <- data_rows |>
  dplyr::mutate(participant_id = as.integer(clean_numeric_score(participant_id))) |>
  dplyr::select(participant_id, dplyr::matches("^(pre|post)_item_\\d+$")) |>
  tidyr::pivot_longer(
    cols = -participant_id,
    names_to = c("timepoint", "item"),
    names_pattern = "(pre|post)_item_(\\d+)",
    values_to = "score_raw"
  ) |>
  dplyr::mutate(
    item = as.integer(item),
    timepoint = factor(timepoint, levels = c("pre", "post")),
    score = clean_numeric_score(score_raw),
    score_out_of_range = !is.na(score) & (score < 1 | score > 10),
    non_integer_valid = !is.na(score) & score %% 1 != 0 & score >= 1 & score <= 10,
    # Per plan: participant 150, POST Item 8 = "-" is missing. The generic
    # score cleaner already treats "-" as NA; this flag makes the decision auditable.
    participant_150_post_item_8_dash = participant_id == 150 & timepoint == "post" & item == 8 &
      stringr::str_trim(as.character(score_raw)) == "-"
  ) |>
  dplyr::left_join(paired_items, by = "item")

item_wide <- item_long |>
  dplyr::select(participant_id, timepoint, item, score) |>
  tidyr::unite("measure", timepoint, item, sep = "_item") |>
  tidyr::pivot_wider(names_from = measure, values_from = score)

composites <- build_composites(item_long)

participant_analysis <- participant_clean |>
  dplyr::left_join(item_wide, by = "participant_id") |>
  dplyr::left_join(
    composites |>
      tidyr::pivot_wider(
        id_cols = participant_id,
        names_from = timepoint,
        values_from = c(wellbeing_composite, wellbeing_n_items, distress_subscale, distress_n_items)
      ),
    by = "participant_id"
  )

missingness_item <- item_long |>
  dplyr::group_by(timepoint, item, short_name, label) |>
  dplyr::summarise(
    n_available = sum(!is.na(score)),
    n_missing = sum(is.na(score)),
    pct_missing = n_missing / dplyr::n(),
    .groups = "drop"
  )

pre_post_completeness <- item_long |>
  dplyr::filter(item <= 9) |>
  dplyr::select(participant_id, timepoint, item, score) |>
  tidyr::pivot_wider(names_from = timepoint, values_from = score) |>
  dplyr::group_by(participant_id) |>
  dplyr::summarise(
    n_pre_available = sum(!is.na(pre)),
    n_post_available = sum(!is.na(post)),
    n_complete_pairs = sum(!is.na(pre) & !is.na(post)),
    completeness_group = dplyr::case_when(
      n_pre_available > 0 & n_post_available > 0 ~ "pre_and_post_available",
      n_pre_available > 0 & n_post_available == 0 ~ "pre_only",
      n_pre_available == 0 & n_post_available > 0 ~ "post_only",
      TRUE ~ "neither_pre_nor_post"
    ),
    .groups = "drop"
  )

row_number_gaps <- tibble::tibble(
  participant_id = seq(
    min(participant_clean$participant_id, na.rm = TRUE),
    max(participant_clean$participant_id, na.rm = TRUE)
  )
) |>
  dplyr::anti_join(
    participant_clean |> dplyr::select(participant_id),
    by = "participant_id"
  ) |>
  dplyr::mutate(
    review_needed = TRUE,
    review_question = "Confirm whether this missing participant number reflects withdrawal or a data-entry gap."
  )

cleaning_flags <- participant_analysis |>
  dplyr::transmute(
    participant_id,
    age_raw,
    age,
    age_uncertain_flag,
    age_outside_plan_range = age_group == "outside_plan_range",
    ae_raw,
    ae_status,
    ae_unknown_flag = ae_status == "unknown",
    ae_type_raw,
    verified_by_raw,
    verification_method
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    review_priority = "Decision required",
    why_review_needed = paste(c(
      if (isTRUE(age_uncertain_flag)) "Age was supplied as a grade, an approximate value, or an otherwise ambiguous entry." else NULL,
      if (isTRUE(age_outside_plan_range)) "Parsed age falls outside the analysis-plan range of 5 to 15 years." else NULL,
      if (isTRUE(ae_unknown_flag)) "AE status was blank, '?', or another value that could not be classified as yes or no." else NULL,
      if (isTRUE(verification_method == "other_unclear")) "Verification text did not match a prespecified verification category." else NULL
    ), collapse = " "),
    action_required = paste(c(
      if (isTRUE(age_uncertain_flag)) "Confirm the intended numeric age; amend the source value or document the recoding decision." else NULL,
      if (isTRUE(age_outside_plan_range)) "Confirm eligibility for the planned 5-15-year analytic range." else NULL,
      if (isTRUE(ae_unknown_flag)) "Resolve the intended AE status as yes, no, or genuinely unknown where source information permits." else NULL,
      if (isTRUE(verification_method == "other_unclear")) "Classify the verification route, or confirm that it should remain uncategorised." else NULL
    ), collapse = " "),
    current_automated_handling = paste(c(
      if (isTRUE(age_uncertain_flag)) "Retained the parsed or grade-midpoint age and flagged it." else NULL,
      if (isTRUE(age_outside_plan_range)) "Retained the participant and labelled the age group outside_plan_range." else NULL,
      if (isTRUE(ae_unknown_flag)) "Kept AE status as unknown; excluded it from the primary AE yes/no comparison and retained it for sensitivity analyses." else NULL,
      if (isTRUE(verification_method == "other_unclear")) "Kept the raw text and assigned verification_method = other_unclear." else NULL
    ), collapse = " "),
    affected_analysis = paste(c(
      if (isTRUE(age_uncertain_flag) || isTRUE(age_outside_plan_range)) "Age-adjusted and age-interaction models." else NULL,
      if (isTRUE(ae_unknown_flag)) "Primary AE comparisons, AE atlas, and AE sensitivity analyses." else NULL,
      if (isTRUE(verification_method == "other_unclear")) "Verification-route exploratory analyses." else NULL
    ), collapse = " ")
  ) |>
  dplyr::ungroup()

item_cleaning_flags <- item_long |>
  dplyr::filter(score_out_of_range | participant_150_post_item_8_dash) |>
  dplyr::transmute(
    participant_id,
    timepoint,
    item,
    label,
    score_raw,
    score,
    review_priority = "Confirm source entry",
    why_review_needed = dplyr::case_when(
      score_out_of_range ~ "The numeric score is outside the valid 1-10 response range.",
      participant_150_post_item_8_dash ~ "The source contains a dash rather than a POST Item 8 score.",
      TRUE ~ "The item requires source verification."
    ),
    action_required = dplyr::case_when(
      score_out_of_range ~ "Confirm the intended score and correct the source record if it is a transcription error.",
      participant_150_post_item_8_dash ~ "Verify that the dash denotes intentional missingness; if it does, no further change is needed.",
      TRUE ~ "Verify the source entry."
    ),
    current_automated_handling = dplyr::case_when(
      score_out_of_range ~ "Retained the value but marked score_out_of_range = TRUE.",
      participant_150_post_item_8_dash ~ "Converted the dash to missing (NA), as specified in the analysis plan.",
      TRUE ~ "No automated correction applied."
    ),
    affected_analysis = "Item-level and paired PRE-to-POST analyses involving this item."
  )

write_csv_safe(participant_analysis, "data/processed/participant_analysis.csv")
write_csv_safe(item_long, "data/processed/item_long.csv")
write_csv_safe(composites, "data/processed/composites.csv")
write_csv_safe(missingness_item, "outputs/tables/missingness_by_item.csv")
write_csv_safe(pre_post_completeness, "outputs/tables/pre_post_completeness.csv")
write_csv_safe(row_number_gaps, "outputs/tables/missing_participant_ids_for_review.csv")
write_csv_safe(cleaning_flags, "outputs/tables/cleaning_flags_for_review.csv")
write_csv_safe(item_cleaning_flags, "outputs/tables/item_cleaning_flags_for_review.csv")

message("Cleaned participant data written to data/processed/participant_analysis.csv")
message("Human-review flags written to outputs/tables/cleaning_flags_for_review.csv")
message("Item-level review flags written to outputs/tables/item_cleaning_flags_for_review.csv")
