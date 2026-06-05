# Cleaning helpers for workbook import, age recoding, AE coding, and item scores.

clean_numeric_score <- function(x) {
  x_chr <- stringr::str_trim(as.character(x))
  x_chr[x_chr %in% c("", "-", "NA", "N/A", "na", "n/a")] <- NA_character_
  readr::parse_number(x_chr, na = c("", "NA", "N/A"))
}

clean_age_value <- function(age_raw) {
  raw <- stringr::str_squish(stringr::str_to_lower(as.character(age_raw)))

  dplyr::case_when(
    is.na(raw) | raw == "" ~ NA_real_,
    stringr::str_detect(raw, "1st|first") ~ 6.5,
    stringr::str_detect(raw, "2nd|second") ~ 7.5,
    stringr::str_detect(raw, "4th|fourth") ~ 9.5,
    TRUE ~ readr::parse_number(raw)
  )
}

flag_uncertain_age <- function(age_raw) {
  raw <- stringr::str_squish(stringr::str_to_lower(as.character(age_raw)))
  !is.na(raw) & raw != "" & (
    stringr::str_detect(raw, "grade|\\?|cort|quarter|quarters") |
      is.na(readr::parse_number(raw))
  )
}

make_age_group <- function(age) {
  dplyr::case_when(
    is.na(age) ~ NA_character_,
    age >= 5 & age <= 8 ~ "younger_5_8",
    age >= 9 & age <= 11 ~ "middle_9_11",
    age >= 12 & age <= 15 ~ "older_12_15",
    TRUE ~ "outside_plan_range"
  )
}

clean_ae_status <- function(x) {
  raw <- stringr::str_squish(stringr::str_to_lower(as.character(x)))
  dplyr::case_when(
    raw %in% c("y", "yes") ~ "yes",
    raw %in% c("n", "no") ~ "no",
    raw %in% c("?", "unknown", "unsure", "uncertain") ~ "unknown",
    is.na(raw) | raw == "" ~ "unknown",
    TRUE ~ "unknown"
  )
}

code_ae_types <- function(type_raw) {
  txt <- stringr::str_to_lower(dplyr::coalesce(as.character(type_raw), ""))

  tibble::tibble(
    ae_entities_beings = stringr::str_detect(txt, "entit|angel|alien|spirit|fair|ghost|being"),
    ae_auditory = stringr::str_detect(txt, "hear|voice|auditory"),
    ae_visual = stringr::str_detect(txt, "see|saw|visual|aura|vision"),
    ae_deceased = stringr::str_detect(txt, "dead|deceased|passed|ancestor"),
    ae_precognitive_dream = stringr::str_detect(txt, "dream|precog|future|predict|premonition"),
    ae_cosmological_mystical = stringr::str_detect(txt, "universe|oneness|animis|cosmo|mystic|energy"),
    ae_other_ambiguous = txt != "" & !(
      ae_entities_beings | ae_auditory | ae_visual | ae_deceased |
        ae_precognitive_dream | ae_cosmological_mystical
    )
  )
}

code_verification_method <- function(verified_raw) {
  txt <- stringr::str_to_lower(dplyr::coalesce(as.character(verified_raw), ""))
  has_conversation <- stringr::str_detect(txt, "conversation|told|said|talk|verbal")
  has_artwork <- stringr::str_detect(txt, "art|artwork|painting|clay|draw|drawing")
  witnessed <- stringr::str_detect(txt, "witness|observed|staff saw|direct")

  dplyr::case_when(
    witnessed ~ "witnessed_directly_by_staff",
    has_conversation & has_artwork ~ "both_conversation_and_artwork",
    has_conversation ~ "conversation_only",
    has_artwork ~ "artwork_only",
    txt == "" ~ NA_character_,
    TRUE ~ "other_unclear"
  )
}

reverse_if_needed <- function(score, item) {
  dplyr::if_else(item %in% c(5L, 6L), 11 - score, score)
}

make_wide_from_long <- function(long_data, value_col = "score") {
  long_data |>
    dplyr::select(participant_id, timepoint, item, value = {{ value_col }}) |>
    tidyr::unite("item_time", timepoint, item, sep = "_item") |>
    tidyr::pivot_wider(names_from = item_time, values_from = value)
}

build_composites <- function(item_long) {
  item_long |>
    dplyr::filter(item <= 9) |>
    dplyr::mutate(
      score_wellbeing_direction = reverse_if_needed(score, item),
      distress_component = dplyr::if_else(item %in% c(5L, 6L), score, NA_real_)
    ) |>
    dplyr::group_by(participant_id, timepoint) |>
    dplyr::summarise(
      wellbeing_composite = mean(score_wellbeing_direction, na.rm = TRUE),
      wellbeing_n_items = sum(!is.na(score_wellbeing_direction)),
      distress_subscale = mean(distress_component, na.rm = TRUE),
      distress_n_items = sum(!is.na(distress_component)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      wellbeing_composite = dplyr::if_else(wellbeing_n_items == 0, NA_real_, wellbeing_composite),
      distress_subscale = dplyr::if_else(distress_n_items == 0, NA_real_, distress_subscale)
    )
}
