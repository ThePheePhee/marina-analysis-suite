# Metadata for the 9 paired PRE/POST items and the 5 POST-only program items.

paired_items <- tibble::tibble(
  item = 1:9,
  short_name = c(
    "confidence_self",
    "confidence_art",
    "confidence_social",
    "confidence_expression",
    "social_pressure",
    "anxiety_fear",
    "describe_feelings",
    "self_compassion",
    "creative"
  ),
  label = c(
    "Confidence in myself",
    "Confidence in art",
    "Confidence in social situations",
    "Confidence in self-expression",
    "I feel pressure to act differently in front of different people",
    "I'm often anxious and/or afraid",
    "How easy is it to describe how you feel?",
    "I care for/compliment myself as much as my friends",
    "I am creative"
  ),
  direction = c(
    "higher_better", "higher_better", "higher_better", "higher_better",
    "higher_worse", "higher_worse",
    "higher_better", "higher_better", "higher_better"
  ),
  primary_focus = item %in% c(5, 6)
)

post_only_items <- tibble::tibble(
  item = 10:14,
  short_name = c(
    "post_more_confident_after_class",
    "post_expressed_freely",
    "post_made_new_friend",
    "post_more_connected",
    "post_overall_class_rating"
  ),
  analysis_role = "program_evaluation_only"
)

age_group_levels <- c("younger_5_8", "middle_9_11", "older_12_15", "outside_plan_range")
ae_levels <- c("yes", "no", "unknown")

ae_type_columns <- c(
  "ae_entities_beings",
  "ae_auditory",
  "ae_visual",
  "ae_deceased",
  "ae_precognitive_dream",
  "ae_cosmological_mystical",
  "ae_other_ambiguous"
)
