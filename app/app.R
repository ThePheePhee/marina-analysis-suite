# Shiny dashboard for the Marina analysis suite.
# Run from the project root with: shiny::runApp("app")

cwd <- normalizePath(getwd(), mustWork = TRUE)
project_root <- if (file.exists(file.path(cwd, "data", "processed", "participant_analysis.csv"))) {
  cwd
} else {
  normalizePath(file.path(cwd, ".."), mustWork = TRUE)
}

if (dir.exists(file.path(project_root, "r-lib"))) {
  .libPaths(c(normalizePath(file.path(project_root, "r-lib")), .libPaths()))
}

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)

read_optional_csv <- function(path) {
  if (!file.exists(path)) {
    return(tibble())
  }
  readr::read_csv(path, show_col_types = FALSE)
}

project_file <- function(...) {
  file.path(project_root, ...)
}

participant_path <- project_file("data", "processed", "participant_analysis.csv")
item_path <- project_file("data", "processed", "item_long.csv")
change_path <- project_file("data", "processed", "change_scores.csv")

participants <- read_optional_csv(participant_path)
item_long <- read_optional_csv(item_path)
change_scores <- read_optional_csv(change_path)

baseline_tests <- read_optional_csv(project_file("outputs", "tables", "table_2_pre_item_tests_by_ae.csv"))
prepost_tests <- read_optional_csv(project_file("outputs", "tables", "table_3_prepost_full_sample_tests.csv"))
change_by_ae_tests <- read_optional_csv(project_file("outputs", "tables", "table_4_prepost_change_by_ae_tests.csv"))
cleaning_flags <- read_optional_csv(project_file("outputs", "tables", "cleaning_flags_for_review.csv"))
missing_id_flags <- read_optional_csv(project_file("outputs", "tables", "missing_participant_ids_for_review.csv"))
item_cleaning_flags <- read_optional_csv(project_file("outputs", "tables", "item_cleaning_flags_for_review.csv"))
ae_type_freq <- read_optional_csv(project_file("outputs", "tables", "ae_type_frequencies.csv"))
verification_freq <- read_optional_csv(project_file("outputs", "tables", "verification_method_frequencies.csv"))
sensitivity_baseline <- read_optional_csv(project_file("outputs", "tables", "sensitivity_baseline_pre_items_unknown_recode.csv"))
sensitivity_change <- read_optional_csv(project_file("outputs", "tables", "sensitivity_prepost_change_unknown_recode.csv"))
key_differences <- read_optional_csv(project_file("outputs", "tables", "key_ae_differences_ranked.csv"))
second_order_changes <- read_optional_csv(project_file("outputs", "tables", "second_order_change_differences.csv"))
key_fields_controls <- read_optional_csv(project_file("outputs", "tables", "key_difference_fields_and_controls.csv"))
observation_age_effects <- read_optional_csv(project_file("outputs", "tables", "observation_age_change_effects.csv"))
observation_age_ae <- read_optional_csv(project_file("outputs", "tables", "observation_age_ae_interactions.csv"))
observation_baseline_ae <- read_optional_csv(project_file("outputs", "tables", "observation_baseline_ae_interactions.csv"))
observation_ae_types <- read_optional_csv(project_file("outputs", "tables", "observation_ae_type_change_models.csv"))
observation_verification <- read_optional_csv(project_file("outputs", "tables", "observation_verification_change_models.csv"))
observation_scan <- read_optional_csv(project_file("outputs", "tables", "observation_exploratory_scan.csv"))
observation_ae_age <- read_optional_csv(project_file("outputs", "tables", "observation_ae_age_prevalence.csv"))
ae_atlas_observations <- read_optional_csv(project_file("outputs", "tables", "ae_atlas_observations.csv"))
ae_atlas_inventory <- read_optional_csv(project_file("outputs", "tables", "ae_atlas_inventory.csv"))
ae_atlas_global <- read_optional_csv(project_file("outputs", "tables", "ae_atlas_global_tests.csv"))
overall_signals <- read_optional_csv(project_file("outputs", "tables", "overall_signal_observations.csv"))

has_data <- nrow(participants) > 0 && nrow(item_long) > 0

empty_panel <- function() {
  div(
    class = "empty-state",
    h3("Analysis outputs not found"),
    p("Run the numbered scripts from the project root first. At minimum run scripts/01_import_clean.R.")
  )
}

item_choices <- if (nrow(item_long) > 0) {
  item_lookup <- item_long |>
    filter(item <= 9) |>
    distinct(item, label) |>
    arrange(item) |>
    mutate(choice = paste0("Item ", item, ": ", label))
  stats::setNames(item_lookup$item, item_lookup$choice)
} else {
  c("Item 6: anxiety/fear" = 6)
}

atlas_section_choices <- if (nrow(ae_atlas_observations) > 0) {
  sections <- unique(ae_atlas_observations$section)
  c("All sections" = "__all__", stats::setNames(sections, sections))
} else {
  c("All sections" = "__all__")
}

atlas_evidence_choices <- if (nrow(ae_atlas_observations) > 0) {
  levels <- unique(ae_atlas_observations$evidence)
  c("All evidence levels" = "__all__", stats::setNames(levels, levels))
} else {
  c("All evidence levels" = "__all__")
}

disclosure_list <- function(data, show_evidence = TRUE) {
  if (nrow(data) == 0) return(p("No observations match these filters."))

  tagList(lapply(seq_len(nrow(data)), function(i) {
    row <- data[i, , drop = FALSE]
    value_or <- function(name, fallback = "Not applicable") {
      if (!name %in% names(row) || is.na(row[[name]][1]) || row[[name]][1] == "") fallback else as.character(row[[name]][1])
    }
    evidence <- value_or("evidence", "Overall synthesis")
    tags$details(
      class = "analysis-disclosure",
      tags$summary(
        tags$div(
          class = "disclosure-summary",
          tags$div(class = "disclosure-title", value_or("title")),
          if (show_evidence) tags$span(class = "evidence-label", evidence),
          tags$div(class = "disclosure-description", value_or("description"))
        )
      ),
      tags$div(
        class = "disclosure-body",
        tags$h5("Calculation and result"),
        tags$p(value_or("calculation")),
        tags$h5("Method"),
        tags$p(value_or("method")),
        tags$h5("Controlled for"),
        tags$p(value_or("controls", "No covariates; descriptive or unadjusted result.")),
        tags$h5("Related data"),
        tags$p(value_or("related_data")),
        tags$h5("Interpretation limits"),
        tags$p(value_or("caveat", "Standard observational-data limitations apply."))
      )
    )
  }))
}

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Marina Analysis Suite"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-pie")),
      menuItem("Cleaning", tabName = "cleaning", icon = icon("broom")),
      menuItem("Distributions", tabName = "distributions", icon = icon("chart-area")),
      menuItem("Baseline AE Tests", tabName = "baseline", icon = icon("table")),
      menuItem("Pre-Post Change", tabName = "prepost", icon = icon("arrows-rotate")),
      menuItem("Key Differences", tabName = "keydiff", icon = icon("magnifying-glass-chart")),
      menuItem("AE Relationship Atlas", tabName = "aeatlas", icon = icon("diagram-project")),
      menuItem("Overall Signal", tabName = "overall", icon = icon("signal")),
      menuItem("Observations", tabName = "observations", icon = icon("lightbulb")),
      menuItem("AE Types", tabName = "types", icon = icon("shapes")),
      menuItem("Sensitivity", tabName = "sensitivity", icon = icon("sliders"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background: #f6f7f9; }
      .small-box { border-radius: 6px; }
      .box { border-radius: 6px; border-top-width: 2px; }
      .empty-state { padding: 28px; color: #555; }
      .dataTables_wrapper { font-size: 13px; }
      .interpretation-card {
        border: 1px solid #d8dee8;
        border-radius: 6px;
        padding: 12px 13px;
        margin-bottom: 12px;
        background: #ffffff;
      }
      .interpretation-card h4 {
        margin: 0 0 5px;
        font-weight: 700;
      }
      .interpretation-meta {
        color: #64748b;
        font-size: 12px;
        margin-bottom: 6px;
      }
      .change-detail {
        border-left: 3px solid #2a7f9e;
        padding-left: 10px;
        margin: 8px 0;
      }
      .evidence-label {
        display: inline-block;
        padding: 2px 7px;
        border-radius: 4px;
        background: #eef2f7;
        color: #344054;
        font-size: 12px;
        font-weight: 700;
        margin-bottom: 7px;
      }
      .analysis-disclosure {
        border-top: 1px solid #d7dee7;
        background: #ffffff;
      }
      .analysis-disclosure:last-child { border-bottom: 1px solid #d7dee7; }
      .analysis-disclosure summary {
        cursor: pointer;
        list-style-position: inside;
        padding: 13px 14px;
      }
      .analysis-disclosure summary:hover { background: #f7f9fb; }
      .disclosure-summary { display: inline-block; width: calc(100% - 24px); vertical-align: top; }
      .disclosure-title { font-size: 15px; font-weight: 700; color: #1f2937; margin-bottom: 5px; }
      .disclosure-description { color: #475467; line-height: 1.45; }
      .disclosure-body {
        padding: 4px 18px 16px 38px;
        border-left: 3px solid #2a7f9e;
        margin: 0 14px 14px;
      }
      .disclosure-body h5 { font-weight: 700; margin: 10px 0 3px; }
      .disclosure-body p { margin: 0 0 6px; }
      .atlas-filter-note { color: #667085; font-size: 12px; margin-top: 8px; }
      .cleaning-note {
        border-left: 3px solid #d17b00;
        background: #fffaf0;
        padding: 11px 13px;
        margin-bottom: 12px;
        color: #5b4214;
      }
      .cleaning-summary {
        display: inline-block;
        margin: 0 8px 8px 0;
        padding: 7px 10px;
        background: #fff3e0;
        border: 1px solid #f2c27c;
        border-radius: 4px;
        font-weight: 600;
      }
    "))),
    tabItems(
      tabItem(
        tabName = "overview",
        if (!has_data) empty_panel() else fluidRow(
          valueBoxOutput("n_participants", width = 3),
          valueBoxOutput("ae_yes", width = 3),
          valueBoxOutput("ae_unknown", width = 3),
          valueBoxOutput("age_flags", width = 3),
          box(width = 6, title = "AE Status", status = "primary", solidHeader = TRUE, plotlyOutput("ae_status_plot")),
          box(width = 6, title = "Age Group", status = "primary", solidHeader = TRUE, plotlyOutput("age_group_plot")),
          box(width = 12, title = "Sample Characteristics", status = "primary", DTOutput("sample_table"))
        )
      ),
      tabItem(
        tabName = "cleaning",
        if (nrow(cleaning_flags) == 0) empty_panel() else fluidRow(
          box(
            width = 12,
            title = "What Needs a Decision",
            status = "warning",
            solidHeader = TRUE,
            tags$div(
              class = "cleaning-note",
              "Each row below states why it was flagged, the exact source decision needed, and what the pipeline currently does until that decision is recorded. The original workbook is never overwritten."
            ),
            uiOutput("cleaning_action_summary")
          ),
          box(width = 12, title = "Participant Rows Requiring Review", status = "warning", solidHeader = TRUE, DTOutput("cleaning_table")),
          box(width = 6, title = "Missing Participant-Number Gaps", status = "warning", solidHeader = TRUE, DTOutput("missing_id_table")),
          box(width = 6, title = "Item-Level Source Checks", status = "warning", solidHeader = TRUE, DTOutput("item_cleaning_table")),
          box(
            width = 12,
            title = "Where Cleaned Data Are Saved",
            status = "primary",
            solidHeader = TRUE,
            p("The pipeline writes regenerated local analysis files to data/processed/participant_analysis.csv, data/processed/item_long.csv, and data/processed/composites.csv."),
            p("These files are excluded from Git and the public static dashboard. They are overwritten whenever scripts/01_import_clean.R or scripts/run_all.R is run; make corrections in the source data or a documented correction step, then rerun the pipeline.")
          )
        )
      ),
      tabItem(
        tabName = "distributions",
        if (!has_data) empty_panel() else fluidRow(
          box(
            width = 4,
            title = "Controls",
            status = "primary",
            selectInput("dist_item", "Item", choices = item_choices, selected = 6),
            selectInput("dist_timepoint", "Timepoint", choices = c("pre", "post"), selected = "pre")
          ),
          box(width = 8, title = "Scores by AE Group", status = "primary", solidHeader = TRUE, plotlyOutput("item_distribution")),
          box(width = 12, title = "Item-Level Data", status = "primary", DTOutput("item_distribution_table"))
        )
      ),
      tabItem(
        tabName = "baseline",
        if (nrow(baseline_tests) == 0) empty_panel() else fluidRow(
          box(width = 12, title = "PRE Item Tests: AE Yes vs. AE No", status = "primary", solidHeader = TRUE, DTOutput("baseline_table"))
        )
      ),
      tabItem(
        tabName = "prepost",
        if (nrow(prepost_tests) == 0) empty_panel() else fluidRow(
          box(width = 6, title = "Full-Sample PRE to POST Tests", status = "primary", solidHeader = TRUE, DTOutput("prepost_table")),
          box(width = 6, title = "Change by AE Group", status = "primary", solidHeader = TRUE, DTOutput("change_by_ae_table")),
          box(width = 12, title = "Change Scores by Item", status = "primary", plotlyOutput("change_plot"))
        )
      ),
      tabItem(
        tabName = "keydiff",
        if (nrow(key_differences) == 0) empty_panel() else fluidRow(
          box(
            width = 12,
            title = "How to Read This Tab",
            status = "primary",
            solidHeader = TRUE,
            p("This tab emphasizes second-order differences: outcomes where AE-yes and AE-no participants changed differently from PRE to POST."),
            p("Change scores are improvement-coded, so positive values mean improvement. The second-order contrast is the AE-yes mean change minus the AE-no mean change."),
            p("Unadjusted group comparisons use Mann-Whitney/rank-biserial effect sizes. Exploratory adjusted models control for age and baseline score."),
            p("Interpret all findings as observational associations, not causal effects. AE status was identified informally, and several possible confounders were not available in the workbook.")
          ),
          box(width = 7, title = "Second-Order Changes: Which Outcomes Changed Differently?", status = "primary", solidHeader = TRUE, plotlyOutput("second_order_plot")),
          box(width = 5, title = "Top Change-Contrast Interpretations", status = "primary", solidHeader = TRUE, uiOutput("second_order_interpretations")),
          box(width = 12, title = "Second-Order Change Contrast Table", status = "primary", solidHeader = TRUE, DTOutput("second_order_table")),
          box(width = 7, title = "All AE vs Non-AE Differences", status = "primary", solidHeader = TRUE, plotlyOutput("key_difference_plot")),
          box(width = 5, title = "Top Overall Interpretations", status = "primary", solidHeader = TRUE, uiOutput("top_difference_interpretations")),
          box(width = 12, title = "Fields Analyzed and Controls Used", status = "primary", solidHeader = TRUE, DTOutput("fields_controls_table")),
          box(width = 12, title = "Ranked Difference Table", status = "primary", solidHeader = TRUE, DTOutput("key_difference_table"))
        )
      ),
      tabItem(
        tabName = "aeatlas",
        if (nrow(ae_atlas_observations) == 0) empty_panel() else fluidRow(
          box(
            width = 12,
            title = "What This Atlas Answers",
            status = "primary",
            solidHeader = TRUE,
            p("This tab collects every defensible analysis of how AE status, AE valence, AE subtype, and verification route relate to baseline responses, adjusted POST outcomes, observed PRE-to-POST change, improvement probability, age, missingness, and the joint nine-item response profile."),
            p("Click any observation to open the calculation, model formula, controls, related data, and interpretation limits. Family FDR is BH correction within a coherent analysis family; atlas FDR is BH correction across every estimable inferential row on this tab."),
            p("AE cannot be treated as a randomized exposure here. The atlas describes relationships and possible moderation; it does not prove that AE experiences caused any outcome.")
          ),
          box(width = 12, title = "Global AE Checks", status = "primary", solidHeader = TRUE, uiOutput("ae_atlas_global_cards")),
          box(
            width = 3,
            title = "Filter Observations",
            status = "primary",
            selectInput("atlas_section", "Section", choices = atlas_section_choices),
            selectInput("atlas_evidence", "Evidence", choices = atlas_evidence_choices),
            textInput("atlas_search", "Search", placeholder = "e.g. anxiety, valence"),
            tags$div(class = "atlas-filter-note", textOutput("atlas_result_count"))
          ),
          box(width = 9, title = "AE Outcome Profile", status = "primary", solidHeader = TRUE, plotlyOutput("ae_atlas_profile_plot")),
          box(width = 12, title = "Every AE-Related Observation", status = "primary", solidHeader = TRUE, uiOutput("ae_atlas_accordion")),
          box(width = 12, title = "Complete Calculation Inventory", status = "primary", solidHeader = TRUE, DTOutput("ae_atlas_inventory_table"))
        )
      ),
      tabItem(
        tabName = "overall",
        if (nrow(overall_signals) == 0) empty_panel() else fluidRow(
          box(
            width = 12,
            title = "Is There Anything Interesting Here?",
            status = "primary",
            solidHeader = TRUE,
            p("Yes, but the strongest signal is not a clean AE-specific intervention effect. The sample improves on several outcomes overall, age is consistently related to the size of improvement, and AE valence has an intriguing baseline relationship with anxiety and confidence. AE yes versus no differences in change remain exploratory and uncertain."),
            p("The entries below are ordered by what I would present first. Click each statement for the supporting calculation, method, related evidence, and limitations.")
          ),
          box(
            width = 7,
            title = "Overall PRE-to-POST Signal",
            status = "primary",
            solidHeader = TRUE,
            tags$div(class = "atlas-filter-note", "Green = item-family FDR < .05; gray = not FDR-supported."),
            plotlyOutput("overall_change_plot")
          ),
          box(width = 5, title = "Evidence Summary", status = "primary", solidHeader = TRUE, plotlyOutput("overall_evidence_plot")),
          box(width = 12, title = "What the Data Support", status = "primary", solidHeader = TRUE, uiOutput("overall_signal_cards"))
        )
      ),
      tabItem(
        tabName = "observations",
        if (nrow(observation_scan) == 0) empty_panel() else fluidRow(
          box(
            width = 12,
            title = "Scope and Statistical Guardrails",
            status = "primary",
            solidHeader = TRUE,
            p("This is a broad exploratory scan of observed PRE-to-POST change. It is not an estimate of causal intervention impact because the dataset has no untreated control group."),
            p("Models examine age, AE-by-age interactions, AE-by-baseline interactions, AE types, and verification methods. Change is improvement-coded and models adjust for baseline score; age or AE status is also included where appropriate."),
            p("Raw p-values are shown to surface hypotheses. Family FDR corrects within each model family; global FDR corrects across the entire exploratory scan. Treat a raw p < .05 without FDR support as a lead for future study, not a finding."),
            p("Baseline-by-AE interactions deserve extra caution because the change score itself contains baseline; regression-to-the-mean and ceiling effects can create or distort these patterns.")
          ),
          box(width = 12, title = "Most Interesting Observations", status = "primary", solidHeader = TRUE, uiOutput("observation_highlights")),
          box(width = 7, title = "Age Effects on Observed Change", status = "primary", solidHeader = TRUE, plotlyOutput("observation_age_plot")),
          box(width = 5, title = "Age and AE Context", status = "primary", solidHeader = TRUE, uiOutput("observation_age_context")),
          box(width = 12, title = "Exploratory Signal Scanner", status = "primary", solidHeader = TRUE, DTOutput("observation_scan_table")),
          box(width = 6, title = "AE Interactions with Age and Baseline", status = "primary", solidHeader = TRUE, DTOutput("observation_interaction_table")),
          box(width = 6, title = "AE Type and Verification Patterns", status = "primary", solidHeader = TRUE, DTOutput("observation_context_table"))
        )
      ),
      tabItem(
        tabName = "types",
        if (nrow(ae_type_freq) == 0 && nrow(verification_freq) == 0) empty_panel() else fluidRow(
          box(width = 7, title = "AE Type Frequencies", status = "primary", solidHeader = TRUE, plotlyOutput("ae_type_plot")),
          box(width = 5, title = "Verification Method Frequencies", status = "primary", solidHeader = TRUE, plotlyOutput("verification_plot")),
          box(width = 12, title = "AE Type Table", status = "primary", DTOutput("ae_type_table"))
        )
      ),
      tabItem(
        tabName = "sensitivity",
        if (nrow(sensitivity_baseline) == 0) empty_panel() else fluidRow(
          box(width = 12, title = "Baseline Item Sensitivity: Unknown AE Recode", status = "primary", solidHeader = TRUE, DTOutput("sensitivity_baseline_table")),
          box(width = 12, title = "Pre-Post Change Sensitivity: Unknown AE Recode", status = "primary", solidHeader = TRUE, DTOutput("sensitivity_change_table"))
        )
      )
    )
  )
)

server <- function(input, output) {
  output$n_participants <- renderValueBox({
    valueBox(nrow(participants), "Participants", icon = icon("users"), color = "aqua")
  })

  output$ae_yes <- renderValueBox({
    valueBox(sum(participants$ae_status == "yes", na.rm = TRUE), "AE yes", icon = icon("circle-check"), color = "blue")
  })

  output$ae_unknown <- renderValueBox({
    valueBox(sum(participants$ae_status == "unknown", na.rm = TRUE), "AE unknown", icon = icon("circle-question"), color = "yellow")
  })

  output$age_flags <- renderValueBox({
    valueBox(sum(participants$age_uncertain_flag, na.rm = TRUE), "Age flags", icon = icon("triangle-exclamation"), color = "orange")
  })

  output$ae_status_plot <- renderPlotly({
    p <- participants |>
      count(ae_status) |>
      ggplot(aes(x = ae_status, y = n, fill = ae_status)) +
      geom_col(show.legend = FALSE) +
      labs(x = "AE status", y = "Count") +
      theme_minimal(base_size = 12)
    ggplotly(p)
  })

  output$age_group_plot <- renderPlotly({
    p <- participants |>
      count(age_group) |>
      ggplot(aes(x = age_group, y = n, fill = age_group)) +
      geom_col(show.legend = FALSE) +
      labs(x = "Age group", y = "Count") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 25, hjust = 1))
    ggplotly(p)
  })

  output$sample_table <- renderDT({
    participants |>
      group_by(ae_status) |>
      summarise(
        n = n(),
        age_mean = mean(age, na.rm = TRUE),
        age_sd = sd(age, na.rm = TRUE),
        age_min = min(age, na.rm = TRUE),
        age_max = max(age, na.rm = TRUE),
        age_uncertain_n = sum(age_uncertain_flag, na.rm = TRUE),
        .groups = "drop"
      ) |>
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })

  output$cleaning_table <- renderDT({
    review_rows <- cleaning_flags |>
      filter(age_uncertain_flag | age_outside_plan_range | ae_unknown_flag | verification_method == "other_unclear") |>
      select(
        review_priority, participant_id, why_review_needed, action_required,
        current_automated_handling, affected_analysis, age_raw, age, ae_raw,
        ae_status, ae_type_raw, verified_by_raw, verification_method
      )

    datatable(review_rows, filter = "top", options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE) |>
      formatStyle("review_priority", backgroundColor = "#fff3e0", color = "#8a4b00", fontWeight = "700") |>
      formatStyle("action_required", backgroundColor = "#fffaf0", fontWeight = "600")
  })

  output$cleaning_action_summary <- renderUI({
    flagged_rows <- cleaning_flags |>
      filter(age_uncertain_flag | age_outside_plan_range | ae_unknown_flag | verification_method == "other_unclear")

    tagList(
      tags$span(class = "cleaning-summary", paste(nrow(flagged_rows), "participant rows require a decision")),
      tags$span(class = "cleaning-summary", paste(sum(flagged_rows$ae_unknown_flag, na.rm = TRUE), "ambiguous AE-status entries")),
      tags$span(class = "cleaning-summary", paste(sum(flagged_rows$age_uncertain_flag, na.rm = TRUE), "uncertain age entries")),
      tags$span(class = "cleaning-summary", paste(sum(flagged_rows$age_outside_plan_range, na.rm = TRUE), "age outside planned range")),
      tags$span(class = "cleaning-summary", paste(nrow(missing_id_flags), "participant-number gaps"))
    )
  })

  output$missing_id_table <- renderDT({
    missing_id_flags |>
      mutate(
        review_priority = "Confirm source record",
        action_required = "Confirm whether this number represents withdrawal, an intentional omission, or a missing data row.",
        current_automated_handling = "No participant row is created; the gap is documented only.",
        affected_analysis = "Sample-size accounting and any attempt to link records by participant number."
      ) |>
      select(review_priority, participant_id, review_question, action_required, current_automated_handling, affected_analysis) |>
      datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) |>
      formatStyle("review_priority", backgroundColor = "#fff3e0", color = "#8a4b00", fontWeight = "700") |>
      formatStyle("action_required", backgroundColor = "#fffaf0", fontWeight = "600")
  })

  output$item_cleaning_table <- renderDT({
    item_cleaning_flags |>
      datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) |>
      formatStyle("review_priority", backgroundColor = "#fff3e0", color = "#8a4b00", fontWeight = "700") |>
      formatStyle("action_required", backgroundColor = "#fffaf0", fontWeight = "600")
  })

  selected_distribution_data <- reactive({
    item_long |>
      left_join(participants |> select(participant_id, ae_status_primary), by = "participant_id") |>
      filter(item == as.integer(input$dist_item), timepoint == input$dist_timepoint)
  })

  output$item_distribution <- renderPlotly({
    p <- selected_distribution_data() |>
      filter(ae_status_primary %in% c("yes", "no")) |>
      ggplot(aes(x = ae_status_primary, y = score, fill = ae_status_primary)) +
      geom_violin(trim = FALSE, alpha = 0.55, color = NA) +
      geom_boxplot(width = 0.15, outlier.shape = NA) +
      geom_jitter(width = 0.10, alpha = 0.6) +
      labs(x = "AE group", y = "Score") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
    ggplotly(p)
  })

  output$item_distribution_table <- renderDT({
    selected_distribution_data() |>
      select(participant_id, ae_status_primary, item, label, timepoint, score) |>
      datatable(filter = "top", options = list(pageLength = 20), rownames = FALSE)
  })

  output$baseline_table <- renderDT({
    baseline_tests |> datatable(filter = "top", options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$prepost_table <- renderDT({
    prepost_tests |> datatable(filter = "top", options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$change_by_ae_table <- renderDT({
    change_by_ae_tests |> datatable(filter = "top", options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$change_plot <- renderPlotly({
    validate(need(nrow(change_scores) > 0, "Run scripts/04_prepost_change.R first."))
    p <- change_scores |>
      filter(!is.na(change)) |>
      ggplot(aes(x = factor(item), y = change)) +
      geom_hline(yintercept = 0, color = "#777777") +
      geom_boxplot(fill = "#74a9cf", alpha = 0.75) +
      labs(x = "Item", y = "Change, scored so positive = improvement") +
      theme_minimal(base_size = 12)
    ggplotly(p)
  })

  output$key_difference_plot <- renderPlotly({
    validate(need(nrow(key_differences) > 0, "Run scripts/07_key_differences.R first."))

    plot_data <- key_differences |>
      arrange(desc(absolute_effect_size)) |>
      slice_head(n = 12) |>
      mutate(
        short_field = field_analyzed |>
          stringr::str_replace("^Change Item ", "Item ") |>
          stringr::str_trunc(width = 38),
        direction_label = case_when(
          median_difference > 0 ~ "AE yes higher",
          median_difference < 0 ~ "AE yes lower",
          TRUE ~ "same median"
        )
      )

    p <- plot_data |>
      ggplot(aes(
        x = reorder(short_field, absolute_effect_size),
        y = absolute_effect_size,
        fill = direction_label,
        text = paste0(
          field_analyzed,
          "<br>Family: ", family,
          "<br>Median AE yes: ", round(median_yes, 2),
          "<br>Median AE no: ", round(median_no, 2),
          "<br>Rank-biserial r: ", round(rank_biserial_r, 3),
          "<br>FDR p: ", fdr_p_text,
          "<br>Controls: ", controls
        )
      )) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("AE yes higher" = "#1f77b4", "AE yes lower" = "#e76f51", "same median" = "#8c8c8c")) +
      labs(x = NULL, y = "|rank-biserial r|", fill = NULL) +
      theme_minimal(base_size = 12)

    ggplotly(p, tooltip = "text") |>
      plotly::layout(margin = list(l = 205, r = 10, b = 60, t = 25))
  })

  output$second_order_plot <- renderPlotly({
    validate(need(nrow(second_order_changes) > 0, "Run scripts/07_key_differences.R first."))

    plot_data <- second_order_changes |>
      arrange(desc(absolute_effect_size)) |>
      slice_head(n = 9) |>
      mutate(
        short_field = field_analyzed |>
          stringr::str_replace("^Change Item ", "Item ") |>
          stringr::str_trunc(width = 38),
        contrast_direction = case_when(
          change_contrast > 0 ~ "AE yes improved more",
          change_contrast < 0 ~ "AE yes improved less",
          TRUE ~ "Same mean change"
        )
      )

    p <- plot_data |>
      ggplot(aes(
        x = reorder(short_field, change_contrast),
        y = change_contrast,
        fill = contrast_direction,
        text = paste0(
          field_analyzed,
          "<br>Mean change AE yes: ", round(mean_yes, 2),
          "<br>Mean change AE no: ", round(mean_no, 2),
          "<br>Original answer AE yes: ", round(original_pre_mean_yes, 2), " -> ", round(original_post_mean_yes, 2),
          "<br>Original answer AE no: ", round(original_pre_mean_no, 2), " -> ", round(original_post_mean_no, 2),
          "<br>", scale_note,
          "<br>Difference-in-change: ", round(change_contrast, 2),
          "<br>Rank-biserial r: ", round(rank_biserial_r, 3),
          "<br>FDR p: ", fdr_p_text,
          "<br>Adjusted estimate: ", round(adjusted_estimate, 3),
          "<br>Adjusted p: ", adjusted_p_text
        )
      )) +
      geom_hline(yintercept = 0, color = "#777777") +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("AE yes improved more" = "#1f77b4", "AE yes improved less" = "#e76f51", "Same mean change" = "#8c8c8c")) +
      labs(x = NULL, y = "AE yes mean change minus AE no mean change", fill = NULL) +
      theme_minimal(base_size = 12)

    ggplotly(p, tooltip = "text") |>
      plotly::layout(margin = list(l = 205, r = 10, b = 75, t = 25))
  })

  output$second_order_interpretations <- renderUI({
    validate(need(nrow(second_order_changes) > 0, "Run scripts/07_key_differences.R first."))

    top_rows <- second_order_changes |>
      arrange(desc(absolute_effect_size)) |>
      slice_head(n = 5)

    tagList(
      purrr::pmap(
        top_rows,
        function(field_analyzed, outcome, scale_note, controls,
                 n_yes, n_no, median_yes, median_no, median_difference,
                 mean_yes, mean_no, change_contrast, change_pattern,
                 original_pre_mean_yes, original_post_mean_yes, original_answer_change_yes,
                 original_pre_mean_no, original_post_mean_no, original_answer_change_no,
                 ae_yes_answer_change, ae_no_answer_change,
                 rank_biserial_r, effect_size_label, absolute_effect_size,
                 p_value, p_fdr, p_text, fdr_p_text,
                 adjusted_estimate, adjusted_p, adjusted_p_text, adjusted_n,
                 interpretation, ...) {
          div(
            class = "interpretation-card",
            h4(field_analyzed),
            tags$div(
              class = "interpretation-meta",
              paste0(
                change_pattern,
                " | mean contrast=", round(change_contrast, 2),
                " | r=", round(rank_biserial_r, 3),
                " (", effect_size_label, ")"
              )
            ),
            tags$div(
              class = "change-detail",
              tags$strong("What actually changed"),
              p(ae_yes_answer_change),
              p(ae_no_answer_change)
            ),
            p(tags$strong("How the item is scored: "), scale_note),
            p(interpretation)
          )
        }
      )
    )
  })

  output$second_order_table <- renderDT({
    second_order_changes |>
      select(
        field_analyzed, scale_note, controls, change_pattern,
        n_yes, n_no, mean_yes, mean_no, change_contrast,
        original_pre_mean_yes, original_post_mean_yes, original_answer_change_yes,
        original_pre_mean_no, original_post_mean_no, original_answer_change_no,
        ae_yes_answer_change, ae_no_answer_change,
        median_yes, median_no, median_difference,
        rank_biserial_r, effect_size_label, p_text, fdr_p_text,
        adjusted_estimate, adjusted_p_text, interpretation
      ) |>
      datatable(filter = "top", options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
  })

  output$top_difference_interpretations <- renderUI({
    validate(need(nrow(key_differences) > 0, "Run scripts/07_key_differences.R first."))

    top_rows <- key_differences |>
      arrange(desc(absolute_effect_size)) |>
      slice_head(n = 5)

    tagList(
      purrr::pmap(
        top_rows,
        function(family, field_analyzed, outcome, direction, scale_note, controls,
                 n_yes, n_no, median_yes, median_no, median_difference, mean_difference,
                 p_value, p_fdr, rank_biserial_r, adjusted_estimate, adjusted_p,
                 adjusted_n, effect_size_label, absolute_effect_size, adjusted_p_text,
                 p_text, fdr_p_text, interpretation, ...) {
          div(
            class = "interpretation-card",
            h4(field_analyzed),
            tags$div(
              class = "interpretation-meta",
              paste0(
                family,
                " | n yes=", n_yes,
                ", n no=", n_no,
                " | r=", round(rank_biserial_r, 3),
                " (", effect_size_label, ")"
              )
            ),
            p(interpretation)
          )
        }
      )
    )
  })

  output$fields_controls_table <- renderDT({
    key_fields_controls |>
      datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$key_difference_table <- renderDT({
    key_differences |>
      select(
        family, field_analyzed, scale_note, controls,
        n_yes, n_no, median_yes, median_no, median_difference,
        rank_biserial_r, effect_size_label, p_text, fdr_p_text,
        adjusted_estimate, adjusted_p_text, interpretation
      ) |>
      datatable(filter = "top", options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })

  atlas_filtered <- reactive({
    data <- ae_atlas_observations
    if (!is.null(input$atlas_section) && input$atlas_section != "__all__") {
      data <- data |> filter(section == input$atlas_section)
    }
    if (!is.null(input$atlas_evidence) && input$atlas_evidence != "__all__") {
      data <- data |> filter(evidence == input$atlas_evidence)
    }
    search <- stringr::str_squish(input$atlas_search %||% "")
    if (search != "") {
      search_lower <- stringr::str_to_lower(search)
      data <- data |>
        filter(stringr::str_detect(
          stringr::str_to_lower(paste(section, family, title, description, related_data)),
          stringr::fixed(search_lower)
        ))
    }
    data
  })

  output$ae_atlas_global_cards <- renderUI({
    global_rows <- ae_atlas_observations |>
      filter(family %in% c("Global multivariate AE profile", "Baseline profile prediction", "Outcome completeness")) |>
      arrange(p_value)
    disclosure_list(global_rows)
  })

  output$atlas_result_count <- renderText({
    paste0(nrow(atlas_filtered()), " of ", nrow(ae_atlas_observations), " observations shown")
  })

  output$ae_atlas_accordion <- renderUI({
    disclosure_list(atlas_filtered() |> arrange(section, p_value))
  })

  output$ae_atlas_profile_plot <- renderPlotly({
    profile <- ae_atlas_inventory |>
      filter(
        family %in% c("AE status at baseline", "AE status and observed change"),
        !is.na(item), !is.na(effect_size)
      ) |>
      mutate(
        profile = recode(
          family,
          "AE status at baseline" = "Baseline AE yes vs no",
          "AE status and observed change" = "AE difference in PRE-to-POST change"
        ),
        item_label = paste0("Item ", item, ": ", stringr::str_trunc(stringr::str_remove(outcome, "^(PRE|Change) Item [0-9]+: "), 36))
      )

    p <- profile |>
      ggplot(aes(
        x = reorder(item_label, item), y = effect_size, color = profile,
        text = paste0(
          outcome,
          "<br>", profile,
          "<br>Rank-biserial r: ", round(effect_size, 3),
          "<br>Raw p: ", p_text,
          "<br>Family FDR: ", family_fdr_text,
          "<br>Atlas FDR: ", atlas_fdr_text
        )
      )) +
      geom_hline(yintercept = 0, color = "#777777") +
      geom_point(size = 3) +
      geom_segment(aes(xend = item_label, y = 0, yend = effect_size), linewidth = 0.7) +
      coord_flip() +
      facet_wrap(~ profile, ncol = 1) +
      scale_color_manual(values = c("Baseline AE yes vs no" = "#2a7f9e", "AE difference in PRE-to-POST change" = "#c86b3c")) +
      labs(x = NULL, y = "Rank-biserial effect", color = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")

    ggplotly(p, tooltip = "text") |>
      plotly::layout(margin = list(l = 220, r = 15, b = 70, t = 25))
  })

  output$ae_atlas_inventory_table <- renderDT({
    ae_atlas_inventory |>
      select(
        evidence, family, outcome, effect_metric, estimate, conf_low, conf_high,
        effect_size, n, p_text, family_fdr_text, atlas_fdr_text,
        method, controls, caveat
      ) |>
      datatable(filter = "top", options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })

  output$overall_change_plot <- renderPlotly({
    plot_data <- prepost_tests |>
      mutate(
        label_short = paste0("Item ", item, ": ", stringr::str_trunc(label, 38)),
        evidence = if_else(p_fdr < 0.05, "Item-family FDR < .05", "Not FDR-supported")
      )
    p <- plot_data |>
      ggplot(aes(
        x = reorder(label_short, rank_biserial_r), y = rank_biserial_r,
        color = evidence,
        text = paste0(
          label,
          "<br>Paired rank-biserial r: ", round(rank_biserial_r, 3),
          "<br>Raw p: ", round(p_value, 4),
          "<br>FDR p: ", round(p_fdr, 4)
        )
      )) +
      geom_hline(yintercept = 0, color = "#777777") +
      geom_segment(aes(xend = label_short, y = 0, yend = rank_biserial_r), linewidth = 0.8) +
      geom_point(size = 3) +
      coord_flip() +
      scale_color_manual(values = c("Item-family FDR < .05" = "#167d5a", "Not FDR-supported" = "#78828c")) +
      labs(x = NULL, y = "Paired rank-biserial effect", color = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
    ggplotly(p, tooltip = "text") |>
      plotly::layout(
        showlegend = FALSE,
        margin = list(l = 220, r = 15, b = 75, t = 25),
        xaxis = list(
          tickmode = "array",
          tickvals = c(0, 0.1, 0.2, 0.3, 0.4),
          ticktext = c("0.0", "0.1", "0.2", "0.3", "0.4"),
          range = c(-0.01, 0.46)
        )
      )
  })

  output$overall_evidence_plot <- renderPlotly({
    plot_data <- ae_atlas_inventory |>
      count(evidence) |>
      tidyr::complete(
        evidence = c(
          "Atlas-wide FDR < .05", "Family FDR < .05", "Nominal p < .05 only",
          "No statistical signal", "Descriptive / insufficient data"
        ),
        fill = list(n = 0)
      ) |>
      mutate(
        evidence = factor(evidence, levels = c(
          "Atlas-wide FDR < .05", "Family FDR < .05", "Nominal p < .05 only",
          "No statistical signal", "Descriptive / insufficient data"
        )),
        evidence_short = recode(
          as.character(evidence),
          "Atlas-wide FDR < .05" = "Atlas-wide FDR",
          "Family FDR < .05" = "Family FDR",
          "Nominal p < .05 only" = "Nominal only",
          "No statistical signal" = "No signal",
          "Descriptive / insufficient data" = "Descriptive only"
        )
      )
    p <- plot_data |>
      ggplot(aes(x = evidence_short, y = n, fill = evidence, text = paste0(evidence, ": ", n, " rows"))) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = c(
        "Atlas-wide FDR < .05" = "#167d5a", "Family FDR < .05" = "#2a7f9e",
        "Nominal p < .05 only" = "#c46b20", "No statistical signal" = "#78828c",
        "Descriptive / insufficient data" = "#b8bec5"
      ), drop = FALSE) +
      labs(x = NULL, y = "Number of AE-atlas rows") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text") |>
      plotly::layout(showlegend = FALSE, margin = list(l = 130, r = 15, b = 55, t = 25))
  })

  output$overall_signal_cards <- renderUI({
    disclosure_list(overall_signals |> arrange(priority), show_evidence = FALSE)
  })

  output$observation_highlights <- renderUI({
    confirmed <- prepost_tests |>
      filter(!is.na(p_fdr), p_fdr < 0.05) |>
      arrange(p_fdr)

    exploratory <- observation_scan |>
      filter(!is.na(p_value), p_value < 0.05) |>
      arrange(p_value) |>
      slice_head(n = 6)

    tagList(
      div(
        class = "interpretation-card",
        h4(paste0(nrow(confirmed), " full-sample changes survive item-wise FDR correction")),
        tags$span(class = "evidence-label", "FDR-supported within the planned PRE-to-POST family"),
        p(paste(confirmed$label, collapse = "; ")),
        p("These are observed improvements in art confidence, self-confidence, social-pressure relief, self-expression, and anxiety/fear. Without a comparison group, they cannot be attributed solely to the program.")
      ),
      purrr::pmap(
        exploratory,
        function(analysis_family, field_analyzed, effect_name, effect_scale, controls,
                 estimate, conf_low, conf_high, statistic, partial_r, n,
                 p_value, p_family_fdr, interpretation, p_global_fdr,
                 evidence, p_text, family_fdr_text, global_fdr_text, ...) {
          div(
            class = "interpretation-card",
            h4(field_analyzed),
            tags$span(class = "evidence-label", evidence),
            tags$div(
              class = "interpretation-meta",
              paste0(
                analysis_family, " | n=", n,
                " | raw p ", p_text,
                " | family FDR ", family_fdr_text,
                " | global FDR ", global_fdr_text
              )
            ),
            p(interpretation),
            p(tags$strong("Controlled for: "), controls)
          )
        }
      )
    )
  })

  output$observation_age_plot <- renderPlotly({
    validate(need(nrow(observation_age_effects) > 0, "Run scripts/08_observations.R first."))

    plot_data <- observation_age_effects |>
      mutate(
        short_field = stringr::str_trunc(field_analyzed, width = 48),
        evidence = case_when(
          p_family_fdr < 0.05 ~ "Family FDR < .05",
          p_value < 0.05 ~ "Raw p < .05 only",
          TRUE ~ "No nominal signal"
        )
      )

    p <- plot_data |>
      ggplot(aes(
        x = reorder(short_field, estimate), y = estimate, color = evidence,
        text = paste0(
          field_analyzed,
          "<br>Age slope: ", round(estimate, 3),
          "<br>95% CI: ", round(conf_low, 3), " to ", round(conf_high, 3),
          "<br>Raw p: ", formatC(p_value, format = "f", digits = 3),
          "<br>Family FDR: ", formatC(p_family_fdr, format = "f", digits = 3),
          "<br>", controls
        )
      )) +
      geom_hline(yintercept = 0, color = "#777777") +
      geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2) +
      geom_point(size = 3) +
      coord_flip() +
      scale_color_manual(values = c("Family FDR < .05" = "#167d5a", "Raw p < .05 only" = "#c46b20", "No nominal signal" = "#78828c")) +
      labs(x = NULL, y = "Adjusted improvement change per additional year", color = NULL) +
      theme_minimal(base_size = 12)

    ggplotly(p, tooltip = "text")
  })

  output$observation_age_context <- renderUI({
    validate(need(nrow(observation_ae_age) > 0, "Age context not available."))
    strongest_age <- observation_age_effects |>
      arrange(p_value) |>
      slice_head(n = 3)
    age_row <- observation_ae_age |> slice_head(n = 1)

    tagList(
      div(
        class = "interpretation-card",
        h4("Does AE prevalence vary with age?"),
        p(age_row$interpretation),
        tags$div(class = "interpretation-meta", paste0("Raw p ", ifelse(age_row$p_value < 0.001, "< .001", paste0("= ", round(age_row$p_value, 3))), " | n=", age_row$n))
      ),
      div(
        class = "interpretation-card",
        h4("Strongest age-change leads"),
        p(paste(strongest_age$interpretation, collapse = " ")),
        p("These age slopes are adjusted associations, not evidence that age causes a different program effect.")
      )
    )
  })

  output$observation_scan_table <- renderDT({
    observation_scan |>
      select(
        evidence, analysis_family, field_analyzed, effect_name, estimate,
        partial_r, n, p_text, family_fdr_text, global_fdr_text,
        controls, interpretation
      ) |>
      datatable(filter = "top", options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })

  output$observation_interaction_table <- renderDT({
    bind_rows(observation_age_ae, observation_baseline_ae) |>
      arrange(p_value) |>
      select(
        analysis_family, field_analyzed, effect_name, estimate, conf_low, conf_high,
        partial_r, n, p_value, p_family_fdr, controls, interpretation
      ) |>
      datatable(filter = "top", options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
  })

  output$observation_context_table <- renderDT({
    bind_rows(
      observation_ae_types |>
        select(analysis_family, field_analyzed, estimate, partial_r, n, p_value, p_family_fdr, controls, interpretation),
      observation_verification |>
        select(analysis_family, field_analyzed, estimate, partial_r, n, p_value, p_family_fdr, controls, interpretation)
    ) |>
      arrange(p_value) |>
      datatable(filter = "top", options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
  })

  output$ae_type_plot <- renderPlotly({
    p <- ae_type_freq |>
      group_by(ae_type) |>
      summarise(n = sum(n), .groups = "drop") |>
      ggplot(aes(x = reorder(ae_type, n), y = n)) +
      geom_col(fill = "#2a9d8f") +
      coord_flip() +
      labs(x = "AE type", y = "Count") +
      theme_minimal(base_size = 12)
    ggplotly(p)
  })

  output$verification_plot <- renderPlotly({
    p <- verification_freq |>
      ggplot(aes(x = reorder(verification_method, n), y = n)) +
      geom_col(fill = "#e76f51") +
      coord_flip() +
      labs(x = "Verification method", y = "Count") +
      theme_minimal(base_size = 12)
    ggplotly(p)
  })

  output$ae_type_table <- renderDT({
    ae_type_freq |> datatable(filter = "top", options = list(pageLength = 15), rownames = FALSE)
  })

  output$sensitivity_baseline_table <- renderDT({
    sensitivity_baseline |> datatable(filter = "top", options = list(pageLength = 18, scrollX = TRUE), rownames = FALSE)
  })

  output$sensitivity_change_table <- renderDT({
    sensitivity_change |> datatable(filter = "top", options = list(pageLength = 18, scrollX = TRUE), rownames = FALSE)
  })
}

shinyApp(ui, server)
