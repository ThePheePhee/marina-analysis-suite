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
source(file.path(project_root, "R", "metadata.R"))
source(file.path(project_root, "R", "research_output_functions.R"))

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
research_answers <- read_optional_csv(project_file("outputs", "tables", "research_question_answers.csv"))
research_sample_table <- read_optional_csv(project_file("outputs", "tables", "research_table_1_sample_characteristics.csv"))
research_composite_tests <- read_optional_csv(project_file("outputs", "tables", "pre_composite_tests_by_ae.csv"))
research_age_correlations <- read_optional_csv(project_file("outputs", "tables", "age_correlations.csv"))
research_age_prevalence <- read_optional_csv(project_file("outputs", "tables", "ae_prevalence_by_age_group_test.csv"))
research_ae_type_descriptives <- read_optional_csv(project_file("outputs", "tables", "ae_type_anxiety_descriptives.csv"))
research_verification_descriptives <- read_optional_csv(project_file("outputs", "tables", "verification_anxiety_descriptives.csv"))
analysis_plan_recommendations <- read_optional_csv(project_file("outputs", "tables", "analysis_plan_recommendations.csv"))
research_tables_workbook <- project_file("outputs", "tables", "research_questions_all_tables.xlsx")

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

research_figure_box <- function(title, output_id, caption, width = 6, height = "450px") {
  box(
    width = width,
    title = title,
    status = "primary",
    solidHeader = TRUE,
    plotOutput(output_id, height = height),
    tags$p(class = "research-caption", caption),
    tags$div(
      class = "figure-actions",
      downloadButton(paste0(output_id, "_png"), "PNG (300 dpi)", icon = icon("image")),
      downloadButton(paste0(output_id, "_pdf"), "PDF (vector)", icon = icon("file-pdf"))
    )
  )
}

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Epicenter data analysis suite"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-pie")),
      menuItem("Plain-Language Summary", tabName = "summary", icon = icon("file-lines")),
      menuItem("Research Questions", tabName = "research", icon = icon("file-lines")),
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
      .research-lead {
        border-left: 4px solid #007c91;
        padding: 12px 15px;
        background: #f2fafb;
        color: #243746;
        margin-bottom: 12px;
      }
      .research-answer {
        border-left: 4px solid #007a5e;
        padding: 13px 15px;
        margin-bottom: 14px;
        background: #ffffff;
        border-top: 1px solid #d8dee8;
        border-right: 1px solid #d8dee8;
        border-bottom: 1px solid #d8dee8;
      }
      .research-answer h4 { margin: 0 0 6px; font-weight: 700; }
      .research-answer .answer-conclusion { font-size: 16px; line-height: 1.45; color: #173b32; }
      .research-answer details { margin-top: 8px; }
      .research-answer summary { cursor: pointer; font-weight: 700; color: #00677a; }
      .figure-actions { display: flex; gap: 8px; flex-wrap: wrap; margin-top: 10px; }
      .research-caption { color: #667085; font-size: 12px; line-height: 1.45; min-height: 34px; }
      .export-note { color: #475467; margin-left: 10px; }
      .research-table-guide { color: #475467; margin: 0 0 10px; }
      .rq-column-title { white-space: nowrap; }
      .rq-header-help {
        width: 18px;
        height: 18px;
        margin-left: 4px;
        padding: 0;
        border: 1px solid #78909c;
        border-radius: 50%;
        background: #ffffff;
        color: #155e75;
        font-size: 12px;
        font-weight: 700;
        line-height: 15px;
        text-align: center;
        vertical-align: middle;
      }
      .rq-header-help:hover, .rq-header-help:focus { background: #e7f4f7; outline: none; }
      .popover { max-width: 300px; }
      .popover-content { line-height: 1.4; }
      .summary-page { max-width: 980px; }
      .summary-page h3 { margin-top: 20px; font-weight: 700; color: #173b32; }
      .summary-page p, .summary-page li { font-size: 15px; line-height: 1.55; color: #344054; }
      .summary-page .summary-lead { font-size: 17px; color: #173b32; }
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
        tabName = "summary",
        if (nrow(research_answers) == 0) empty_panel() else fluidRow(
          box(
            width = 12,
            title = "Plain-Language Analysis Overview",
            status = "success",
            solidHeader = TRUE,
            tags$div(class = "summary-page", uiOutput("plain_language_writeup"))
          )
        )
      ),
      tabItem(
        tabName = "research",
        if (nrow(research_answers) == 0) empty_panel() else fluidRow(
          box(
            width = 12,
            title = "Analysis-Plan Research Questions",
            status = "primary",
            solidHeader = TRUE,
            tags$div(
              class = "research-lead",
              p("This page follows the original analysis plan: three prespecified research questions, complete-pair nonparametric tests, explicit effect sizes and denominators, and BH false-discovery-rate correction within each nine-item family."),
              p("All conclusions are regenerated by scripts/10_research_questions.R from the current processed dataset. Figure exports are created with ggplot2; PNG files are 300 dpi and PDF files are vector graphics."),
              p("Because the Likert responses contain many ties, the current Wilcoxon/Mann-Whitney p-values use the asymptotic implementation. The plan should specify this explicitly rather than requesting generic 'exact' p-values.")
            ),
            downloadButton("rq_all_tables_xlsx", "Download all tables (.xlsx)", icon = icon("file-excel")),
            tags$span(class = "export-note", "Individual visible tables also provide Copy, CSV, and Excel export controls.")
          ),
          box(width = 12, title = "Direct Answers", status = "success", solidHeader = TRUE, uiOutput("rq_answer_cards")),
          tabsetPanel(
            id = "research_sections",
            tabPanel(
              "RQ1: Baseline",
              fluidRow(
                box(width = 12, title = "Baseline AE, Anxiety, and Wellbeing", status = "primary", solidHeader = TRUE,
                    p("Primary outcome: PRE Item 6. Supporting analyses: all nine PRE items, the wellbeing composite, and the Items 5-6 distress subscale.")),
                research_figure_box("Planned Figure 1", "rq_fig1", "Baseline Item 6 anxiety/fear by AE group, showing the full distribution and robust summaries."),
                research_figure_box("Planned Figure 2", "rq_fig2", "Baseline nine-item wellbeing composite by AE group after reversing negatively directed Items 5 and 6."),
                box(
                  width = 12,
                  title = "Table 2. PRE Item Scores by AE Group",
                  status = "primary",
                  solidHeader = TRUE,
                  p(class = "research-table-guide", "Click a ? beside any column heading for a plain-language definition. All values are item-specific, so sample sizes may differ because of missing responses."),
                  DTOutput("rq_table2")
                ),
                box(width = 12, title = "Prespecified Baseline Composite Tests", status = "primary", solidHeader = TRUE, DTOutput("rq_composite_table"))
              )
            ),
            tabPanel(
              "RQ2: Overall Change",
              fluidRow(
                box(width = 12, title = "Overall PRE-to-POST Change", status = "primary", solidHeader = TRUE,
                    p("Change is direction-coded so positive values always indicate improvement. These are observed within-person changes, not causal estimates of program impact.")),
                research_figure_box("Full-Sample Change Summary", "rq_full_change", "Paired rank-biserial effects for all nine outcomes, with planned BH FDR support highlighted.", width = 12, height = "540px"),
                box(width = 12, title = "Table 3. Full-Sample PRE-to-POST Change", status = "primary", solidHeader = TRUE, DTOutput("rq_table3"))
              )
            ),
            tabPanel(
              "RQ3: AE Trajectories",
              fluidRow(
                box(width = 12, title = "Does Change Differ by AE Status?", status = "primary", solidHeader = TRUE,
                    p("The planned second-order contrast compares improvement-coded change scores between AE-yes and AE-no participants.")),
                research_figure_box("Planned Figure 3", "rq_fig3", "Observed PRE-to-POST Item 6 trajectories, retaining the original anxiety/fear scale so direction is unambiguous."),
                research_figure_box("AE Change-Contrast Summary", "rq_ae_change", "Rank-biserial effects for the difference in change between AE-yes and AE-no groups."),
                box(width = 12, title = "Table 4. PRE-to-POST Change by AE Group", status = "primary", solidHeader = TRUE, DTOutput("rq_table4"))
              )
            ),
            tabPanel(
              "Supporting Outputs",
              fluidRow(
                box(width = 12, title = "Planned Descriptive and Exploratory Outputs", status = "primary", solidHeader = TRUE,
                    p("These outputs cover the plan's requested Item 5/6 frequency distributions, AE type and verification summaries, and age analyses.")),
                research_figure_box("Requested Item 5/6 Distributions", "rq_item56", "Baseline frequency distributions for the two negatively valenced core outcomes."),
                research_figure_box("Planned Figure 4", "rq_fig4", "Non-exclusive AE type frequencies among AE-yes participants, filled by verification route."),
                box(width = 6, title = "Age Analyses", status = "primary", solidHeader = TRUE, DTOutput("rq_age_table")),
                box(width = 6, title = "AE Type and Verification Descriptives", status = "primary", solidHeader = TRUE, DTOutput("rq_context_table")),
                box(width = 12, title = "Table 1. Sample Characteristics", status = "primary", solidHeader = TRUE, DTOutput("rq_table1"))
              )
            ),
            tabPanel(
              "Plan Review",
              fluidRow(
                box(
                  width = 12,
                  title = "Comments on the Analysis Plan",
                  status = "warning",
                  solidHeader = TRUE,
                  p("These recommendations distinguish changes needed before confirmatory reporting from useful refinements. They do not alter the original Word file."),
                  downloadButton("rq_plan_comments_csv", "Download recommendations (.csv)", icon = icon("download")),
                  DTOutput("rq_plan_recommendations")
                )
              )
            )
          )
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
              "Each row below remains on the cleaning register until the source is confirmed. “Tentatively resolved” means calculations currently use a documented provisional value: age 8.5 is floored to 8, while grade entries use the expected midpoint age for that grade (1st = 6.5, 2nd = 7.5, 4th = 9.5). The original workbook is never overwritten."
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
          box(
            width = 12,
            title = "What This Sensitivity Check Does",
            status = "primary",
            solidHeader = TRUE,
            tags$div(
              class = "research-lead",
              p("The primary AE comparisons exclude participants recorded as '?'/unknown. This page deliberately reruns those comparisons twice: first treating every unknown record as AE-no, then treating every unknown record as AE-yes."),
              p("Use these checks to ask whether the conclusion depends on that unresolved coding decision. A result that appears only under one extreme recoding should be treated as fragile, not definitive.")
            ),
            uiOutput("sensitivity_summary")
          ),
          box(
            width = 12,
            title = "Baseline Item Sensitivity",
            status = "primary",
            solidHeader = TRUE,
            p(class = "research-table-guide", "This table repeats the baseline AE comparisons after each possible unknown-AE recode. Click a ? beside any heading for a definition."),
            DTOutput("sensitivity_baseline_table")
          ),
          box(
            width = 12,
            title = "PRE-to-POST Change Sensitivity",
            status = "primary",
            solidHeader = TRUE,
            p(class = "research-table-guide", "This table repeats the AE-group change comparisons after each possible unknown-AE recode. Change is coded so positive values mean improvement. Click a ? beside any heading for a definition."),
            DTOutput("sensitivity_change_table")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  research_header <- function(label, explanation) {
    safe_label <- htmltools::htmlEscape(label, attribute = TRUE)
    safe_explanation <- htmltools::htmlEscape(explanation, attribute = TRUE)
    paste0(
      "<span class='rq-column-title'>", htmltools::htmlEscape(label), "</span>",
      "<button type='button' class='rq-header-help' tabindex='0' aria-label='Explain ",
      safe_label, "' data-title='", safe_label, "' data-content='", safe_explanation,
      "' onclick=\"event.stopPropagation(); var button=$(this); $('.rq-header-help').not(button).popover('hide'); ",
      "button.popover({container:'body', placement:'auto top', trigger:'manual', title:button.attr('data-title'), ",
      "content:button.attr('data-content')}).popover('toggle'); return false;\">?</button>"
    )
  }

  research_datatable <- function(data, page_length = 10) {
    datatable(
      data,
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = page_length,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      )
    )
  }

  sensitivity_datatable <- function(data, analysis_label) {
    display_data <- data |>
      transmute(
        scenario = dplyr::recode(
          scenario,
          unknown_as_no = "Treat unknown AE as no",
          unknown_as_yes = "Treat unknown AE as yes"
        ),
        item,
        label,
        scale_interpretation = if (analysis_label == "baseline") {
          dplyr::recode(direction, higher_better = "Higher = better", higher_worse = "Higher = worse")
        } else {
          "Positive change = improvement"
        },
        n_yes,
        n_no,
        median_iqr_yes,
        median_iqr_no,
        statistic,
        p_value,
        rank_biserial_r,
        p_fdr,
        effect_magnitude = dplyr::case_when(
          is.na(rank_biserial_r) ~ "Not estimable",
          abs(rank_biserial_r) < 0.1 ~ "Negligible",
          abs(rank_biserial_r) < 0.3 ~ "Small",
          abs(rank_biserial_r) < 0.5 ~ "Medium",
          TRUE ~ "Large"
        )
      )

    summary_label <- if (analysis_label == "baseline") "median PRE score (IQR)" else "median improvement-coded change (IQR)"
    headers <- c(
      research_header("Unknown-AE scenario", "How the analysis temporarily classifies participants recorded as AE-unknown. These are deliberately extreme alternatives, not verified classifications."),
      research_header("Item", "Question number in the nine paired PRE/POST questionnaire."),
      research_header("Outcome statement", "The questionnaire item being compared."),
      research_header("How to read the score", if (analysis_label == "baseline") "For baseline scores, this states whether a higher raw score is better or worse." else "For change scores, positive always means improvement because negatively worded Items 5 and 6 were reversed before calculating change."),
      research_header("AE yes, n", "Number of participants assigned to the AE-yes group under this scenario with usable data for this item."),
      research_header("AE no, n", "Number of participants assigned to the AE-no group under this scenario with usable data for this item."),
      research_header(paste0("AE yes ", summary_label), "Robust summary for the group assigned AE-yes under this scenario. IQR is the middle 50% of observations."),
      research_header(paste0("AE no ", summary_label), "Robust summary for the group assigned AE-no under this scenario. IQR is the middle 50% of observations."),
      research_header("Mann-Whitney U", "Rank-based test statistic comparing the two scenario-defined groups. It is reported for reproducibility, not as an effect size."),
      research_header("Raw p", "Unadjusted p-value for this item and this scenario."),
      research_header("Rank-biserial r", "Effect size. Positive values mean relatively higher scores or greater improvement in the scenario-defined AE-yes group; negative values mean the opposite."),
      research_header("BH FDR q", "Benjamini-Hochberg false-discovery-rate adjusted p-value across the nine item tests within this scenario. Values below .05 are FDR-supported within that scenario."),
      research_header("Effect magnitude", "Magnitude of the absolute rank-biserial effect: below .10 negligible, .10-.29 small, .30-.49 medium, and .50 or above large.")
    )

    datatable(
      display_data,
      rownames = FALSE,
      filter = "top",
      escape = FALSE,
      colnames = headers,
      extensions = "Buttons",
      options = list(
        pageLength = 18,
        scrollX = TRUE,
        autoWidth = FALSE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        columnDefs = list(
          list(width = "190px", targets = 0),
          list(width = "55px", targets = 1),
          list(width = "220px", targets = 2),
          list(width = "150px", targets = 3),
          list(width = "70px", targets = c(4, 5)),
          list(width = "170px", targets = c(6, 7)),
          list(width = "95px", targets = 8),
          list(width = "80px", targets = c(9, 10, 11, 12))
        )
      )
    ) |>
      formatRound("statistic", digits = 1) |>
      formatSignif(c("p_value", "rank_biserial_r", "p_fdr"), digits = 3)
  }

  output$plain_language_writeup <- renderUI({
    n_total <- nrow(participants)
    n_ae_yes <- sum(participants$ae_status == "yes", na.rm = TRUE)
    n_ae_no <- sum(participants$ae_status == "no", na.rm = TRUE)
    n_ae_unknown <- sum(participants$ae_status == "unknown", na.rm = TRUE)
    supported_change <- prepost_tests |> filter(!is.na(p_fdr), p_fdr < 0.05)

    tagList(
      p(
        class = "summary-lead",
        "This is a plain-language account of the current analysis outputs. It is regenerated from the processed data and the scripted results, so it should be read as a snapshot of the most recently run pipeline."
      ),
      tags$h3("What Was Studied"),
      p(
        paste0(
          "The analysis includes ", n_total, " participant records: ", n_ae_yes,
          " coded AE-yes, ", n_ae_no, " coded AE-no, and ", n_ae_unknown,
          " recorded as uncertain. The main comparisons follow the plan by comparing AE-yes with AE-no participants, while the uncertain group is examined separately in sensitivity checks."
        )
      ),
      tags$h3("What the Three Main Questions Show"),
      tagList(lapply(seq_len(nrow(research_answers)), function(i) {
        row <- research_answers[i, , drop = FALSE]
        tags$div(
          class = "research-answer",
          tags$h4(paste0(row$question_id, ". ", row$research_question)),
          tags$p(class = "answer-conclusion", row$conclusion),
          tags$p(row$primary_result)
        )
      })),
      tags$h3("Overall Interpretation"),
      p(
        paste0(
          "The clearest pattern in this dataset is an overall pre-to-post improvement on ",
          nrow(supported_change), " of the nine planned outcomes after correcting for multiple tests. ",
          "The data do not provide corresponding corrected evidence that AE-yes and AE-no participants began with different anxiety/fear scores or changed differently over the program."
        )
      ),
      tags$h3("What This Does Not Establish"),
      tags$ul(
        tags$li("The pre-to-post changes cannot be attributed solely to the program because there is no untreated comparison group."),
        tags$li("The AE comparisons are observational: AE status was identified informally and was not randomly assigned."),
        tags$li("The questionnaire does not directly measure depression, and the anxiety/fear result is based on a single non-standardized item."),
        tags$li("The sizable uncertain-AE group remains an important source of uncertainty; the Sensitivity tab shows how extreme recoding assumptions affect the results.")
      ),
      tags$h3("Practical Bottom Line"),
      p("For a writeup, I would report the overall improvement signal cautiously, make the uncertainty around causal attribution explicit, and describe the absence of FDR-supported AE-group differences rather than presenting it as evidence that anomalous experiences have no relationship to outcomes. Better AE ascertainment, a defined comparison condition, and final resolution of the uncertain records would materially strengthen a future study.")
    )
  })

  output$sensitivity_summary <- renderUI({
    n_unknown <- sum(participants$ae_status == "unknown", na.rm = TRUE)
    baseline_supported <- sensitivity_baseline |> filter(!is.na(p_fdr), p_fdr < 0.05)
    change_supported <- sensitivity_change |> filter(!is.na(p_fdr), p_fdr < 0.05)

    summary_text <- if (nrow(baseline_supported) == 0 && nrow(change_supported) == 0) {
      "Across both extreme recodings, no baseline item and no PRE-to-POST change item reaches BH FDR < .05. In the current data, the broad conclusion of no corrected AE-group signal is therefore not driven solely by excluding the uncertain-AE group."
    } else {
      paste0(
        "At least one result reaches BH FDR < .05 under an extreme unknown-AE recoding. Inspect the scenario, item, and effect size carefully: a finding that appears under only one recoding is sensitive to an unresolved classification decision."
      )
    }

    tagList(
      div(
        class = "interpretation-card",
        h4("What the page says in the current data"),
        p(summary_text),
        tags$div(class = "interpretation-meta", paste0(n_unknown, " participants are currently coded AE-unknown."))
      ),
      div(
        class = "interpretation-card",
        h4("How to interpret a stable result"),
        p("A result is more robust when its direction and inference are similar under both recoding scenarios. Stability does not turn the observational comparison into causal evidence; it only reduces dependence on the unknown-AE coding choice.")
      )
    )
  })

  output$rq_answer_cards <- renderUI({
    tagList(lapply(seq_len(nrow(research_answers)), function(i) {
      row <- research_answers[i, , drop = FALSE]
      div(
        class = "research-answer",
        h4(paste0(row$question_id, ". ", row$research_question)),
        p(class = "answer-conclusion", tags$strong(row$conclusion)),
        p(row$primary_result),
        tags$details(
          tags$summary("Methods and interpretation limits"),
          p(tags$strong("Analysis: "), row$method),
          p(tags$strong("Interpretation limit: "), row$interpretation_limit)
        )
      )
    }))
  })

  output$rq_table1 <- renderDT({
    research_datatable(research_sample_table)
  })

  output$rq_table2 <- renderDT({
    display_data <- baseline_tests |>
      transmute(
        item,
        label,
        direction = dplyr::recode(direction, higher_better = "Higher = better", higher_worse = "Higher = worse"),
        primary_focus = dplyr::case_when(
          item == 6 ~ "Yes - primary anxiety outcome",
          item == 5 ~ "Yes - related distress outcome",
          TRUE ~ "No"
        ),
        n_yes,
        n_no,
        median_iqr_yes,
        median_iqr_no,
        statistic,
        p_value,
        rank_biserial_r,
        test_note = "Mann-Whitney U test; asymptotic p-value used because responses contain ties.",
        p_fdr,
        effect_size_interpretation
      )

    headers <- c(
      research_header("Item", "Question number in the nine paired PRE/POST questionnaire."),
      research_header("Outcome statement", "The exact questionnaire item being compared at PRE (before the program)."),
      research_header("Scale direction", "How to read higher raw scores. Items 5 and 6 are negatively worded, so a higher score means more difficulty or distress."),
      research_header("Primary focus in plan", "Yes marks the two distress-focused items highlighted in the original plan. Item 6 is the single primary anxiety/fear outcome; Item 5 is a related distress outcome."),
      research_header("AE yes, n", "Number of AE-yes participants with a non-missing PRE score for this item."),
      research_header("AE no, n", "Number of AE-no participants with a non-missing PRE score for this item."),
      research_header("AE yes median (IQR)", "Median PRE score in the AE-yes group. The interquartile range (IQR) gives the middle 50% of scores."),
      research_header("AE no median (IQR)", "Median PRE score in the AE-no group. The interquartile range (IQR) gives the middle 50% of scores."),
      research_header("Mann-Whitney U", "Rank-based test statistic comparing the two independent AE groups. It is reported for reproducibility, not as an effect size."),
      research_header("Raw p", "Unadjusted p-value for this individual item comparison."),
      research_header("Rank-biserial r", "Effect size for the AE-yes versus AE-no comparison. Positive values indicate relatively higher raw scores in AE-yes; negative values indicate relatively lower raw scores."),
      research_header("Test note", "Method note. The analysis uses an asymptotic Mann-Whitney p-value because the bounded Likert responses contain ties."),
      research_header("BH FDR q", "Benjamini-Hochberg false-discovery-rate adjusted p-value across all nine planned baseline item tests. Values below .05 are FDR-supported within this family."),
      research_header("Effect magnitude", "Magnitude label for the absolute rank-biserial effect: below .10 negligible, .10-.29 small, .30-.49 medium, and .50 or above large.")
    )

    datatable(
      display_data,
      rownames = FALSE,
      filter = "top",
      escape = FALSE,
      colnames = headers,
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = FALSE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        columnDefs = list(
          list(width = "55px", targets = 0),
          list(width = "230px", targets = 1),
          list(width = "125px", targets = 2),
          list(width = "185px", targets = 3),
          list(width = "70px", targets = c(4, 5)),
          list(width = "145px", targets = c(6, 7)),
          list(width = "95px", targets = 8),
          list(width = "80px", targets = c(9, 10, 12, 13)),
          list(width = "185px", targets = 11)
        )
      )
    ) |>
      formatRound(c("statistic"), digits = 1) |>
      formatSignif(c("p_value", "rank_biserial_r", "p_fdr"), digits = 3)
  })

  output$rq_composite_table <- renderDT({
    research_datatable(research_composite_tests)
  })

  output$rq_table3 <- renderDT({
    research_datatable(prepost_tests)
  })

  output$rq_table4 <- renderDT({
    research_datatable(change_by_ae_tests)
  })

  output$rq_age_table <- renderDT({
    age_rows <- dplyr::bind_rows(
      research_age_correlations |> dplyr::mutate(analysis = "Spearman age correlation", .before = 1),
      research_age_prevalence |> dplyr::mutate(analysis = "AE prevalence by age group", .before = 1)
    )
    research_datatable(age_rows, page_length = 6)
  })

  output$rq_context_table <- renderDT({
    context_rows <- dplyr::bind_rows(
      research_ae_type_descriptives |> dplyr::mutate(context = "AE type", .before = 1),
      research_verification_descriptives |> dplyr::mutate(context = "Verification method", .before = 1)
    )
    research_datatable(context_rows, page_length = 8)
  })

  output$rq_plan_recommendations <- renderDT({
    research_datatable(analysis_plan_recommendations, page_length = 13) |>
      formatStyle(
        "priority",
        backgroundColor = styleEqual(c("Critical", "High", "Medium"), c("#fdecec", "#fff4df", "#eef4fb")),
        color = styleEqual(c("Critical", "High", "Medium"), c("#9b1c1c", "#8a4b00", "#254f7a")),
        fontWeight = "700"
      )
  })

  output$rq_fig1 <- renderPlot(rq_plot_pre_anxiety(participants), res = 120)
  output$rq_fig2 <- renderPlot(rq_plot_pre_wellbeing(participants), res = 120)
  output$rq_fig3 <- renderPlot(rq_plot_item6_paired(item_long, participants), res = 120)
  output$rq_fig4 <- renderPlot(rq_plot_ae_types(participants), res = 120)
  output$rq_item56 <- renderPlot(rq_plot_item_5_6_distributions(item_long, participants), res = 120)
  output$rq_full_change <- renderPlot(rq_plot_full_sample_change(prepost_tests), res = 120)
  output$rq_ae_change <- renderPlot(rq_plot_ae_change_contrasts(change_by_ae_tests), res = 120)

  plot_download_specs <- list(
    rq_fig1 = list(plot = function() rq_plot_pre_anxiety(participants), stem = "figure_1_pre_anxiety", width = 7.2, height = 5.2),
    rq_fig2 = list(plot = function() rq_plot_pre_wellbeing(participants), stem = "figure_2_pre_wellbeing", width = 7.2, height = 5.2),
    rq_fig3 = list(plot = function() rq_plot_item6_paired(item_long, participants), stem = "figure_3_prepost_item6", width = 8.2, height = 5.5),
    rq_fig4 = list(plot = function() rq_plot_ae_types(participants), stem = "figure_4_ae_types_verification", width = 8.2, height = 5.8),
    rq_item56 = list(plot = function() rq_plot_item_5_6_distributions(item_long, participants), stem = "supplementary_figure_1_item5_6_distributions", width = 9, height = 6.2),
    rq_full_change = list(plot = function() rq_plot_full_sample_change(prepost_tests), stem = "supplementary_figure_2_full_sample_change", width = 9, height = 6),
    rq_ae_change = list(plot = function() rq_plot_ae_change_contrasts(change_by_ae_tests), stem = "supplementary_figure_3_ae_change_contrasts", width = 9, height = 6)
  )

  purrr::iwalk(plot_download_specs, function(spec, id) {
    local({
      plot_spec <- spec
      plot_id <- id
      output[[paste0(plot_id, "_png")]] <- downloadHandler(
        filename = function() paste0(plot_spec$stem, ".png"),
        content = function(file) ggplot2::ggsave(file, plot_spec$plot(), width = plot_spec$width, height = plot_spec$height, dpi = 300, bg = "white")
      )
      output[[paste0(plot_id, "_pdf")]] <- downloadHandler(
        filename = function() paste0(plot_spec$stem, ".pdf"),
        content = function(file) ggplot2::ggsave(file, plot_spec$plot(), width = plot_spec$width, height = plot_spec$height, bg = "white")
      )
    })
  })

  output$rq_all_tables_xlsx <- downloadHandler(
    filename = function() "research_questions_all_tables.xlsx",
    content = function(file) {
      validate(need(file.exists(research_tables_workbook), "Run scripts/10_research_questions.R first."))
      file.copy(research_tables_workbook, file, overwrite = TRUE)
    }
  )

  output$rq_plan_comments_csv <- downloadHandler(
    filename = function() "analysis_plan_recommendations.csv",
    content = function(file) readr::write_csv(analysis_plan_recommendations, file, na = "")
  )

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
        age_tentatively_resolved_n = sum(age_tentatively_resolved_flag, na.rm = TRUE),
        .groups = "drop"
      ) |>
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })

  output$cleaning_table <- renderDT({
    review_rows <- cleaning_flags |>
      filter(age_uncertain_flag | age_outside_plan_range | ae_unknown_flag | verification_method == "other_unclear") |>
      select(
        review_priority, participant_id, age_resolution_status, age_resolution_note,
        why_review_needed, action_required, current_automated_handling, affected_analysis, age_raw, age, ae_raw,
        ae_status, ae_type_raw, verified_by_raw, verification_method
      )

    datatable(review_rows, filter = "top", options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE) |>
      formatStyle(
        "review_priority",
        backgroundColor = styleEqual(
          c("Tentatively resolved", "Tentatively resolved; other review remains", "Decision required"),
          c("#e8f5e9", "#fff8e1", "#fff3e0")
        ),
        color = styleEqual(
          c("Tentatively resolved", "Tentatively resolved; other review remains", "Decision required"),
          c("#246b35", "#7a5a00", "#8a4b00")
        ),
        fontWeight = "700"
      ) |>
      formatStyle("age_resolution_status", backgroundColor = styleEqual("Tentatively resolved", "#e8f5e9"), color = "#246b35", fontWeight = "700") |>
      formatStyle("action_required", backgroundColor = "#fffaf0", fontWeight = "600")
  })

  output$cleaning_action_summary <- renderUI({
    flagged_rows <- cleaning_flags |>
      filter(age_uncertain_flag | age_outside_plan_range | ae_unknown_flag | verification_method == "other_unclear")

    tagList(
      tags$span(class = "cleaning-summary", paste(nrow(flagged_rows), "participant rows remain on the cleaning register")),
      tags$span(class = "cleaning-summary", paste(sum(flagged_rows$ae_unknown_flag, na.rm = TRUE), "ambiguous AE-status entries")),
      tags$span(class = "cleaning-summary", paste(sum(flagged_rows$age_tentatively_resolved_flag, na.rm = TRUE), "age entries tentatively resolved for calculation")),
      tags$span(class = "cleaning-summary", paste(sum(flagged_rows$age_uncertain_flag, na.rm = TRUE), "age issues still awaiting source confirmation")),
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
    sensitivity_datatable(sensitivity_baseline, "baseline")
  })

  output$sensitivity_change_table <- renderDT({
    sensitivity_datatable(sensitivity_change, "change")
  })
}

shinyApp(ui, server)
