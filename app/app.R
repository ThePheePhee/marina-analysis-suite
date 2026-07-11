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
          box(width = 12, title = "Rows Needing Human Review", status = "warning", solidHeader = TRUE, DTOutput("cleaning_table"))
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
    cleaning_flags |>
      filter(age_uncertain_flag | age_outside_plan_range | ae_unknown_flag | verification_method == "other_unclear") |>
      datatable(filter = "top", options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
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
