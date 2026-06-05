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
