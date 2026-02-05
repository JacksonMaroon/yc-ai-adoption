suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(readr)
  library(jsonlite)
  library(scales)
  library(bslib)
})

# ---- Data Load ----
companies <- read_csv(
  "data/yc_companies.csv",
  show_col_types = FALSE,
  na = c("", "NA")
)

# Coerce logicals and numeric
companies <- companies %>%
  mutate(
    has_ai = tolower(as.character(has_ai)) == "true",
    is_hiring = tolower(as.character(is_hiring)) == "true",
    batch_year = as.integer(batch_year),
    batch_order = as.integer(batch_order),
    team_size = as.numeric(team_size)
  )

# Exclude partial/unreleased batches
excluded_batches <- c("Spring 2026", "Summer 2026")
companies <- companies %>%
  filter(!(batch %in% excluded_batches))

# Load meta
meta <- tryCatch(fromJSON("data/yc_meta.json"), error = function(e) list(last_updated = NA))
last_updated <- if (!is.null(meta$last_updated)) {
  as.POSIXct(meta$last_updated, tz = "UTC")
} else {
  NA
}

# Helper to split list-like columns
split_list <- function(x) {
  lapply(x, function(v) {
    if (is.na(v) || v == "") return(character(0))
    str_split(v, ";\\s*")[[1]]
  })
}

companies <- companies %>%
  mutate(
    tags_list = split_list(tags),
    industries_list = split_list(industries),
    regions_list = split_list(regions),
    ai_tags_list = split_list(ai_tags)
  )

# Batch ordering
batch_levels <- companies %>%
  filter(!is.na(batch_order)) %>%
  distinct(batch_code, batch_order) %>%
  arrange(batch_order) %>%
  pull(batch_code)

companies <- companies %>%
  mutate(batch_code = ifelse(is.na(batch_code), batch, batch_code)) %>%
  mutate(batch_code = factor(batch_code, levels = unique(batch_levels)))

# ---- UI Theme ----
app_theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  base_font = font_google("Source Sans 3"),
  heading_font = font_google("Source Sans 3"),
  primary = "#1C4E80",
  secondary = "#E8F1F2"
)

# ---- UI ----
ui <- navbarPage(
  title = "YC AI Adoption Dashboard",
  theme = app_theme,
  header = tagList(
    tags$style(HTML(".card {border: 1px solid #E0E7EF; border-radius: 14px; padding: 18px; background: #fff; box-shadow: 0 6px 16px rgba(0,0,0,0.05);}\n.stat {font-size: 26px; font-weight: 700;}\n.muted {color: #667085;}\n.hero {padding: 24px 26px; background: linear-gradient(120deg,#0F2D4D,#1C4E80); color: white; border-radius: 16px;}\n.hero h2 {color: #fff;}\n.section-title {margin-top: 18px; margin-bottom: 8px; font-weight: 700;}\n"))
  ),
  tabPanel(
    "Home",
    fluidPage(
      fluidRow(
        column(12,
               div(class = "hero",
                   h2("AI Adoption in YC Startups"),
                   p("Explore how AI adoption, hiring, and industry trends have evolved across Y Combinator batches. Data sourced from the yc-oss GitHub API."),
                   p(class = "muted", paste0("Last data refresh (UTC): ", ifelse(is.na(last_updated), "Unknown", format(last_updated, "%Y-%m-%d %H:%M"))))
               )
        )
      ),
      br(),
      fluidRow(
        column(3, div(class = "card", div(class = "muted", "Total Companies"), div(class = "stat", format(nrow(companies), big.mark = ",")))),
        column(3, div(class = "card", div(class = "muted", "AI Adoption"), div(class = "stat", percent(mean(companies$has_ai, na.rm = TRUE), accuracy = 0.1)))),
        column(3, div(class = "card", div(class = "muted", "Hiring Now"), div(class = "stat", percent(mean(companies$is_hiring, na.rm = TRUE), accuracy = 0.1)))),
        column(3, div(class = "card", div(class = "muted", "Active/Public/Acquired"), div(class = "stat", percent(mean(companies$status_group == "Active/Public/Acquired", na.rm = TRUE), accuracy = 0.1))))
      ),
      br(),
      fluidRow(
        column(6,
               div(class = "card",
                   h4("What counts as AI?"),
                   p("Companies are tagged as AI if they include any YC AI-related tags (e.g., AI, Artificial Intelligence, Generative AI, NLP, Computer Vision).")
               )
        ),
        column(6,
               div(class = "card",
                   h4("How to use this dashboard"),
                   p("Use the filters in each tab to explore adoption trends, industry shifts, and hiring patterns across batches. Download the data from the Explorer tab.")
               )
        )
      )
    )
  ),
  tabPanel(
    "AI Adoption",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          sliderInput("year_range", "Batch Year Range",
                      min = min(companies$batch_year, na.rm = TRUE),
                      max = max(companies$batch_year, na.rm = TRUE),
                      value = c(min(companies$batch_year, na.rm = TRUE), max(companies$batch_year, na.rm = TRUE)),
                      sep = ""),
          selectInput("status_filter", "Company Status",
                      choices = c("All", sort(unique(companies$status))), selected = "All"),
          selectInput("region_filter", "Region (optional)",
                      choices = sort(unique(unlist(companies$regions_list))),
                      selected = character(0), multiple = TRUE),
          sliderInput("top_ai_tags", "Top AI Tags", min = 5, max = 15, value = 10)
        ),
        mainPanel(
          width = 9,
          div(class = "card", plotlyOutput("ai_share_plot", height = 360)),
          br(),
          div(class = "card", plotlyOutput("ai_tags_plot", height = 360))
        )
      )
    )
  ),
  tabPanel(
    "Industry Trends",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput("industry_select", "Industry", choices = NULL),
          sliderInput("industry_year_range", "Year Range",
                      min = min(companies$batch_year, na.rm = TRUE),
                      max = max(companies$batch_year, na.rm = TRUE),
                      value = c(min(companies$batch_year, na.rm = TRUE), max(companies$batch_year, na.rm = TRUE)),
                      sep = ""),
          sliderInput("top_industries", "Top Industries (bar)", min = 5, max = 15, value = 10)
        ),
        mainPanel(
          width = 9,
          div(class = "card", plotlyOutput("industry_trend_plot", height = 360)),
          br(),
          div(class = "card", plotlyOutput("industry_top_plot", height = 360))
        )
      )
    )
  ),
  tabPanel(
    "Hiring & Status",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          sliderInput("hiring_year_range", "Year Range",
                      min = min(companies$batch_year, na.rm = TRUE),
                      max = max(companies$batch_year, na.rm = TRUE),
                      value = c(min(companies$batch_year, na.rm = TRUE), max(companies$batch_year, na.rm = TRUE)),
                      sep = "")
        ),
        mainPanel(
          width = 9,
          div(class = "card", plotlyOutput("hiring_plot", height = 360)),
          br(),
          div(class = "card", plotlyOutput("status_plot", height = 360))
        )
      )
    )
  ),
  tabPanel(
    "Geography",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          checkboxInput("geo_ai_only", "AI companies only", value = FALSE),
          sliderInput("geo_top", "Top Regions", min = 5, max = 20, value = 12)
        ),
        mainPanel(
          width = 9,
          div(class = "card", plotlyOutput("region_plot", height = 420))
        )
      )
    )
  ),
  tabPanel(
    "Explorer",
    fluidPage(
      fluidRow(
        column(12,
               div(class = "card",
                   DTOutput("company_table")
               )
        )
      )
    )
  ),
  tabPanel(
    "About",
    fluidPage(
      div(class = "card",
          h4("Data Source"),
          p("This project uses the yc-oss GitHub API (https://github.com/yc-oss/api) which mirrors YC's public company directory."),
          h4("AI Tag Definition"),
          p("AI companies are identified using YC's official AI-related tags: AI, Artificial Intelligence, Machine Learning, Deep Learning, Generative AI, AIOps, NLP, Computer Vision, Conversational AI, AI Assistant, AI-Enhanced Learning, AI-powered Drug Discovery, Swarm AI."),
          h4("Limitations"),
          p("Counts by region/industry can exceed total companies because a company can appear in multiple categories. Hiring status indicates whether the YC profile marks the company as hiring, not the number or type of roles. Partial/unreleased batches (e.g., Spring/Summer 2026) are excluded.")
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  # Populate industry choices
  observe({
    industries <- companies %>%
      filter(!is.na(industries)) %>%
      separate_rows(industries, sep = ";\\s*") %>%
      distinct(industries) %>%
      arrange(industries) %>%
      pull(industries)
    updateSelectInput(session, "industry_select", choices = industries, selected = industries[1])
  })

  filtered_data <- reactive({
    df <- companies
    if (!is.null(input$year_range)) {
      df <- df %>% filter(is.na(batch_year) | (batch_year >= input$year_range[1] & batch_year <= input$year_range[2]))
    }
    if (!is.null(input$status_filter) && input$status_filter != "All") {
      df <- df %>% filter(status == input$status_filter)
    }
    if (!is.null(input$region_filter) && length(input$region_filter) > 0) {
      df <- df[sapply(df$regions_list, function(r) any(r %in% input$region_filter)), ]
    }
    df
  })

  output$ai_share_plot <- renderPlotly({
    df <- filtered_data() %>% filter(!is.na(batch_order))
    ai_by_batch <- df %>%
      group_by(batch_code, batch_order) %>%
      summarise(total = n(), ai = sum(has_ai), ai_pct = ai / total, .groups = "drop") %>%
      arrange(batch_order)
    p <- ggplot(ai_by_batch, aes(x = batch_code, y = ai_pct, group = 1)) +
      geom_line(color = "#1C4E80", linewidth = 1.2) +
      geom_point(color = "#1C4E80", size = 2) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(title = "AI Adoption by Batch", x = "Batch", y = "% AI Companies") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = c("x", "y"))
  })

  output$ai_tags_plot <- renderPlotly({
    df <- filtered_data() %>% filter(has_ai, !is.na(ai_tags))
    ai_tags_long <- df %>% separate_rows(ai_tags, sep = ";\\s*")
    tag_counts <- ai_tags_long %>% count(ai_tags, sort = TRUE) %>% head(input$top_ai_tags)
    p <- ggplot(tag_counts, aes(x = reorder(ai_tags, n), y = n)) +
      geom_col(fill = "#2E8B57") +
      coord_flip() +
      labs(title = "Top AI Subcategories", x = "", y = "Companies") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = c("x", "y"))
  })

  output$industry_trend_plot <- renderPlotly({
    df <- companies %>% filter(!is.na(batch_order))
    df <- df %>% filter(batch_year >= input$industry_year_range[1] & batch_year <= input$industry_year_range[2])
    ind_long <- df %>% separate_rows(industries, sep = ";\\s*")
    ind_trend <- ind_long %>%
      group_by(batch_code, batch_order, industries) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(batch_code) %>%
      mutate(pct = count / sum(count)) %>%
      ungroup()
    focus <- ind_trend %>% filter(industries == input$industry_select) %>% arrange(batch_order)
    p <- ggplot(focus, aes(x = batch_code, y = pct, group = 1)) +
      geom_line(color = "#1C4E80", linewidth = 1.2) +
      geom_point(color = "#1C4E80", size = 2) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(title = paste0("Industry Share: ", input$industry_select), x = "Batch", y = "% of Batch") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = c("x", "y"))
  })

  output$industry_top_plot <- renderPlotly({
    df <- companies %>% filter(!is.na(batch_order))
    df <- df %>% filter(batch_year >= input$industry_year_range[1] & batch_year <= input$industry_year_range[2])
    ind_long <- df %>% separate_rows(industries, sep = ";\\s*")
    top_inds <- ind_long %>% count(industries, sort = TRUE) %>% head(input$top_industries)
    p <- ggplot(top_inds, aes(x = reorder(industries, n), y = n)) +
      geom_col(fill = "#1C4E80") +
      coord_flip() +
      labs(title = "Top Industries (Selected Years)", x = "", y = "Companies") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = c("x", "y"))
  })

  output$hiring_plot <- renderPlotly({
    df <- companies %>% filter(!is.na(batch_order))
    df <- df %>% filter(batch_year >= input$hiring_year_range[1] & batch_year <= input$hiring_year_range[2])
    hiring <- df %>%
      group_by(batch_code, batch_order, has_ai) %>%
      summarise(hiring_pct = mean(is_hiring, na.rm = TRUE), .groups = "drop") %>%
      arrange(batch_order)
    hiring$group <- ifelse(hiring$has_ai, "AI", "Non-AI")
    p <- ggplot(hiring, aes(x = batch_code, y = hiring_pct, color = group, group = group)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(title = "Hiring Share by Batch", x = "Batch", y = "% Hiring", color = "Group") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = c("x", "y", "color"))
  })

  output$status_plot <- renderPlotly({
    df <- companies
    status <- df %>%
      group_by(status, has_ai) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(has_ai) %>%
      mutate(pct = count / sum(count)) %>%
      ungroup()
    status$group <- ifelse(status$has_ai, "AI", "Non-AI")
    p <- ggplot(status, aes(x = group, y = pct, fill = status)) +
      geom_col(position = "stack") +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(title = "Status Breakdown", x = "", y = "% of Group", fill = "Status") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })

  output$region_plot <- renderPlotly({
    df <- companies
    if (isTRUE(input$geo_ai_only)) {
      df <- df %>% filter(has_ai)
    }
    regions <- df %>%
      filter(!is.na(regions)) %>%
      separate_rows(regions, sep = ";\\s*") %>%
      count(regions, sort = TRUE) %>%
      head(input$geo_top)
    p <- ggplot(regions, aes(x = reorder(regions, n), y = n)) +
      geom_col(fill = "#1C4E80") +
      coord_flip() +
      labs(title = "Top Regions", x = "", y = "Companies") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = c("x", "y"))
  })

  output$company_table <- renderDT({
    display <- companies %>%
      select(name, batch, status, is_hiring, team_size, tags, industries, regions, one_liner, url, website, ai_tags)
    datatable(
      display,
      filter = "top",
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)
