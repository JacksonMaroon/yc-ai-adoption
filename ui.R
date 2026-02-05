ui <- navbarPage(
  title = "YC AI Adoption Dashboard",
  theme = app_theme,
  header = tagList(
    tags$style(HTML(
      ".card {border: 1px solid #E0E7EF; border-radius: 14px; padding: 18px; background: #fff; box-shadow: 0 6px 16px rgba(0,0,0,0.05);}"
    )),
    tags$style(HTML(
      ".stat {font-size: 26px; font-weight: 700;}\n.muted {color: #667085;}\n.hero {padding: 24px 26px; background: linear-gradient(120deg,#0F2D4D,#1C4E80); color: white; border-radius: 16px;}\n.hero h2 {color: #fff;}\n.section-title {margin-top: 18px; margin-bottom: 8px; font-weight: 700;}"
    ))
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
                      min = year_min,
                      max = year_max,
                      value = c(year_min, year_max),
                      sep = ""),
          selectInput("status_filter", "Company Status",
                      choices = c("All", sort(unique(companies$status))), selected = "All"),
          selectInput("region_filter", "Region (optional)",
                      choices = region_choices,
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
          selectInput("industry_select", "Industry", choices = industry_choices, selected = industry_choices[1]),
          sliderInput("industry_year_range", "Year Range",
                      min = year_min,
                      max = year_max,
                      value = c(year_min, year_max),
                      sep = ""),
          sliderInput("top_industries", "Top Industries (bar)", min = 5, max = 15, value = 10),
          sliderInput("heatmap_top", "Heatmap Industries", min = 6, max = 20, value = 12)
        ),
        mainPanel(
          width = 9,
          div(class = "card", plotlyOutput("industry_trend_plot", height = 360)),
          br(),
          div(class = "card", plotlyOutput("industry_top_plot", height = 360)),
          br(),
          div(class = "card", plotlyOutput("industry_heatmap", height = 420))
        )
      )
    )
  ),
  tabPanel(
    "Capabilities",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          sliderInput("cap_year_range", "Year Range",
                      min = cap_year_min,
                      max = cap_year_max,
                      value = c(cap_year_min, cap_year_max),
                      sep = ""),
          selectInput("cap_metric", "Capability Metric",
                      choices = capability_metric_choices,
                      selected = "eci_frontier_cum"),
          helpText(paste0("Frontier = best available to date. Years reflect model release dates. Coverage starts ", cap_year_min, "."))
        ),
        mainPanel(
          width = 9,
          div(class = "card",
              h4("YC AI Share vs AI Capability"),
              plotlyOutput("capability_trend_plot", height = 360),
              p(class = "muted", "Capability line is scaled to the share axis; right axis shows actual capability values.")
          ),
          br(),
          div(class = "card",
              h4("AI Share vs Capability (by Year)"),
              plotlyOutput("capability_scatter_plot", height = 360)
          )
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
                      min = year_min,
                      max = year_max,
                      value = c(year_min, year_max),
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
    "Statistics",
    fluidPage(
      fluidRow(
        column(12, uiOutput("stats_cards"))
      ),
      br(),
      fluidRow(
        column(12,
               div(class = "card",
                   h4("Industry AI Adoption Rates (Top Industries)"),
                   DTOutput("industry_rate_table")
               )
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
          h4("AI Capability Data"),
          p("Capability trends use Epoch AI's Benchmarking Hub (https://epoch.ai/benchmarks) and METR time horizon benchmarks, aggregated by model release year."),
          p("Citation (Epoch): Epoch AI, 'Data on AI Benchmarking'. Published online at epoch.ai. Retrieved from https://epoch.ai/benchmarks."),
          h4("AI Tag Definition"),
          p("AI companies are identified using YC's official AI-related tags: AI, Artificial Intelligence, Machine Learning, Deep Learning, Generative AI, AIOps, NLP, Computer Vision, Conversational AI, AI Assistant, AI-Enhanced Learning, AI-powered Drug Discovery, Swarm AI."),
          h4("Methods"),
          tags$ul(
            tags$li("Trend test: linear regression of AI adoption rate by batch year."),
            tags$li("Industry comparison: chi-square test of AI vs non-AI across top industries."),
            tags$li("Hiring comparison: proportion test of hiring rates for AI vs non-AI companies."),
            tags$li("Status comparison: chi-square test of AI vs non-AI across status categories.")
          ),
          h4("Limitations"),
          p("Counts by region/industry can exceed total companies because a company can appear in multiple categories. Hiring status indicates whether the YC profile marks the company as hiring, not the number or type of roles. Partial/unreleased batches (e.g., Spring/Summer 2026) are excluded. Epoch ECI coverage in the bundled data begins in 2023; METR time horizons begin in 2019.")
      )
    )
  )
)
