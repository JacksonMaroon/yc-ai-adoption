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

# UI choices
industry_choices <- companies %>%
  filter(!is.na(industries)) %>%
  separate_rows(industries, sep = ";\\s*") %>%
  distinct(industries) %>%
  arrange(industries) %>%
  pull(industries)

region_choices <- sort(unique(unlist(companies$regions_list)))

year_min <- min(companies$batch_year, na.rm = TRUE)
year_max <- max(companies$batch_year, na.rm = TRUE)

# ---- UI Theme ----
app_theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  base_font = font_google("Source Sans 3"),
  heading_font = font_google("Source Sans 3"),
  primary = "#1C4E80",
  secondary = "#E8F1F2"
)
