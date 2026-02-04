suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(stringr)
  library(readr)
})

all_url <- "https://yc-oss.github.io/api/companies/all.json"
meta_url <- "https://yc-oss.github.io/api/meta.json"

message("Downloading YC metadata...")
meta <- fromJSON(meta_url)
writeLines(toJSON(meta, pretty = TRUE, auto_unbox = TRUE), "data/yc_meta.json")

message("Downloading YC companies...")
companies <- fromJSON(all_url)

# Convert to data.frame and collapse list columns to '; '-separated strings
companies_df <- as.data.frame(companies, stringsAsFactors = FALSE)
list_cols <- names(companies_df)[sapply(companies_df, is.list)]
for (col in list_cols) {
  companies_df[[col]] <- vapply(
    companies_df[[col]],
    function(x) if (length(x) == 0) NA_character_ else paste(x, collapse = "; "),
    character(1)
  )
}

# AI tag set based on YC tag taxonomy in meta.json
ai_tag_set <- c(
  "AI",
  "Artificial Intelligence",
  "Machine Learning",
  "Deep Learning",
  "Generative AI",
  "AIOps",
  "NLP",
  "Computer Vision",
  "Conversational AI",
  "AI Assistant",
  "AI-Enhanced Learning",
  "AI-powered Drug Discovery",
  "Swarm AI"
)

# Match AI tags per company
raw_tags <- ifelse(is.na(companies_df$tags), "", companies_df$tags)
tag_list <- strsplit(raw_tags, ";\\s*")
companies_df$ai_tags <- vapply(tag_list, function(tags) {
  tags <- tags[tags != ""]
  hits <- tags[tags %in% ai_tag_set]
  if (length(hits) == 0) NA_character_ else paste(unique(hits), collapse = "; ")
}, character(1))
companies_df$has_ai <- !is.na(companies_df$ai_tags)

# Batch parsing for ordering
batch_match <- str_match(companies_df$batch, "^(Winter|Spring|Summer|Fall)\\s+(\\d{4})$")
companies_df$batch_season <- batch_match[, 2]
companies_df$batch_year <- as.integer(batch_match[, 3])
season_order <- c(Winter = 1, Spring = 2, Summer = 3, Fall = 4)
season_code <- c(Winter = "W", Spring = "Sp", Summer = "Su", Fall = "F")
companies_df$batch_order <- ifelse(
  !is.na(companies_df$batch_year),
  companies_df$batch_year * 10 + season_order[companies_df$batch_season],
  NA
)
companies_df$batch_code <- ifelse(
  !is.na(companies_df$batch_year),
  paste0(season_code[companies_df$batch_season], substr(companies_df$batch_year, 3, 4)),
  companies_df$batch
)

# Status grouping
companies_df$status_group <- ifelse(
  companies_df$status %in% c("Active", "Public", "Acquired"),
  "Active/Public/Acquired",
  "Inactive"
)

# Friendly snake_case for key fields (keep originals too)
companies_df$is_hiring <- companies_df$isHiring

message("Writing data/yc_companies.csv ...")
write_csv(companies_df, "data/yc_companies.csv")
message("Done.")
