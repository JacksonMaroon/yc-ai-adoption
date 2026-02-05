# YC AI Adoption Dashboard (R Shiny)

A Shiny dashboard that analyzes AI adoption, industry trends, hiring signals, and geography across Y Combinator companies using the **yc-oss** GitHub API.

## Data Source
- YC OSS API: https://github.com/yc-oss/api
- Companies (all): https://yc-oss.github.io/api/companies/all.json
- Metadata: https://yc-oss.github.io/api/meta.json
- Epoch AI Benchmarks (ECI): https://epoch.ai/benchmarks
- METR Time Horizons: https://metr.org

The latest collection timestamp is stored in `data/yc_meta.json` under `last_updated`.

## Code Structure
- `global.R` — data loading and shared objects
- `ui.R` — interface
- `server.R` — logic and outputs

## Quick Start
1. Install R packages:
   - `shiny`, `dplyr`, `tidyr`, `stringr`, `ggplot2`, `plotly`, `DT`, `readr`, `jsonlite`, `scales`, `bslib`
2. Refresh the dataset (optional, but recommended):

```r
Rscript scripts/fetch_yc_data.R
```

3. Run the app:

```r
shiny::runApp("/Users/jacksonmaroon/yc-ai-adoption")
```

## CSV Snapshot
A timestamped CSV snapshot is stored at:
- `data/yc_companies.csv`

This is generated from the yc-oss API and includes computed columns:
- `has_ai`, `ai_tags`
- `batch_year`, `batch_season`, `batch_code`, `batch_order`
- `status_group`, `is_hiring`

## AI Tag Definition
AI companies are flagged if they include any of the following YC tags:
- AI
- Artificial Intelligence
- Machine Learning
- Deep Learning
- Generative AI
- AIOps
- NLP
- Computer Vision
- Conversational AI
- AI Assistant
- AI-Enhanced Learning
- AI-powered Drug Discovery
- Swarm AI

## Methods
- **Trend test**: Linear regression of batch AI adoption rate on batch year.
- **Industry comparison**: Chi-square test of AI vs non-AI across top industries.
- **Hiring comparison**: Proportion test of hiring rates for AI vs non-AI companies.
- **Status comparison**: Chi-square test of AI vs non-AI across status categories.
- **Capabilities overlay**: Epoch Capabilities Index (ECI) and METR time horizon benchmarks, aggregated by model release year. We compare YC AI share vs frontier (best available to date) capability values for overlapping years.

## Notes
- Region and industry counts can exceed the number of companies because companies can have multiple regions/industries.
- Hiring status is a boolean from YC company profiles, not job-level roles.
- Partial/unreleased batches (Spring 2026 and Summer 2026) are excluded from analysis.
- Epoch AI benchmark data is licensed under CC-BY. Please cite Epoch AI when reusing outputs.
- ECI coverage in the bundled data begins in 2023; METR time horizon coverage begins in 2019.
