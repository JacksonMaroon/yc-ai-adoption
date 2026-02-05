server <- function(input, output, session) {
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

  output$industry_heatmap <- renderPlotly({
    df <- companies %>% filter(!is.na(batch_year))
    df <- df %>% filter(batch_year >= input$industry_year_range[1] & batch_year <= input$industry_year_range[2])
    ind_long <- df %>% separate_rows(industries, sep = ";\\s*")

    top_inds <- ind_long %>% count(industries, sort = TRUE) %>% head(input$heatmap_top)
    ind_long <- ind_long %>% filter(industries %in% top_inds$industries)

    heat <- ind_long %>%
      group_by(batch_year, industries) %>%
      summarise(ai_rate = mean(has_ai), total = n(), .groups = "drop") %>%
      complete(batch_year, industries, fill = list(ai_rate = NA_real_, total = 0))

    heat$industries <- factor(heat$industries, levels = rev(top_inds$industries))

    p <- ggplot(heat, aes(x = batch_year, y = industries, fill = ai_rate)) +
      geom_tile(color = "white", linewidth = 0.2) +
      scale_fill_gradient(low = "#E8F1F2", high = "#1C4E80",
                          labels = percent_format(accuracy = 1), na.value = "#F7F7F7") +
      labs(title = "AI Adoption Heatmap (Year x Industry)", x = "Batch Year", y = "Industry", fill = "% AI") +
      theme_minimal(base_size = 12)

    ggplotly(p, tooltip = c("x", "y", "fill"))
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

  # ---- Capabilities (Epoch ECI) ----
  capability_data <- reactive({
    req(nrow(capability_yearly) > 0)
    req(input$cap_year_range)
    req(input$cap_metric)

    ai_by_year <- companies %>%
      filter(!is.na(batch_year)) %>%
      group_by(batch_year) %>%
      summarise(total = n(), ai = sum(has_ai), ai_share = ai / total, .groups = "drop") %>%
      rename(year = batch_year)

    cap <- capability_yearly %>%
      rename(year = release_year)

    combined <- ai_by_year %>%
      inner_join(cap, by = "year") %>%
      arrange(year)

    combined <- combined %>%
      filter(year >= input$cap_year_range[1] & year <= input$cap_year_range[2])

    combined$metric_value <- dplyr::case_when(
      input$cap_metric == "eci_frontier_cum" ~ combined$eci_frontier_cum,
      input$cap_metric == "metr_frontier_cum" ~ combined$metr_frontier_cum,
      TRUE ~ NA_real_
    )

    combined <- combined %>% filter(!is.na(metric_value))

    combined$metric_label <- ifelse(
      input$cap_metric == "metr_frontier_cum",
      "METR Time Horizon",
      "Epoch ECI"
    )

    combined
  })

  output$capability_trend_plot <- renderPlotly({
    df <- capability_data()
    shiny::validate(shiny::need(nrow(df) > 1, "Not enough overlapping years for capability data."))

    metric_label <- unique(df$metric_label)
    metric_label <- metric_label[!is.na(metric_label)]
    metric_label <- if (length(metric_label) > 0) metric_label[1] else "Capability"

    shiny::validate(shiny::need(all(is.finite(df$ai_share)) && any(is.finite(df$metric_value)),
                                "Capability data unavailable."))

    plot_ly(df, x = ~year) %>%
      add_trace(
        y = ~ai_share,
        type = "scatter",
        mode = "lines+markers",
        name = "YC AI share",
        line = list(color = "#1C4E80", width = 2),
        marker = list(color = "#1C4E80", size = 6),
        hovertemplate = "Year: %{x}<br>AI share: %{y:.1%}<extra></extra>"
      ) %>%
      add_trace(
        y = ~metric_value,
        type = "scatter",
        mode = "lines+markers",
        name = metric_label,
        yaxis = "y2",
        line = list(color = "#D99000", width = 2),
        marker = list(color = "#D99000", size = 6),
        hovertemplate = paste0("Year: %{x}<br>", metric_label, ": %{y:.2f}<extra></extra>")
      ) %>%
      layout(
        title = "YC AI Share vs Frontier AI Capability",
        xaxis = list(title = "Year", tickmode = "array", tickvals = df$year),
        yaxis = list(title = "% YC Companies (AI)", tickformat = ".0%", rangemode = "tozero"),
        yaxis2 = list(title = metric_label, overlaying = "y", side = "right", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = 1.1)
      )
  })

  output$capability_scatter_plot <- renderPlotly({
    df <- capability_data()
    shiny::validate(shiny::need(nrow(df) > 1, "Not enough overlapping years for capability data."))

    cor_val <- suppressWarnings(cor(df$metric_value, df$ai_share, use = "complete.obs"))
    subtitle <- ifelse(is.finite(cor_val), paste0("Pearson r = ", round(cor_val, 2)), "Pearson r = NA")

    p <- ggplot(df, aes(x = metric_value, y = ai_share)) +
      geom_point(aes(text = paste0("Year: ", year,
                                   "<br>", metric_label, ": ", round(metric_value, 2),
                                   "<br>AI share: ", percent(ai_share, accuracy = 0.1))),
                 color = "#1C4E80", size = 3, alpha = 0.9) +
      geom_smooth(method = "lm", se = FALSE, color = "#D99000", linewidth = 1) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(title = "AI Share vs Capability (by Year)", subtitle = subtitle,
           x = "Capability metric (selected)", y = "% YC Companies (AI)") +
      theme_minimal(base_size = 12)

    ggplotly(p, tooltip = "text")
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

  # ---- Statistical Tests ----
  output$stats_cards <- renderUI({
    # Trend test
    batch_ai <- companies %>%
      filter(!is.na(batch_year), !is.na(batch_order)) %>%
      group_by(batch_code, batch_year, batch_order) %>%
      summarise(total = n(), ai = sum(has_ai), ai_pct = ai / total, .groups = "drop")
    trend_model <- lm(ai_pct ~ batch_year, data = batch_ai, weights = total)
    trend_coef <- coef(summary(trend_model))
    trend_slope_pp <- trend_coef["batch_year", "Estimate"] * 100
    trend_p <- trend_coef["batch_year", "Pr(>|t|)"]

    # Industry comparison (top industries)
    ind_long <- companies %>%
      filter(!is.na(industries)) %>%
      separate_rows(industries, sep = ";\\s*")
    top_inds <- ind_long %>% count(industries, sort = TRUE) %>% head(12)
    ind_sub <- ind_long %>% filter(industries %in% top_inds$industries)
    ind_tab <- table(ind_sub$industries, ind_sub$has_ai)
    ind_chi <- suppressWarnings(chisq.test(ind_tab))

    # Hiring comparison
    hire_df <- companies %>% filter(!is.na(is_hiring))
    ai_hire <- sum(hire_df$has_ai & hire_df$is_hiring)
    ai_total <- sum(hire_df$has_ai)
    non_hire <- sum(!hire_df$has_ai & hire_df$is_hiring)
    non_total <- sum(!hire_df$has_ai)
    hire_test <- prop.test(c(ai_hire, non_hire), c(ai_total, non_total))

    # Status comparison
    status_df <- companies %>% filter(!is.na(status))
    status_tab <- table(status_df$status, status_df$has_ai)
    status_chi <- suppressWarnings(chisq.test(status_tab))

    tagList(
      fluidRow(
        column(3,
               div(class = "card",
                   div(class = "muted", "Trend Test"),
                   div(class = "stat", paste0(round(trend_slope_pp, 2), " pp/yr")),
                   div(class = "muted", paste0("p = ", signif(trend_p, 3)))
               )
        ),
        column(3,
               div(class = "card",
                   div(class = "muted", "Industry Comparison"),
                   div(class = "stat", "Chi-square"),
                   div(class = "muted", paste0("p = ", signif(ind_chi$p.value, 3)))
               )
        ),
        column(3,
               div(class = "card",
                   div(class = "muted", "Hiring Difference"),
                   div(class = "stat", paste0(round((ai_hire / ai_total - non_hire / non_total) * 100, 2), " pp")),
                   div(class = "muted", paste0("p = ", signif(hire_test$p.value, 3)))
               )
        ),
        column(3,
               div(class = "card",
                   div(class = "muted", "Status Comparison"),
                   div(class = "stat", "Chi-square"),
                   div(class = "muted", paste0("p = ", signif(status_chi$p.value, 3)))
               )
        )
      )
    )
  })

  output$industry_rate_table <- renderDT({
    ind_long <- companies %>%
      filter(!is.na(industries)) %>%
      separate_rows(industries, sep = ";\\s*")
    top_inds <- ind_long %>% count(industries, sort = TRUE) %>% head(12)
    rates <- ind_long %>%
      filter(industries %in% top_inds$industries) %>%
      group_by(industries) %>%
      summarise(
        total = n(),
        ai = sum(has_ai),
        ai_rate = ai / total,
        .groups = "drop"
      ) %>%
      arrange(desc(ai_rate))

    datatable(
      rates %>% mutate(ai_rate = percent(ai_rate, accuracy = 0.1)),
      options = list(pageLength = 12, scrollX = TRUE),
      rownames = FALSE
    )
  })
}
