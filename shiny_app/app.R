# app.R

library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

ui <- fluidPage(
  titlePanel("Biotoxin depuration forecaster and monitoring scheduler"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = "tox0",
        label = "Recent toxicity level",
        value = 125,
        min = 0,
        step = 0.1
      ),
      numericInput(
        inputId = "k",
        label = "Depuration rate, k (per day)",
        value = 0.03,
        min = 0.000001,
        step = 0.01
      ),
      numericInput(
        inputId = "action_level",
        label = "Action level (safe-to-eat threshold)",
        value = 30,
        min = 0.000001,
        step = 0.1
      ),
      tags$hr(),
      numericInput(
        inputId = "test_interval",
        label = "Interval between tests (days)",
        value = 7,
        min = 0.1,
        step = 0.1
      ),
      numericInput(
        inputId = "n_before",
        label = "Number of tests before crossing action level",
        value = 2,
        min = 0,
        step = 1
      ),
      numericInput(
        inputId = "n_after",
        label = "Number of tests after crossing action level",
        value = 2,
        min = 0,
        step = 1
      )
    ),
    
    mainPanel(
      plotOutput("forecast_plot", height = "550px", width="750px")
    )
  )
)

server <- function(input, output, session) {
  
  forecast_data <- reactive({
    req(input$tox0, input$k, input$action_level, input$test_interval)
    
    tox0 <- input$tox0
    k <- input$k
    action_level <- input$action_level
    test_interval <- input$test_interval
    n_before <- input$n_before
    n_after <- input$n_after
    
    # Day toxicity is forecast to fall below the action level
    crossing_day <- if (tox0 > action_level) {
      log(tox0 / action_level) / k
    } else {
      0
    }
    
    # Extend x-axis until toxicity reaches X% of the action level
    target_perc <- 0.2
    target_tox <- target_perc * action_level
    
    horizon <- if (tox0 > target_tox) {
      log(tox0 / target_tox) / k
    } else {
      0
    }
    
    horizon <- ceiling(horizon)
    
    days <- seq(0, horizon, by = 0.1)
    curve_df <- tibble(
      day = days,
      toxicity = tox0 * exp(-k * day)
    )
    
    # Build full expected testing schedule starting at day 0
    test_days <- seq(0, horizon, by = test_interval)
    
    test_df <- tibble(day = test_days) %>%
      mutate(
        toxicity = tox0 * exp(-k * day),
        test_num = row_number()
      )
    
    # Identify optimized tests:
    # - always include day 0
    # - include the desired number before crossing
    # - include the desired number after crossing
    tests_before_cross <- which(test_df$day < crossing_day & test_df$day > 0)
    tests_after_cross  <- which(test_df$day >= crossing_day)
    
    optimized_idx <- c(
      1,  # always day 0
      tail(tests_before_cross, n_before),
      head(tests_after_cross, n_after)
    ) %>%
      unique() %>%
      sort()
    
    test_df <- test_df %>%
      mutate(
        status = ifelse(row_number() %in% optimized_idx, "Optimized", "Unneeded"),
        point_fill = ifelse(status == "Optimized", "Optimized", "Unneeded"),
        day_label = ifelse(status == "Optimized", format(round(day, 1), nsmall = 1), NA)
      )
    
    list(
      curve_df = curve_df,
      test_df = test_df,
      crossing_day = crossing_day,
      horizon = horizon
    )
  })
  
  output$forecast_plot <- renderPlot({
    dat <- forecast_data()
    
    curve_df <- dat$curve_df
    test_df <- dat$test_df
    crossing_day <- dat$crossing_day
    
    crossing_tox <- input$tox0 * exp(-input$k * crossing_day)
    y_max <- max(curve_df$toxicity, na.rm = TRUE)
    
    # Build plot
    p <- ggplot(curve_df, aes(x = day, y = toxicity)) +
      # Decay curve
      geom_line(linewidth = 1.2) +
      # Action level line
      geom_hline(
        yintercept = input$action_level,
        linetype = "dashed",
        linewidth = 0.7,
        color = "grey40"
      ) +
      annotate(
        geom = "text",
        x = 0,
        y = input$action_level,
        label = "Action level",
        color = "grey40",
        hjust = 0,
        vjust = -0.5,
        size = 5
      ) +
      # Crossing day marker
      geom_vline(
        xintercept = crossing_day,
        linetype = "dotted",
        color="grey40",
        linewidth=0.7
      ) +
      annotate(
        "text",
        x = crossing_day - 0.01 * dat$horizon,
        y = 0.03 * y_max,
        label = paste0("Day of first clean test = ", round(crossing_day, 1)),
        hjust = 1,
        vjust = 0,
        size = 5,
        color="grey40"
      ) +
      # Test points
      geom_point(
        data = test_df,
        aes(x = day, y = toxicity, fill = status),
        shape = 21,
        size = 4,
        stroke = 1,
        color = "black"
      ) +
      # Labels for optimized tests only
      geom_text(
        data = filter(test_df, status == "Optimized" & day>0),
        aes(x = day, y = toxicity, label = day),
        vjust = -1,
        size = 5
      ) +
      # Crossing point
      # geom_point(
      #   data = tibble(day = crossing_day, toxicity = crossing_tox),
      #   aes(x = day, y = toxicity),
      #   size = 2.5
      # ) +
      # Labels
      labs(
        x = "Day of testing",
        y = "Toxicity",
        title = "Forecasted depuration timeline",
        fill = ""
      ) +
      # Scales
      scale_x_continuous(
        limits = c(0, dat$horizon),
        expand = c(0.05, 0)
      ) +
      scale_y_continuous(
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.08))
      ) +
      # Legeng
      scale_fill_manual(
        values = c("Optimized" = "black", "Unneeded" = "white")
      ) +
      # Theme
      theme_bw() +
      theme(
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        plot.title = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = NA, color = NA),
        legend.background = element_rect(fill = alpha("blue", 0)),
        legend.position = c(0.88, 0.88)
      )
    
    p
  })
}

shinyApp(ui = ui, server = server)