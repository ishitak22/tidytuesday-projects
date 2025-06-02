library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(readr)

# Load and prepare data
nsf_data <- read_csv("data/nsf_terminations.csv") %>%
  mutate(
    start = nsf_startdate,
    term = termination_letter_date,
    end = nsf_expected_end_date
  ) %>%
  select(-nsf_startdate, -termination_letter_date, -nsf_expected_end_date)

state_counts <- nsf_data %>%
  filter(!is.na(org_state)) %>%
  count(org_state, name = "termination_count") %>%
  rename(state = org_state)

state_counts_full <- state_counts %>%
  right_join(tibble(state = state.abb, full_name = state.name), by = "state") %>%
  mutate(termination_count = if_else(is.na(termination_count), 0, termination_count))

# UI
ui <- fluidPage(
  titlePanel("NSF Terminations Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dir", "Select a Directorate:", choices = unique(na.omit(nsf_data$directorate)))
    ),
    mainPanel(
      plotlyOutput("timeline_plot"),
      br(),
      plotlyOutput("state_heatmap"),
      br(),
      plotlyOutput("org_barplot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Timeline (Gantt)
  output$timeline_plot <- renderPlotly({
    filtered <- nsf_data %>%
      filter(directorate == input$dir) %>%
      arrange(start) %>%
      slice_head(n = 30) %>%
      mutate(label = paste(org_name, " - ", substr(grant_number, 1, 5)))
    
    p <- ggplot(filtered, aes(y = reorder(label, -start))) +
      geom_segment(aes(x = start, xend = term, y = label, yend = label), color = "black", size = 1) +
      geom_segment(aes(x = term, xend = end, y = label, yend = label), linetype = "dotted", color = "gray50") +
      geom_point(aes(x = start, y = label), color = "blue", size = 2) +
      geom_point(aes(x = term, y = label), color = "red", size = 2) +
      geom_point(aes(x = end, y = label), color = "darkgreen", size = 2) +
      labs(title = paste("Project Duration vs. Termination –", input$dir),
           x = "Date", y = "Projects") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 6))
    
    ggplotly(p)
  })
  
  # State Heatmap
  output$state_heatmap <- renderPlotly({
    plot_geo(state_counts_full, locationmode = "USA-states", source = "heatmap") %>%
      add_trace(z = ~termination_count, locations = ~state, text = ~paste(full_name, "<br>Terminations:", termination_count),
                hoverinfo = "text", color = ~termination_count, colors = c("yellow", "orange", "red", "darkred"),
                type = "choropleth") %>%
      layout(geo = list(scope = "usa"),
             title = "NSF Grant Terminations in U.S. States (2025)",
             margin = list(t = 50))
  })
  
  # Reactively track clicked state
  clicked_state <- reactive({
    event <- event_data("plotly_click", source = "heatmap")
    if (is.null(event)) return(NULL)
    clicked_index <- event$pointNumber + 1
    state_counts_full$state[clicked_index]
  })
  
  # Organization barplot
  output$org_barplot <- renderPlotly({
    req(clicked_state())
    
    nsf_data %>%
      filter(org_state == clicked_state()) %>%
      count(org_name, sort = TRUE) %>%
      slice_max(n, n = 30) %>%
      plot_ly(x = ~n, y = ~reorder(org_name, n), type = 'bar', orientation = 'h',
              text = ~paste("Terminations:", n),
              hoverinfo = "text", textposition = "none", marker = list(color = "firebrick")) %>%
      layout(
        title = list(text = paste("Top Organisations by Terminations in", clicked_state()), font = list(size = 16)),
        xaxis = list(title = "Number of Terminations"),
        yaxis = list(title = "Organisation", automargin = TRUE),
        margin = list(l = 100, r = 20, t = 60, b = 40)
      )
  })
}

# Run app
shinyApp(ui, server)