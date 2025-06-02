# app.R

library(shiny)
library(tidyverse)
library(stringr)

user2025 <- read_csv("data/user2025.csv")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #2c3e50;
        color: white;
        font-family: 'Segoe UI', sans-serif;
      }
      h2 {
        font-weight: bold;
        text-align: center;
        color: white;
        margin-top: 20px;
      }
      label {
        color: white;
        font-weight: 500;
      }
      .irs--shiny .irs-bar, 
      .irs--shiny .irs-from, 
      .irs--shiny .irs-to, 
      .irs--shiny .irs-single {
        background-color: orange;
        border-color: orange;
      }
      .irs--shiny .irs-min, 
      .irs--shiny .irs-max {
        color: white;
      }
    "))
  ),
  
  titlePanel("🔍 Top Keywords at useR! 2025"),
  
  # Slider above plot
  fluidRow(
    column(12,
           sliderInput("top_key", "Number of top keywords to display:", min = 5, max = 30, value = 10)
    )
  ),
  
  fluidRow(
    column(12,
           plotOutput("keyword_plot", height = "500px")
    )
  )
)

server <- function(input, output) {
  output$keyword_plot <- renderPlot({
    keyword_counts <- user2025 %>%
      separate_rows(keywords, sep = ",") %>%
      mutate(keywords = str_squish(str_trim(keywords))) %>%
      count(keywords, sort = TRUE)
    
    keyword_counts %>%
      slice_max(n, n = input$top_key) %>%
      mutate(keyword_short = str_wrap(str_trunc(keywords, width = 40), width = 20)) %>%
      ggplot(aes(x = reorder(keyword_short, n), y = n)) +
      geom_col(fill = "orange") +
      coord_flip() +
      labs(title = "Top Keywords", x = "Keyword", y = "Count") +
      theme_minimal(base_size = 13)
  })
}

shinyApp(ui, server)