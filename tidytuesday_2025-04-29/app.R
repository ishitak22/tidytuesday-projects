# app.R

library(shiny)
library(tidyverse)
library(stringr)

user2025 <- read_csv("data/user2025.csv")

ui <- fluidPage(
  sliderInput("top_key", "Number of top keywords to display:", min = 5, max = 30, value = 10),
  plotOutput("keyword_plot")
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
      geom_col(fill = "darkorange") +
      coord_flip() +
      labs(title = "Top Keywords", x = "Keyword", y = "Count") +
      theme_minimal(base_size = 13)
  })
}

shinyApp(ui, server)