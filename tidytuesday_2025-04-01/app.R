library(shiny)
library(tidyverse)
library(ggplot2)

pokemon_df <- read_csv("data/pokemon_df.csv")

df_long <- pokemon_df %>%
  pivot_longer(
    cols = c(hp, attack, defense, special_attack, special_defense, speed),
    names_to = "stat",
    values_to = "value"
  )

gen_avg <- pokemon_df %>%
  group_by(generation_id) %>%
  summarise(across(
    c(hp, attack, defense, special_attack, special_defense, speed),
    mean,
    na.rm = TRUE
  )) %>%
  pivot_longer(-generation_id, names_to = "stat", values_to = "average")

ui <- fluidPage(
  titlePanel("Pokemon Stat Dashboard - TidyTuesday 01-04-2025"),
  fluidRow(
    column(
      width = 12,
      h4("This dashboard explores Pokémon stat design across generations using the 01-04-2025 TidyTuesday dataset."),
      p("We’ll look at how Pokemon stats are distributed, how they vary across generations, and visualise individual Pokemon profiles.")
    )
  ),
  
  fluidRow(
    column(
      width = 6,
      h3("Pokemon Power Grid"),
      selectInput(
        inputId = "selected_pokemon",
        label = "Choose a Pokemon:",
        choices = sort(unique(pokemon_df$pokemon)),
        selected = "Pikachu"
      ),
      plotOutput("powerGridPlot")
    ),
  
    column(
      width = 6,
      h3("Stat Distribution"),
      plotOutput("histogramPlot")
    )
  ),
  
  fluidRow(
    column(
      width = 12,
      h3("Average Stats by Generation"),
      plotOutput("generationPlot")
    )
  )
)

server <- function(input, output, session) {
  
  output$histogramPlot <- renderPlot({
    ggplot(df_long, aes(x = value, fill = stat)) +
      geom_histogram(bins = 30, color = "white", alpha = 0.8) +
      facet_wrap(~stat, scales = "free") +
      labs(
        title = "Distribution of Pokemon Stats",
        x = "Stat Value",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  output$generationPlot <- renderPlot({
    ggplot(gen_avg, aes(x = factor(generation_id), y = average, fill = stat)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      labs(
        title = "Average Pokémon Stats by Generation",
        x = "Generation",
        y = "Average Stat",
        fill = "Stat"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        legend.position = "right"
      )
  })
  
  output$powerGridPlot <- renderPlot({
    selected <- pokemon_df %>%
      filter(pokemon == input$selected_pokemon) %>%
      select(hp, attack, defense, special_attack, special_defense, speed) %>%
      pivot_longer(cols = everything(), names_to = "stat", values_to = "value")
    
    # Add manual grid layout (2 rows x 3 columns)
    layout_map <- tibble(
      stat = c("hp", "attack", "defense", "special_attack", "special_defense", "speed"),
      row = c(1, 1, 1, 2, 2, 2),
      col = c(1, 2, 3, 1, 2, 3)
    )
    
    selected <- left_join(selected, layout_map, by = "stat")
    
    ggplot(selected, aes(x = col, y = row, fill = value)) +
      geom_tile(width = 0.95, height = 0.95, color = "white") +
      geom_text(aes(label = paste(stat, value, sep = "\n")), color = "white", size = 5, fontface = "bold") +
      scale_fill_gradient(low = "#fcae91", high = "#fb6a4a") +
      scale_y_reverse() +
      coord_fixed() +  # Force square tiles
      theme_void() +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
      ) +
      labs(title = paste("Power Grid for", input$selected_pokemon))
  })
  
}

shinyApp(ui, server)