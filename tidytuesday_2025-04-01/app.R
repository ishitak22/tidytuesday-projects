library(shiny)
library(tidyverse)

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
      width = 12,
      h3("Stat Distribution"),
      plotOutput("histogramplot")
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
}

shinyApp(ui, server)