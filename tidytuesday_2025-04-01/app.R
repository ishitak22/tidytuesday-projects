library(shiny)
library(tidyverse)

pokemon_df <- read_csv("tidytuesday_2025-04-01/data/pokemon_df.csv")

df_long <- pokemon_df %>%
  pivot_longer(
    cols = c(hp, attack, defense, special_attack, special_defense, speed),
    names_to = "stat",
    values_to = "value"
  )

ui <- fluidPage(
  titlePanel("Pokemon Stat Dashboard - TidyTuesday 01-04-2025"),
  fluidRow(
    column(
      width = 12,
      h4("This dashboard explores Pokémon stat design across generations using the 01-04-2025 TidyTuesday dataset."),
      p("We’ll look at how Pokemon stats are distributed, how they vary across generations, and visualise individual Pokemon profiles."),
      h3("Stat Distribution"),
      plotOutput("histogramPlot")
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)