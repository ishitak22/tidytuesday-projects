library(shiny)
library(tidyverse)
library(ggplot2)
library(ggimage)
library(httr)
library(bslib)

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

is_valid_url <- function(url) {
  tryCatch({
    status_code(HEAD(url)) == 200
  }, error = function(e) FALSE)
}

ui <- fluidPage(
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Press+Start+2P&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
    body {
      background: linear-gradient(to right, #e0c3fc, #8ec5fc);
      font-family: 'Verdana', sans-serif;
    }
    h1, h2, h3 {
      font-family: 'Press Start 2P', cursive;
      color: #4b0082;
    }
      .mewtwo-img {
  position: fixed;
  top: 10px;
  right: 20px;
  width: 180px;  
  opacity: 0.95;
  z-index: 1000;
}
    "))
  ),
  
  tags$img(src = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/other/official-artwork/150.png", 
           class = "mewtwo-img"),
  
  
  titlePanel("Pokemon Stat Dashboard - TidyTuesday 01-04-2025"),
  
  fluidRow(
    column(
      width = 12,
      h4("This dashboard explores Pokemon Stat design across generations using the 01-04-2025 TidyTuesday dataset."),
      p("We’ll look at how Pokemon stats are distributed, how they vary across generations, what are the extreme pokemon cases and visualise individual Pokemon profiles.")
    )
  ),
  
  fluidRow(
    column(
      width = 8,
      h3("Outlier Pokemon by Stat"),
      plotOutput("outlierPlot", height = "500px")
    ),
    
    column(
      width = 4,
      h3("Pokemon Power Grid"),
      selectInput(
        inputId = "selected_pokemon",
        label = "Choose a Pokemon:",
        choices = sort(unique(pokemon_df$pokemon)),
        selected = "Pikachu"
      ),
      plotOutput("powerGridPlot", height = "300px"),
      
      br(),
      div(style = "
     margin-top: 10px;
     margin-left: 0px;
     padding: 10px 15px;
     background-color: rgba(255, 255, 255, 0.88);
     border-left: 4px solid #8e44ad;
     border-radius: 10px;
     font-size: 12px;
     line-height: 1.4;
     color: #2c3e50;
     box-shadow: 0 2px 4px rgba(0,0,0,0.08);
     max-width: 100%;",
          span("📊 Insight:", style = "font-weight: bold; font-size: 14px; color: #8e44ad;"),
          br(), "Special Attack and Speed show wide variation across Pokemon, while Defense is more tightly clustered in the Stat Distribution plot.",
          br(), "Generation 7 shows the highest average stats, reflecting evolving game design trends.",
          br(), "The boxplot reveals standout outliers like Blissey for HP and Deoxys for Speed, indicating Pokemon with extreme specialisations.",
          br(), "The Power Grid offers a quick comparison of individual Pokemon profiles which is useful for spotting balanced versus highly skewed stat spreads."
      )
    )
  ),
  
  fluidRow(
    column(
      width = 4,
      h3("Stat Distribution"),
      plotOutput("histogramPlot", height = "400px")
    ),
    
    column(
      width = 8,
      h3("Average Stats by Generation"),
      plotOutput("generationPlot", height = "400px")
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
        title = "Average Pokemon Stats by Generation",
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
      coord_fixed() +  
      theme_void() +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
      ) +
      labs(title = paste("Power Grid for", input$selected_pokemon))
  })
  
  output$outlierPlot <- renderPlot({
    stat_data <- pokemon_df %>%
      select(pokemon, url_image, hp, attack, defense, special_attack, special_defense, speed) %>%
      pivot_longer(cols = c(hp, attack, defense, special_attack, special_defense, speed),
                   names_to = "stat", values_to = "value") %>%
      filter(!is.na(url_image), url_image != "")
    
    outliers <- stat_data %>%
      group_by(stat) %>%
      mutate(Q1 = quantile(value, 0.25),
             Q3 = quantile(value, 0.75),
             IQR = Q3 - Q1,
             is_outlier = value < (Q1 - 1.5 * IQR) | value > (Q3 + 1.5 * IQR)) %>%
      filter(is_outlier) %>%
      ungroup() %>%
      filter(map_lgl(url_image, is_valid_url))
    
    ggplot(stat_data, aes(x = stat, y = value)) +
      geom_boxplot(outlier.shape = NA, fill = "#e0e0e0") +
      ggimage::geom_image(data = outliers, aes(image = url_image), size = 0.05) +
      labs(title = "Stat Outliers by Pokemon", x = "Stat", y = "Value") +
      theme_minimal(base_size = 14)
  })
  
  
}

shinyApp(ui, server)