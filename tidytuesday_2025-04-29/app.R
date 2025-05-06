#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library(shiny)
# library(tidyverse)
# 
# user2025 <- read_csv("data/user2025.csv")
# 
# glimpse(user2025)
# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # App title
#     titlePanel("TidyTuesday 29-04-2025 -- SessionS by Date"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             dateRangeInput(
#               inputId = "date_range",
#               label = "Filter sessions by date:",
#               start = min(user2025$date, na.rm = TRUE),
#               end = max(user2025$date, na.rm = TRUE)
#             )
#         ),
# 
#         mainPanel(
#            plotOutput("sessionsPlot")
#         )
#     )
# )
# 
# # Server
# server <- function(input, output) {
# 
#     filtered_data <- reactive({
#       user2025 %>%
#         filter(date >= input$date_range[1], date <= input$date_range[2])
#     })
#     
#     output$sessionsPlot <- renderPlot({
#       filtered_data() %>%
#         count(date) %>%
#         ggplot(aes(x = date, y = n)) +
#         geom_col(fill = "steelblue") +
#         labs(title = "Sessions per date", x = "Date", y = "Number of Sessions") +
#         theme_minimal()
#     })
# }
# 
# # Run the application 
# shinyApp(ui, server)
