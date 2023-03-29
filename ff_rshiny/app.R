#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## app.R ##
library(shiny)
library(shinydashboard)
library(nflfastR)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(shinyWidgets)
stats <- load_player_stats()
#dplyr::glimpse(stats)
#head(stats)
#order_stats <- stats[order(stats["player_name"]),]
#p_names <- c(unique(order_stats[["player_name"]]))
p_names <- c(unique(stats[["player_display_name"]]))

if (interactive()) {
  
ui <- dashboardPage(
  
  dashboardHeader(
    titleWidth = 350,
    title = "Fantasy Football Compare"),
  
  dashboardSidebar(
    
    width = 350,
    
    multiInput(
      inputId = "p_id", 
      label = "Player :",
      choices = p_names,
      selected = NULL, 
      choiceValues = p_names2,
      width = "100%"
    ),
    
    br(),
    
    selectInput("y_stat", 
                label = h3("Select Stat"), 
                choices = list("Passing Yards" = "passing_yards", 
                               "Rushing Yards" = "rushing_yards"), 
                selected = 1),
    
    hr()
    
  ),
  
  dashboardBody(
    
    tags$head(
    tags$style(HTML('
            .skin-blue .sidebar a {
                color: #0000FF;
            }',
                              '
            .skin-blue .search-input {
                color: #000000;
            }',
                              '
            .skin-blue .multi-wrapper {
                color: #0000FF;
            }'
                              
    ))),
    
    br(),
    #verbatimTextOutput(outputId = "res"),
    
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot_py"))
    )
  )
)

server <- function(input, output) {
  
  p_data <- reactive({
    stats %>% filter(stats$player_display_name %in% input$p_id)
  })
  
  y_var <- reactive({
    input$y_stat
  })
  
  output$table_py <- renderDataTable({
    p_data()
  })
  
  output$plot_py <- renderPlot({
    ggplot(p_data(), aes(p_data()$week, p_data()[[y_var()]], colour = p_data()$player_display_name)) + 
      geom_point()
  })
}

shinyApp(ui = ui, server = server)

}
