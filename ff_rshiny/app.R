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
stats <- load_player_stats()
#dplyr::glimpse(stats)
#head(stats)
order_stats <- stats[order(stats["player_name"]),]
p_names <- c(unique(order_stats[["player_name"]]))

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    selectInput("p_select", 
                label = h3("Select Player"), 
                choices = p_names, 
                selected = 1),
    
    hr(),
    fluidRow(column(3, verbatimTextOutput("value")))
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    ),
    #fluidRow(
      #box(dataTableOutput("table_py"))
    #)
    fluidRow(
      box(plotOutput("plot_py", height = 250))
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  stats <- load_player_stats()
  
  p_data <- reactive({
    stats %>% filter(stats['player_name'] == input$p_select)
  })
  
  output$table_py <- renderDataTable({
    p_data()
  })
  
  output$plot_py <- renderPlot({
    plot(p_data()$week, p_data()$passing_yards)
  })
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)
