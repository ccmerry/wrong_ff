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
library(stringr)
library(shinyjs)
stats <- load_player_stats()
#dplyr::glimpse(stats)
#head(stats)
#order_stats <- stats[order(stats["player_name"]),]
#p_names <- c(unique(order_stats[["player_name"]]))
p_names <- c(unique(stats[["player_display_name"]]))


col_pretty <- data.frame(og_name = c("passing_yards", "passing_tds", "rushing_yards", 
                                     "fantasy_points", "fantasy_points_ppr"),
                         new_name = c("Passing Yards", "Passing TDs", "Rushing Yards",
                                      "Fantasy Points", "Fantasy Points PPR"),
                         avg_name = c("Pass Yds/Game", "PassTD/Game", "RshYds/Game",
                                      "FntsyPts/Game","FntsyPtsPPR/Game")
)


if (interactive()) {
  ui <- dashboardPage(
    
    dashboardHeader(
      titleWidth = 350,
      title = "Fantasy Football Compare"),
    
    dashboardSidebar(
      useShinyjs(),
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
                                 "Passing TDs" = "passing_tds",
                                 "Rushing Yards" = "rushing_yards",
                                 "Fantasy Points" = "fantasy_points",
                                 "Fantasy Points PPR" = "fantasy_points_ppr"), 
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
      
      fluidRow(
        
        ##### First Player #####
        box(id = "box1",
            width = 3,
          fluidRow(align="center",
                   htmlOutput("name1")
                   ),
          fluidRow(align="center",
                   htmlOutput("picture1")
                   ),
          fluidRow(align="center",
                   textOutput("avg1")
                   )
          ),
        
        ##### Second Player #####
        box(id = "box2",
            width = 3,
            fluidRow(align="center",
                     htmlOutput("name2")
            ),
            fluidRow(align="center",
                     htmlOutput("picture2")
            ),
            fluidRow(align="center",
                     textOutput("avg2")
            )
        ),
        
        ##### Third Player #####
        box(id = "box3",
            width = 3,
            fluidRow(align="center",
                     htmlOutput("name3")
            ),
            fluidRow(align="center",
                     htmlOutput("picture3")
            ),
            fluidRow(align="center",
                     textOutput("avg3")
            )
        ),
        
        ##### Fourth Player #####
        box(id = "box4",
            width = 3,
            fluidRow(align="center",
                     htmlOutput("name4")
            ),
            fluidRow(align="center",
                     htmlOutput("picture4")
            ),
            fluidRow(align="center",
                     textOutput("avg4")
            )
        )
      ),
      
      # Boxes need to be put in a row (or column)
      fluidRow(
        box(width = 12,
            plotOutput("plot_py"))
        ),
      fluidRow(
        renderPlot("table_py")
      )
      )
    )
  
  
  server <- function(input, output) {
    
    ######## reactive functions #######
    
    p_data <- reactive({
      stats %>% filter(stats$player_display_name %in% input$p_id)
    })
    
    player_lists <- reactive({
      c(unique(p_data()$player_display_name))
    })
    
    player_pics <- reactive({
      c(unique(p_data()$headshot_url))
    })
    
    y_var <- reactive({
      input$y_stat
    })
    
    y_stat_clean <- reactive({
      col_pretty %>% filter(col_pretty$og_name == y_var())
    })
    
    y_stat_name <- reactive({
      y_stat_clean()[["new_name"]][1]
    })
    
    avg_name <- reactive({
      y_stat_clean()[["avg_name"]][1]
    })
    
    ################################
    
    p_sum <- reactive({
      p_data() %>%
        group_by(p_data()$player_display_name) %>%
        summarise_if(is.numeric, funs(sum))
      
      
    })
    
    observe({colnames(p_sum())[1] <- "new_pname"})
    
    ################################
    
    output$table_py <- renderPlot({
      ggplot() +
        geom_col(p_sum(),aes(p_sum()$new_pname, p_sum()$passing_yards))
    })
    
    ################################
    
    observe({
      if(is.na(player_lists()[1])){
        shinyjs::hide(id = "box1")
      }else{
        shinyjs::show(id = "box1")
      }
    })
    
    observe({
      if(is.na(player_lists()[2])){
        shinyjs::hide(id = "box2")
      }else{
        shinyjs::show(id = "box2")
      }
    })
    
    observe({
      if(is.na(player_lists()[3])){
        shinyjs::hide(id = "box3")
      }else{
        shinyjs::show(id = "box3")
      }
    })
    
    observe({
      if(is.na(player_lists()[4])){
        shinyjs::hide(id = "box4")
      }else{
        shinyjs::show(id = "box4")
      }
    })
    
    ########### Box 1 ###########
    
    first_avg <- reactive({
      stats %>% filter(stats$player_display_name == player_lists()[1])
    })
    
    output$name1 <- renderText({
      paste("<b>",player_lists()[1],"</b>")
    })
    
    src1 <- reactive({
      substring((player_pics()[1]), 1)
    })
    
    output$picture1 <- renderText({
      c('<img src="',src1(),'", height="90%", width="90%">')
    })
    
    output$avg1 <- renderText({
      paste(avg_name(),round(mean(first_avg()[[y_var()]]),2),sep = ": ")
    })
    
    ########### Box 2 ###########
    
    second_avg <- reactive({
      stats %>% filter(stats$player_display_name == player_lists()[2])
    })
    
    output$name2 <- renderText({
      paste("<b>",player_lists()[2],"</b>")
    })
    
    src2 <- reactive({
      substring((player_pics()[2]), 1)
    })
    
    output$picture2 <- renderText({
      c('<img src="',src2(),'", height="90%", width="90%">')
    })
    
    output$avg2 <- renderText({
      paste(avg_name(),round(mean(second_avg()[[y_var()]]),2),sep = ": ")
    })
    
    ########### Box 3 ###########
    
    third_avg <- reactive({
      stats %>% filter(stats$player_display_name == player_lists()[3])
    })
    
    output$name3 <- renderText({
      paste("<b>",player_lists()[3],"</b>")
    })
    
    src3 <- reactive({
      substring((player_pics()[3]), 1)
    })
    
    output$picture3 <- renderText({
      c('<img src="',src3(),'", height="90%", width="90%">')
    })
    
    output$avg3 <- renderText({
      paste(avg_name(),round(mean(third_avg()[[y_var()]]),2),sep = ": ")
    })
    
    ########### Box 4 ###########
    
    fourth_avg <- reactive({
      stats %>% filter(stats$player_display_name == player_lists()[4])
    })
    
    output$name4 <- renderText({
      paste("<b>",player_lists()[4],"</b>")
    })
    
    src4 <- reactive({
      substring((player_pics()[4]), 1)
    })
    
    output$picture4 <- renderText({
      c('<img src="',src4(),'", height="90%", width="90%">')
    })
    
    output$avg4 <- renderText({
      paste(avg_name(),round(mean(fourth_avg()[[y_var()]]),2),sep = ": ")
    })
    
    ############################
    
    output$plot_py <- renderPlot({
      ggplot(p_data(), aes(p_data()$week, p_data()[[y_var()]])) + 
        geom_point(aes(fill = p_data()$player_display_name), size = 5, shape = 21) +
        theme(legend.title=element_blank()) +
        xlab("Week") + ylab(y_stat_name())
    })
    
    ############################
    
    output$percent_plot <- renderPlot({
      ggplot(p_data(), aes(p_data()$week, p_data()[[y_var()]])) + 
        geom_point(aes(fill = p_data()$player_display_name), size = 5, shape = 21) +
        theme(legend.title=element_blank()) +
        xlab("Week") + ylab(y_stat_name())
    })
    
  }
  
  shinyApp(ui = ui, server = server)

}
