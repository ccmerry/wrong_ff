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
library(tidyr)
library(RColorBrewer)
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

col_ls <- c("completions","attempts","passing_yards","passing_tds","interceptions",
           "sacks","sack_yards","sack_fumbles","sack_fumbles_lost","passing_air_yards",
           "passing_yards_after_catch","passing_first_downs",
           "carries","rushing_yards","rushing_tds","rushing_fumbles","rushing_first_downs",
           "receptions","targets","receiving_yards","receiving_tds",
           "fantasy_points","fantasy_points_ppr")

col_l <- c("completions","attempts","passing_yards","passing_tds","interceptions",
           "sacks","sack_yards","sack_fumbles","sack_fumbles_lost","passing_air_yards",
           "passing_yards_after_catch","passing_first_downs",
           "carries","rushing_yards","rushing_tds","rushing_fumbles","rushing_first_downs",
           "fantasy_points","fantasy_points_ppr")

non_qb_ls <- c("carries","rushing_yards","rushing_tds","rushing_first_downs",
               "receptions","targets","receiving_yards","receiving_tds",
               "fantasy_points","fantasy_points_ppr")

min_yr = 1


#"player_id"                   "player_name"                 "player_display_name"        
#"position"                    "position_group"              "headshot_url"               
#"recent_team"                 "season"                      "week"                       
#"season_type"                 "completions"                 "attempts"                   
#"passing_yards"               "passing_tds"                 "interceptions"              
#"sacks"                       "sack_yards"                  "sack_fumbles"               
#"sack_fumbles_lost"           "passing_air_yards"           "passing_yards_after_catch"  
#"passing_first_downs"         "passing_epa"                 "passing_2pt_conversions"    
#"pacr"                        "dakota"                      "carries"                    
#"rushing_yards"               "rushing_tds"                 "rushing_fumbles"            
#"rushing_fumbles_lost"        "rushing_first_downs"         "rushing_epa"                
#"rushing_2pt_conversions"     "receptions"                  "targets"                    
#"receiving_yards"             "receiving_tds"               "receiving_fumbles"          
#"receiving_fumbles_lost"      "receiving_air_yards"         "receiving_yards_after_catch"
#"receiving_first_downs"       "receiving_epa"               "receiving_2pt_conversions"  
#"racr"                        "target_share"                "air_yards_share"            
#"wopr"                        "special_teams_tds"           "fantasy_points"             
#"fantasy_points_ppr" 


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
      
      
      selectInput("y_stat", 
                  label = h3("Select Stat"), 
                  choices = list("Passing Yards" = "passing_yards",
                                 "Passing TDs" = "passing_tds",
                                 "Rushing Yards" = "rushing_yards",
                                 "Fantasy Points" = "fantasy_points",
                                 "Fantasy Points PPR" = "fantasy_points_ppr"), 
                  selected = 1),
      
      
      checkboxGroupInput("yrCheck", 
                         label = h3("Season"), 
                         choices = list("2022" = 2022, "2021" = 2021, "2020" = 2020),
                         selected = 2022),
      
      
      checkboxGroupInput("chartGraphs", 
                         label = h3("Season"), 
                         choices = list("Stat by Week" = "sWeek", "Overview" = "oView")
                         )
      
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
                      }',
                      
                      
                      '
                      .content-wrapper, .right-side {
                      background-color: #E5E4E2;
                      }'
                      
                      ))),
      
      br(),
      
      fluidRow(
        
        ##### First Player #####
        div(id = "b1_wrap",
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
                )
          ),
        
        ##### Second Player #####
        div(id = "b2_wrap",
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
                )
            ),
        
        ##### Third Player #####
        div(id = "b3_wrap",
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
                )
            ),
        
        ##### Fourth Player #####
        div(id = "b4_wrap",
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
            )
      ),
      
      # Boxes need to be put in a row (or column)
      fluidRow(
        div(id = "point_wrap",
            box(id = "plotPoint",
                width = 12,
                plotOutput("plot_pt"))
            )
        ),
      fluidRow(
        div(id = "bar_wrap",
            box(id = "plotBar",
                width = 12,
                plotOutput("bar_oview"))
            )
        )
      )
    )
  
  
  
  
  ##########################################
  ################# SERVER #################
  ##########################################
  
  
  
  server <- function(input, output, session) {
    
    observe({
      if(length(input$yrCheck) < min_yr){
        updateCheckboxGroupInput(session, "yrCheck", selected= "2022")
      }
    })
    
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
    
    p_sum_name <- reactive({
      p_sum() %>% 
        rename(new_pname = 1)
    })
    
    p_sum_long <- reactive({
      p_sum_name() %>% 
        pivot_longer(
          cols = "season":"fantasy_points_ppr", 
          names_to = "stats",
          values_to = "value"
          )
      })
    
    long_f <- reactive({
      p_sum_long() %>%
        filter(value > 0,stats %in% col_l)
    })
    
    sg <- reactive({
      long_f() %>% 
        group_by(stats) %>% 
        mutate(perc = value/sum(value))
    })
    
    p_sum_f <- reactive({
      sg() #%>%
        #filter(value > 0)
    })
    
    
    
    ################################
    
    output$bar_oview <- renderPlot({
      ggplot(p_sum_f(),aes(p_sum_f()$perc, p_sum_f()$stats, fill = p_sum_f()$new_pname)) +
        geom_col() + 
        geom_text(aes(label=paste0(p_sum_f()$value)),
                  position=position_stack(vjust=0.5),
                  colour = "white") +
        scale_x_continuous(limits = c(0, 1)) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank()) + 
        scale_fill_manual(values=c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600"))
    })
    
    ################################
    
    #Hide/Show point plot
    observe({
      if(length(input$chartGraphs) < min_yr){
        shinyjs::hide(id = "point_wrap")
        shinyjs::hide(id = "bar_wrap")
      }else{
        if("sWeek" %in% input$chartGraphs){
          shinyjs::show(id = "point_wrap")
        }else{
          shinyjs::hide(id = "point_wrap")
        }
        if("oView" %in% input$chartGraphs){
          shinyjs::show(id = "bar_wrap")
        }else{
          shinyjs::hide(id = "bar_wrap")
        }
      }
    })
    
    #Hide/Show bar plot
    #observe({
      #if(input$chartGraphs == ){
        #shinyjs::show(id = "plotBar")
      #}else{
        #shinyjs::hide(id = "plotBar")
      #}
    #})
    
    
    observe({
      if(is.na(player_lists()[1])){
        shinyjs::hide(id = "b1_wrap")
      }else{
        shinyjs::show(id = "b1_wrap")
      }
    })
    
    observe({
      if(is.na(player_lists()[2])){
        shinyjs::hide(id = "b2_wrap")
      }else{
        shinyjs::show(id = "b2_wrap")
      }
    })
    
    observe({
      if(is.na(player_lists()[3])){
        shinyjs::hide(id = "b3_wrap")
      }else{
        shinyjs::show(id = "b3_wrap")
      }
    })
    
    observe({
      if(is.na(player_lists()[4])){
        shinyjs::hide(id = "b4_wrap")
      }else{
        shinyjs::show(id = "b4_wrap")
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
    
    output$plot_pt <- renderPlot({
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
