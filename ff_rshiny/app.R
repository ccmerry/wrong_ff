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
library(magrittr)

set.seed(1)
even_num <- c(2,4,6,8,0)
stats <- load_player_stats(seasons=c(2020,2021,2022))
p_names <- sort(c(unique(stats[["player_display_name"]])))

#creates method of consistently making the same player's stats inflated or deflated
stats %<>% mutate(num_rand = substr(player_id, nchar(player_id)-1+1, nchar(player_id)))
stats$even_odd <- ifelse(stats$num_rand %in% even_num, 1, -1)

#creates rand column and then creates percentage to multiply lg values(pass yds, rush yds, etc)
stats$lg_rnorm <- rnorm(nrow(stats), mean=0, sd=40)
stats$abs <- (abs(stats$lg_rnorm)/1000 * stats$even_odd) + 1

#changes lg stats by rand value
stats <- stats %>% mutate(new_passing_yards = round(stats$passing_yards * stats$abs))
stats <- stats %>% mutate(new_passing_yards_after_catch = round(stats$passing_yards_after_catch * stats$abs))
stats <- stats %>% mutate(new_receiving_yards = round(stats$receiving_yards * stats$abs))
stats <- stats %>% mutate(new_receiving_yards_after_catch = round(stats$receiving_yards_after_catch * stats$abs))
stats <- stats %>% mutate(new_receiving_air_yards = round(stats$receiving_air_yards * stats$abs))
stats <- stats %>% mutate(new_rushing_yards = round(stats$rushing_yards * stats$abs))
stats <- stats %>% mutate(new_fantasy_points = round(stats$fantasy_points * stats$abs))
stats <- stats %>% mutate(new_fantasy_points_ppr = round(stats$fantasy_points_ppr * stats$abs))

#filter out postseason games and last game of season
stats <- stats %>% filter(week < 18)

stats$single_change <- ifelse(stats$abs > .06, 1, 0)


col_pretty <- data.frame(og_name = c("new_passing_yards", "passing_tds","new_receiving_yards", 
                                     "new_rushing_yards", 
                                     "new_fantasy_points", "new_fantasy_points_ppr"),
                         new_name = c("Passing Yards", "Passing TDs", "Receiving Yards", 
                                      "Rushing Yards",
                                      "Fantasy Points", "Fantasy Points PPR"),
                         avg_name = c("Avg Pass Yds/Game", "Avg PassTD/Game", "Avg Rec Yds", 
                                      "Avg Rsh Yds/Game",
                                      "Avg FntsyPts/Game", "Avg FntsyPtsPPR/Game"),
                         med_name = c("Median Pass Yds", "Median PassTD", "Median Rec Yds", 
                                      "Median Rsh Yds",
                                      "Median FntsyPts", "Median FntsyPtsPPR")
)

col_ls <- c("completions","attempts","passing_yards","passing_tds","interceptions",
           "sacks","sack_yards","sack_fumbles","sack_fumbles_lost","passing_air_yards",
           "passing_yards_after_catch","passing_first_downs",
           "carries","rushing_yards","rushing_tds","rushing_fumbles","rushing_first_downs",
           "receptions","targets","receiving_yards","receiving_tds",
           "fantasy_points","fantasy_points_ppr")

col_l <- c("completions","attempts","new_passing_yards","passing_tds","interceptions",
           "new_passing_air_yards",
           "new_passing_yards_after_catch",
           "new_receiving_yards","new_receiving_air_yards","new_receiving_yards_after_catch",
           "carries","new_rushing_yards","rushing_tds","rushing_fumbles","rushing_first_downs",
           "new_fantasy_points","new_fantasy_points_ppr")

col_equals <- c("Completions","Attempts","Passing Yards","Passing TDs","Interceptions",
                "Passing Air Yards",
                "Passing Yards after Catch",
                "Receiving Yards","Receiving Air Yards","Receiving Yards after Catch",
                "Carries","Rushing Yards","Rushing TDs","Rushing Fumbles","Rushing First Downs",
                "Fantasy Points","Fantasy Points PPR")

c_name <- data.frame(stats = col_l,
                     t_name = col_equals)

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
        width = "100%",
        options = list(limit = 4)
        ),
      
      fluidRow(
        column(width =6,
               checkboxGroupInput("chartGraphs", 
                                  label = h3("Chart"), 
                                  choices = list("Stat by Week" = "sWeek", "Overview" = "oView")
                                  )
               ),
        
      column(width = 6,
             checkboxGroupInput("yrCheck", 
                                label = h3("Season"), 
                                choices = list("2022" = 2022, "2021" = 2021, "2020" = 2020),
                                selected = 2022
                                )
             )
      ),
      
      
      selectInput("y_stat", 
                  label = h3("Select Stat"), 
                  choices = list("Passing Yards" = "new_passing_yards",
                                 "Passing TDs" = "passing_tds",
                                 "Receiving Yards" = "new_receiving_yards",
                                 "Rushing Yards" = "new_rushing_yards",
                                 "Fantasy Points" = "new_fantasy_points",
                                 "Fantasy Points PPR" = "new_fantasy_points_ppr"), 
                  selected = 1)
      
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
                         ),
                fluidRow(align="center",
                         textOutput("med1")
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
                         ),
                fluidRow(align="center",
                         textOutput("med2")
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
                         ),
                fluidRow(align="center",
                         textOutput("med3")
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
                         ),
                fluidRow(align="center",
                         textOutput("med4")
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
        div(id = "sea_wrap",
            box(id = "seaPoint",
                width = 12,
                plotOutput("sea_pt"))
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
    
    #Make sure at least one year is checked
    observe({
      if(length(input$yrCheck) < min_yr){
        updateCheckboxGroupInput(session, "yrCheck", selected= "2022")
      }
    })
    
    ######## reactive functions #######
    
    p_data_n <- reactive({
      stats %>% filter(stats$player_display_name %in% input$p_id)
    })
    
    #filter for year
    p_data <- reactive({
      p_data_n() %>% filter(p_data_n()$season %in% input$yrCheck)
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
    
    med_name <- reactive({
      y_stat_clean()[["med_name"]][1]
    })
    
    ################################
    
    p_sum <- reactive({
      p_data() %>%
        group_by(p_data()$player_display_name) %>%
        summarise_if(is.numeric, funs(sum))
    })
    
    p_sum_yr <- reactive({
      p_data() %>%
        group_by(p_data()$player_display_name,p_data()$season) %>%
        summarise_if(is.numeric, funs(sum))
    })
    
    p_sum_yr_name <- reactive({
      p_sum_yr() %>% 
        rename(new_pname = 1, new_season = 2)
    })
    
    p_sum_name <- reactive({
      p_sum() %>% 
        rename(new_pname = 1)
    })
    
    p_sum_long <- reactive({
      p_sum_name() %>% 
        pivot_longer(
          cols = "season":"new_fantasy_points_ppr", 
          names_to = "stats",
          values_to = "value"
          )
      })
    
    #p_merge_s <- reactive({
      #merge(x = p_sum_long(), y = c_name, 
            #by.x = stats, by.y = f_name)
      #})
    
    p_merge_s <- reactive({
      p_sum_long() %>% left_join(c_name, by = 'stats')
      })
    
    long_f <- reactive({
      p_merge_s() %>%
        filter(value > 0,stats %in% col_l)
    })
    
    sg <- reactive({
      long_f() %>% 
        group_by(stats) %>% 
        mutate(perc = value/sum(value))
    })
    
    p_sum_f <- reactive({
      sg() #%>%
        #filter(sg()$t_name != NULL)
    })
    
    
    
    ################################
    
    output$bar_oview <- renderPlot({
      ggplot(p_sum_f(),
             aes(p_sum_f()$perc, y = reorder(p_sum_f()$t_name,desc(p_sum_f()$t_name)), 
                 fill = p_sum_f()$new_pname)) +
        geom_col() + 
        geom_text(aes(label=paste0(p_sum_f()$value)),
                  position=position_stack(vjust=0.5),
                  colour = "white") +
        scale_x_continuous(expand = c(0, 0)) +
        guides(fill=guide_legend(title="")) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_blank(),
              axis.title.y = element_blank()) + 
        scale_fill_manual(values=c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600"))
    })
    
    ################################
    
    #Hide/Show point plot
    observe({
      if(length(input$chartGraphs) < min_yr){
        shinyjs::hide(id = "point_wrap")
        shinyjs::hide(id = "sea_wrap")
        shinyjs::hide(id = "bar_wrap")
      }else{
        if("sWeek" %in% input$chartGraphs){
          if(length(input$yrCheck) > 1){
            shinyjs::show(id = "sea_wrap")
            shinyjs::hide(id = "point_wrap")
          }else{
            shinyjs::show(id = "point_wrap")
            shinyjs::hide(id = "sea_wrap")
          }
        }else{
          shinyjs::hide(id = "point_wrap")
          shinyjs::hide(id = "sea_wrap")
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
      p_data() %>% filter(p_data()$player_display_name == player_lists()[1])
    })
    
    first_p <- reactive({
      c(unique(first_avg()$headshot_url))
    })
    
    output$name1 <- renderText({
      paste("<b>",player_lists()[1],"</b>")
    })
    
    src1 <- reactive({
      substring((first_p()[1]), 1)
    })
    
    output$picture1 <- renderText({
      c('<img src="',src1(),'", height="90%", width="90%">')
    })
    
    output$avg1 <- renderText({
      paste(avg_name(),round(mean(first_avg()[[y_var()]]),2),sep = ": ")
    })
    
    output$med1 <- renderText({
      paste(med_name(),round(median(first_avg()[[y_var()]]),2),sep = ": ")
    })
    
    ########### Box 2 ###########
    
    second_avg <- reactive({
      p_data() %>% filter(p_data()$player_display_name == player_lists()[2])
    })
    
    second_p <- reactive({
      c(unique(second_avg()$headshot_url))
    })
    
    output$name2 <- renderText({
      paste("<b>",player_lists()[2],"</b>")
    })
    
    src2 <- reactive({
      substring((second_p()[1]), 1)
    })
    
    output$picture2 <- renderText({
      c('<img src="',src2(),'", height="90%", width="90%">')
    })
    
    output$avg2 <- renderText({
      paste(avg_name(),round(mean(second_avg()[[y_var()]]),2),sep = ": ")
    })
    
    output$med2 <- renderText({
      paste(med_name(),round(median(second_avg()[[y_var()]]),2),sep = ": ")
    })
    
    ########### Box 3 ###########
    
    third_avg <- reactive({
      p_data() %>% filter(p_data()$player_display_name == player_lists()[3])
    })
    
    third_p <- reactive({
      c(unique(third_avg()$headshot_url))
    })
    
    output$name3 <- renderText({
      paste("<b>",player_lists()[3],"</b>")
    })
    
    src3 <- reactive({
      substring((third_p()[1]), 1)
    })
    
    output$picture3 <- renderText({
      c('<img src="',src3(),'", height="90%", width="90%">')
    })
    
    output$avg3 <- renderText({
      paste(avg_name(),round(mean(third_avg()[[y_var()]]),2),sep = ": ")
    })
    
    output$med3 <- renderText({
      paste(med_name(),round(median(third_avg()[[y_var()]]),2),sep = ": ")
    })
    
    ########### Box 4 ###########
    
    fourth_avg <- reactive({
      p_data() %>% filter(p_data()$player_display_name == player_lists()[4])
    })
    
    fourth_p <- reactive({
      c(unique(fourth_avg()$headshot_url))
    })
    
    output$name4 <- renderText({
      paste("<b>",player_lists()[4],"</b>")
    })
    
    src4 <- reactive({
      substring((fourth_p()[1]), 1)
    })
    
    output$picture4 <- renderText({
      c('<img src="',src4(),'", height="90%", width="90%">')
    })
    
    output$avg4 <- renderText({
      paste(avg_name(),round(mean(fourth_avg()[[y_var()]]),2),sep = ": ")
    })
    
    output$med4 <- renderText({
      paste(med_name(),round(median(fourth_avg()[[y_var()]]),2),sep = ": ")
    })
    
    ############################
    
    output$plot_pt <- renderPlot({
      ggplot(p_data(), aes(p_data()$week, p_data()[[y_var()]])) + 
        geom_point(aes(fill = p_data()$player_display_name), size = 5, shape = 21) +
        theme(legend.title=element_blank()) +
        xlab("Week") + ylab(y_stat_name())
    })
    
    #output$sea_pt <- renderPlot({
      #ggplot(p_sum_yr_name(), aes(p_sum_yr_name()$new_season, p_sum_yr_name()[[y_var()]])) + 
        #geom_point(aes(fill = p_sum_yr_name()$new_pname), size = 5, shape = 21) +
        #theme(legend.title=element_blank()) +
        #xlab("Season") + ylab(y_stat_name())
    #})
    
    output$sea_pt <- renderPlot({
      ggplot(p_data(), aes(p_data()[[y_var()]], p_data()$player_display_name)) +
        geom_point(aes(fill = p_data()$player_display_name),size=5, shape = 21) +
        #scale_colour_manual(values = c("black", "black", "black", "black", "black"))+
        guides(fill=guide_legend(title="")) +
        theme(legend.position = "none",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title.y = element_blank()) + 
        xlab(y_stat_name()) +
        scale_fill_manual(values=alpha(c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600"),.2))
    })
    
    ############################
    
  }
  
  shinyApp(ui = ui, server = server)

}
