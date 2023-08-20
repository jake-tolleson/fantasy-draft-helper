# Load the required libraries
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(bslib)
library(reticulate)
library(shinydashboard)
library(fresh)
library(nflreadr)
library(lubridate)
options(shiny.autoreload = TRUE) 

current_season <- get_current_season()
current_week <- get_current_week()

if(month(today()) > 4 && current_season < year(today())){
  current_season <- current_season + 1
  current_week <- 0
}

if(current_week == 0){
  num_week_inputs <- 0
}else{
  num_week_inputs <- 1:current_week %>% as.character()
}

week_inputs <- num_week_inputs
week_inputs[1] <- 'Season'

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Settings", tabName = "settings", icon = icon("gears")),
    menuItem("Projections", icon = icon("chart-bar"), tabName = "projections"),
    hr(),
    h5("Fantasy League", style = "font-size:15px; color:#000000; text-align:center;"),
    menuItem("Lineup Optimizer", tabName = "lineup_optimizer", icon = icon("chart-simple"),
             badgeLabel = "new", badgeColor = "green"),
    menuItem("Draft Optimizer", tabName = "draft_optimizer", icon = icon("th")),
    menuItem("ESPN Live Draft", tabName = "live_draft", icon = icon("edge")),
    hr(),
    h5("Daily Fantasy Sports", style = "font-size:15px; color:#000000; text-align:center;"),
    hr(),
    menuItem("My Account", tabName = "my_account", icon = icon("user"))
  )
  
)

body <- dashboardBody(
  tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              font-family: use_googlefont("Nunito"), Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 18px;
                              background-color: #3D9970;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #3D9970;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #f8f8f8;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #f8f8f8;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #3D9970;
                              color: #f5f5f5;
                              border-radius: 4px;
                              margin-top: 5px;
                              margin-left: 7px;
                              margin-right: 7px;
        }
        
         /* active selected tab in the sidebarmenu hovered*/
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a:hover{
                              background-color: #3D9970;
                              color: #f5f5f5;
                              border-radius: 4px;
                              margin-top: 5px;
                              margin-left: 7px;
                              margin-right: 7px;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #f8f8f8;
                              color: #000000;
                              margin-top: 5px;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              color: #3D9970
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #ead1dc;
         }
        
        /* toggle button */                    
         .skin-blue .main-header .navbar .sidebar-toggle{
                              background-color: #f8f8f8;
                              color: #ead1dc;
                              '))),
  
  tabItems(
    tabItem(tabName = "settings"
            ,h2("Settings tab content")
            ,fluidRow(box(numericInput('year', h5("Season", style = "font-size:15px;"), current_season, min=current_season, max=current_season)
                          ,selectInput('week', h5("Week", style = "font-size:15px;"), week_inputs, num_week_inputs[which.max(num_week_inputs)])
                          ,numericInput('roster_size', h5('Roster Size', style = "font-size:15px;"), 16, min=7, max=20)
                          ,numericInput('num_teams', h5('Number of Teams', style = "font-size:15px;"), 12, min=4, max=14)
                          ,selectizeInput('positions', label=h5('Positions', style = "font-size:15px;"), choices=c("QB", "RB", "WR", "TE", "K", "DST"), selected=c("QB", "RB", "WR", "TE", "K", "DST"), multiple=T)
                          ,title='General', collapsible = T, solidHeader = T, width=4, collapsed=T)
                      ,box(selectInput('avg_type', h5('Average Type', style = "font-size:15px;"), choices=c("Weighted", "Mean", "Robust"), selected="Mean")
                           ,title='Data Aggregation', collapsible = T, solidHeader = T, width=4, collapsed=T)
                      ,box(
                        fluidRow(
                          column(numericInput('num_qb', h5('QB', style = "font-size:15px;"), 1, min=0), width=3)
                          ,column(numericInput('num_rb', h5('RB', style = "font-size:15px;"), 2, min=0), width=3)
                          ,column(numericInput('num_wr', h5('WR', style = "font-size:15px;"), 2, min=0), width=3)
                          ,column(numericInput('num_te', h5('TE', style = "font-size:15px;"), 1, min=0), width=3)
                        )
                        ,fluidRow(
                          column(numericInput('num_flex', h5('Flex', style = "font-size:15px;"), 1, min=0), width=3)
                          ,column(numericInput('num_k', h5('K', style = "font-size:15px;"), 1, min=0), width=3)
                          ,column(numericInput('num_dst', h5('DST', style = "font-size:15px;"), 1, min=0), width=3)
                        )
                        ,title='Number of Starters', collapsible = T, solidHeader = T, width=4, collapsed=T)
            )
            ,fluidRow(
              box(selectInput('adp_source', h5('ADP Source', style = "font-size:15px;"), choices=c("Average", "ESPN", "CBS"), selected="Average")
                  ,hr()
                  ,h4("VOR Baseline")
                  ,fluidRow(
                    column(numericInput('qb_baseline', h5('QB', style = "font-size:15px;"), 13, min=0), width=4)
                    ,column(numericInput('rb_baseline', h5('RB', style = "font-size:15px;"), 35, min=0), width=4)
                    ,column(numericInput('wr_baseline', h5('WR', style = "font-size:15px;"), 36, min=0), width=4)
                  )
                  ,fluidRow(
                    column(numericInput('te_baseline', h5('TE', style = "font-size:15px;"), 13, min=0), width=4)
                    ,column(numericInput('k_baseline', h5('K', style = "font-size:15px;"), 8, min=0), width=4)
                    ,column(numericInput('dst_baseline', h5('DST', style = "font-size:15px;"), 3, min=0), width=4)
                  )
                  ,title='Draft Settings', collapsible = T, solidHeader = T, width=4, collapsed=T)
              
              ,box(fluidRow(
                column(numericInput('qb_cohen', h5('QB', style = "font-size:15px;"), 13, min=0), width=4)
                ,column(numericInput('rb_cohen', h5('RB', style = "font-size:15px;"), 35, min=0), width=4)
                ,column(numericInput('wr_cohen', h5('WR', style = "font-size:15px;"), 36, min=0), width=4)
              )
              ,fluidRow(
                column(numericInput('te_cohen', h5('TE', style = "font-size:15px;"), 13, min=0), width=4)
                ,column(numericInput('k_cohen', h5('K', style = "font-size:15px;"), 8, min=0), width=4)
                ,column(numericInput('dst_cohen', h5('DST', style = "font-size:15px;"), 3, min=0), width=4)
              )
              ,title="Cohen's D Values", collapsible = T, solidHeader = T, width=4, collapsed=T)
              
              ,box(numericInput('salary_cap', h5('Salary Cap', style = "font-size:15px;"), 50000, min=0)
                   ,selectInput('dfs_source', h5('DFS Source', style = "font-size:15px;"), choices=c("Draft Kings"), selected="Draft Kings")
                   ,selectInput('dfs_optimizer', h5('Optimizer', style = "font-size:15px;"), choices=c("Points", "Ceiling", "Floor"), selected="Points")
                   ,numericInput('max_uncertainty', h5('Max Uncertainty', style = "font-size:15px;"), 95, min=0, max=100)
                   ,title="Daily Fantasy", collapsible = T, solidHeader = T, width=4, collapsed=T)
            )
            ,fluidRow(
              box(tabBox(
                side = "left", height = "250px",
                selected = "Passing",
                tabPanel("Passing"
                         ,fluidRow(column(3, h5("Pass Attempts", style = "font-size:15px;")), column(3, numericInput("ss_pass_attemts", NULL, 0)))
                         ,fluidRow(column(3, h5("Pass Completions", style = "font-size:15px;")), column(3, numericInput("ss_pass_completions", NULL, 0)))
                         ,fluidRow(column(3, h5("Pass Incompletions", style = "font-size:15px;")), column(3, numericInput("ss_pass_incompletions", NULL, 0)))
                         ,fluidRow(column(3, h5("Pass Yards", style = "font-size:15px;")), column(3, numericInput("ss_pass_yards", NULL, 0.04)))
                         ,fluidRow(column(3, h5("Pass TDs", style = "font-size:15px;")), column(3, numericInput("ss_pass_tds", NULL, 4)))
                         ,fluidRow(column(3, h5("Interceptions", style = "font-size:15px;")), column(3, numericInput("ss_pass_interceptions", NULL, -3)))
                         ,fluidRow(column(3, h5("40 Yard Pass Bonus", style = "font-size:15px;")), column(3, numericInput("ss_pass_40_yd_bonus", NULL, 0)))
                         ,fluidRow(column(3, h5("300 Yard Passing Bonus", style = "font-size:15px;")), column(3, numericInput("ss_pass_300_yd_bonus", NULL, 0)))
                         ,fluidRow(column(3, h5("350 Yard Passing Bonus", style = "font-size:15px;")), column(3, numericInput("ss_pass_350_yd_bonus", NULL, 0)))
                         ,fluidRow(column(3, h5("400 Yard Passing Bonus", style = "font-size:15px;")), column(3, numericInput("ss_pass_400_yd_bonus", NULL, 0)))
                )
                ,tabPanel("Rushing"
                          ,fluidRow(column(3, h5("Rush Yards", style = "font-size:15px;")), column(3, numericInput("ss_rush_yards", NULL, 0.1)))
                          ,fluidRow(column(3, h5("Rush Attempts", style = "font-size:15px;")), column(3, numericInput("ss_rush_attempts", NULL, 0)))
                          ,fluidRow(column(3, h5("Rush TDs", style = "font-size:15px;")), column(3, numericInput("ss_rush_td", NULL, 6)))
                          ,fluidRow(column(3, h5("40 Yard Rush Bonus", style = "font-size:15px;")), column(3, numericInput("ss_rush_40_yd_bonus", NULL, 0)))
                          ,fluidRow(column(3, h5("100 Yard Rushing Bonus", style = "font-size:15px;")), column(3, numericInput("ss_rush_40_yd_bonus", NULL, 0)))
                          ,fluidRow(column(3, h5("150 Yard Rushing Bonus", style = "font-size:15px;")), column(3, numericInput("ss_rush_40_yd_bonus", NULL, 0)))
                          ,fluidRow(column(3, h5("200 Yard Rushing Bonus", style = "font-size:15px;")), column(3, numericInput("ss_rush_40_yd_bonus", NULL, 0)))
                )
                ,tabPanel("Receiving"
                          ,fluidRow(column(3, h5("Receptions", style = "font-size:15px;")), column(3, numericInput("ss_receiving_receptions", NULL, 1)))
                          ,fluidRow(column(3, h5("Receiving Yards", style = "font-size:15px;")), column(3, numericInput("ss_receiving_yards", NULL, 0.1)))
                          ,fluidRow(column(3, h5("Receiving TDs", style = "font-size:15px;")), column(3, numericInput("ss_receiving_td", NULL, 6)))
                          ,fluidRow(column(3, h5("40 Yard Reception Bonus", style = "font-size:15px;")), column(3, numericInput("ss_receiving_40_yd_bonus", NULL, 0)))
                          ,fluidRow(column(3, h5("100 Yard Receiving Bonus", style = "font-size:15px;")), column(3, numericInput("ss_receiving_100_yd_bonus", NULL, 0)))
                          ,fluidRow(column(3, h5("150 Yard Receiving Bonus", style = "font-size:15px;")), column(3, numericInput("ss_receiving_150_yd_bonus", NULL, 0)))
                          ,fluidRow(column(3, h5("200 Yard Receiving Bonus", style = "font-size:15px;")), column(3, numericInput("ss_receiving_200_yd_bonus", NULL, 0)))
                )
                ,tabPanel("Returning"
                          ,fluidRow(column(3, h5("Return TDs", style = "font-size:15px;")), column(3, numericInput("ss_return_td", NULL, 6)))
                          ,fluidRow(column(3, h5("Return Yards", style = "font-size:15px;")), column(3, numericInput("ss_return_yards", NULL, 0)))
                )
                ,tabPanel("Kicking"
                          ,fluidRow(column(3, h5("Extra Points", style = "font-size:15px;")), column(3, numericInput("ss_kicking_xp", NULL, 1)))
                          ,fluidRow(column(3, h5("FG 0-19 Yards", style = "font-size:15px;")), column(3, numericInput("ss_kicking_0_19", NULL, 3)))
                          ,fluidRow(column(3, h5("FG 20-29 Yards", style = "font-size:15px;")), column(3, numericInput("ss_kicking_20_29", NULL, 3)))
                          ,fluidRow(column(3, h5("FG 30-39 Yards", style = "font-size:15px;")), column(3, numericInput("ss_kicking_30_39", NULL, 3)))
                          ,fluidRow(column(3, h5("FG 40-49 Yards", style = "font-size:15px;")), column(3, numericInput("ss_kicking_40_49", NULL, 4)))
                          ,fluidRow(column(3, h5("FG 50+ Yards", style = "font-size:15px;")), column(3, numericInput("ss_kicking_50_plus", NULL, 5)))
                          ,fluidRow(column(3, h5("Missed FGs", style = "font-size:15px;")), column(3, numericInput("ss_kicking_missed", NULL, 0)))
                )
                ,tabPanel("Defense"
                          ,fluidRow(column(3, h5("Recovered Fumbles", style = "font-size:15px;")), column(3, numericInput("ss_defense_fumble", NULL, 2)))
                          ,fluidRow(column(3, h5("Interceptions", style = "font-size:15px;")), column(3, numericInput("ss_defense_interception", NULL, 2)))
                          ,fluidRow(column(3, h5("Safeties", style = "font-size:15px;")), column(3, numericInput("ss_defense_safety", NULL, 2)))
                          ,fluidRow(column(3, h5("Sacks", style = "font-size:15px;")), column(3, numericInput("ss_defense_sack", NULL, 1)))
                          ,fluidRow(column(3, h5("TDs", style = "font-size:15px;")), column(3, numericInput("ss_defense_td", NULL, 6)))
                          ,fluidRow(column(3, h5("Blocked Kicks", style = "font-size:15px;")), column(3, numericInput("ss_defense_blocked_kick", NULL, 0)))
                          ,fluidRow(column(3, h5("Return Yards", style = "font-size:15px;")), column(3, numericInput("ss_defense_return_yds", NULL, 0)))
                )
                ,tabPanel("DST Points Allowed"
                          ,fluidRow(column(3, h5("0 points Allowed", style = "font-size:15px;")), column(3, numericInput("ss_pa_0", NULL, 5)))
                          ,fluidRow(column(3, h5("1-6 Points Allowed", style = "font-size:15px;")), column(3, numericInput("ss_pa_1_6", NULL, 4)))
                          ,fluidRow(column(3, h5("7-13 Points Allowed", style = "font-size:15px;")), column(3, numericInput("ss_pa_7_13", NULL, 3)))
                          ,fluidRow(column(3, h5("14-17 Points Allowed", style = "font-size:15px;")), column(3, numericInput("ss_pa_14_17", NULL, 1)))
                          ,fluidRow(column(3, h5("18-27 Points Allowed", style = "font-size:15px;")), column(3, numericInput("ss_pa_18_27", NULL, 0)))
                          ,fluidRow(column(3, h5("28-34 Points Allowed", style = "font-size:15px;")), column(3, numericInput("ss_pa_28_34", NULL, -1)))
                          ,fluidRow(column(3, h5("35-44 Points Allowed", style = "font-size:15px;")), column(3, numericInput("ss_pa_35_44", NULL, -3)))
                          ,fluidRow(column(3, h5("45-99 Points Allowed", style = "font-size:15px;")), column(3, numericInput("ss_pa_45_99", NULL, -5)))
                )
                ,tabPanel("Misc"
                          ,fluidRow(column(3, h5("Fumbles Lost", style = "font-size:15px;")), column(3, numericInput("ss_misc_fumble_lost", NULL, 1)))
                          ,fluidRow(column(3, h5("Total Fumbles", style = "font-size:15px;")), column(3, numericInput("ss_misc_total_fumbles", NULL, 3)))
                          ,fluidRow(column(3, h5("Sacks", style = "font-size:15px;")), column(3, numericInput("ss_misc_sacks", NULL, 3)))
                          ,fluidRow(column(3, h5("2 Point Conversions", style = "font-size:15px;")), column(3, numericInput("ss_misc_2_point_conversions", NULL, 3)))
                )
                ,width=12
              ),title="Scoring Settings", collapsible = T, solidHeader = T, width=8, collapsed = T)
            )
    ),
    
    tabItem(tabName = "projections",
            h2("Projections tab content")
            
    ),
    
    tabItem(tabName = "lineup_optimizer",
            h2("Lineup tab content")
    ),
    
    tabItem(tabName = "draft_optimizer",
            h2("Draft tab content")
    ),
    
    tabItem(tabName = "my_acount",
            h2("Account tab content")
    )
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "Fantasy Analytics"),
  sidebar,
  body,
  tags$head(
    tags$style(
      HTML(
        "
          /* input forms */
                .form-control {
                    border-radius: 4px 4px 4px 4px;
                }
          ")))
)


# Define the server
server <- function(input, output, session) {
  output$zero <- renderPrint({print('0')})
}


# Run the application
shinyApp(ui = ui, server = server)