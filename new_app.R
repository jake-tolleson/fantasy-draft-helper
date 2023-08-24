# Load the required libraries
library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(bslib)
library(reticulate)
library(shinydashboard)
library(fresh)
library(nflreadr)
library(lubridate)
options(shiny.autoreload = TRUE)


# Helper function
get_next_pick_number <- function(total_drafted, starting_pick, total_teams){
  current_round <- total_drafted + 1
  if(current_round %% 2 == 0){
    next_pick <- current_round * total_teams - starting_pick + 1
  } else {
    next_pick <- (current_round-1)*total_teams+starting_pick
  }

  return(next_pick)
}

current_season <- get_current_season()
current_week <- get_current_week()

if(month(today()) > 4 && current_season < year(today())){
  current_season <- current_season + 1
  current_week <- 0
}

if(current_week == 0){
  num_week_inputs <- 0
}else{
  num_week_inputs <- 1:current_week |> as.character()
}

week_inputs <- num_week_inputs
week_inputs[1] <- 'Season'

eld <- import("espn_live_draft")

df <- readRDS('/Users/johnathan/Desktop/Misc/average_projections.rds')
df <- df |>
  mutate(ppg = points/17) |>
  mutate(ppg_ceiling = ceiling/17) |>
  mutate(ppg_floor = floor/17) |>
  mutate(ppg_vor = points_vor/17) |>
  mutate(ppg_vor_ceiling = ceiling_vor/17) |>
  mutate(ppg_vor_floor = floor_vor/17) |>
  mutate(name=case_when(position=='DST'~last_name
                      ,TRUE~paste(first_name, last_name))) |>
  mutate(cov = sd_pts / points) |>
  mutate(across(where(is.numeric), \(x) round(x, 2))) |>
  mutate(name=clean_player_names(name)) |>
  mutate()

df$position <- factor(df$position, levels=c('QB', 'RB', 'WR', 'TE', 'K', 'DST'))

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

         .content-wrapper { overflow: auto; }

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

    tabItem(tabName = "live_draft"
           ,box(fluidRow(column(4, selectizeInput('draft_browser_type', 'Browser Type', c('Edge', 'Firefox', 'Chrome'), 'Edge')))
               ,fluidRow(column(4, actionButton('launch_browser', "Launch Browser", icon('play'))))
                ,title="Browser Settings", collapsible = T, solidHeader = T, width=4, collapsed=T)
           ,box(fluidRow(column(12,selectInput('my_fantasy_team', 'Fantasy Team Name', c('Placeholder'))))
               ,fluidRow(column(12, actionButton('configure_draft_button', "Configure Draft", icon('user-gear'))))
                ,title="Live Draft Configuration", collapsible = T, solidHeader = T, width=4, collapsed=T)
           ,box(fluidRow(column(12, selectInput("ld_position_filter", 'Position Filter', c('All', 'QB', 'RB', 'WR', 'TE', 'Flex', 'K', 'DST'), 'All')))
               ,fluidRow(column(12, actionButton('update_draft_button', "Update Draft", icon('refresh'))))
                ,title="Live Draft Controls", collapsible = T, solidHeader = T, width=4, collapsed=T
           )
           ,box(fluidRow(column(12, div(DT::dataTableOutput("draft_board", height='500px'), style = "font-size:100%")))
                ,title="Draft Board", collapsible = F, solidHeader = T, width=12, collapsed=F)
           ,box(fluidRow(column(4, selectInput('ld_ceiling_floor_x_axis', 'Value', c('Points', 'PPG', 'Points VOR', 'PPG VOR'), 'PPG')))
               ,fluidRow(column(12, plotlyOutput("ld_ceiling_floor_plot", height='500px')))
                ,title="Ceiling vs Floor", collapsible = F, solidHeader = T, width=6, collapsed=F)
           ,box(fluidRow(column(4, selectInput('ld_points_v_adp_y_axis', 'Value', c('Points', 'PPG', 'Points VOR', 'PPG VOR'), 'Points')))
               ,fluidRow(column(12, plotlyOutput("ld_points_v_adp", height='500px')))
                ,title="Points vs ADP", collapsible = F, solidHeader = T, width=6, collapsed=F)
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


  ##################
  ### Live Draft ###
  ##################

  # set variables
  draft <- reactiveVal(py_none())
  roster <- reactiveVal(tibble())
  draft_board_data <- reactiveVal(df |> arrange(rank))
  draft_position <- reactiveVal(1)
  total_teams <- reactiveVal(12)
  drafted_players <- reactiveVal(tibble())
  my_picks <- reactiveVal(c())


  # Reactive value to store filtered data based on position filter
  filtered_draft_board <- reactive({

    data <- draft_board_data() |>
      mutate(prob_of_avail_next = round(1 - pnorm(next_pick_number(), mean = adp, sd = adp_sd),2)) |>
      mutate(prob_of_avail_two_away = round(1 - pnorm(two_picks_away(), mean = adp, sd = adp_sd),2))

    if (!input$ld_position_filter %in%  c("All", "Flex")) {
      data <- data[data$position == input$ld_position_filter, ]
    }

    if (input$ld_position_filter == "Flex"){
      data <- data[data$position %in% c("RB", "WR", "TE"), ]

    }

    data
  })

  filtered_live_data <- reactive({
    data <- df |>
              filter(rank <= 210)

    if (!input$ld_position_filter %in%  c("All", "Flex")) {
      data <- data[data$position == input$ld_position_filter, ]
    }

    if (input$ld_position_filter == "Flex"){
      data <- data[data$position %in% c("RB", "WR", "TE"), ]

    }

    data
  })

  next_pick_number <- reactive({
    picks_made <- roster() |> drop_na() |> nrow()
    next_pick <- get_next_pick_number(picks_made, draft_position(), total_teams())
    return(next_pick)
  })

  two_picks_away <- reactive({
    picks_made <- roster() |> drop_na() |> nrow() + 1
    next_pick <- get_next_pick_number(picks_made, draft_position(), total_teams())
    return(next_pick)
  })
  df |> drop_na()
  three_picks_away <- reactive({
    picks_made <- roster() |> drop_na() |> nrow() + 2
    next_pick <- get_next_pick_number(picks_made, draft_position(), total_teams())
    return(next_pick)
  })

  # Start ESPN Live Draft
  observeEvent(input$launch_browser, {
    if(py_has_attr(draft(), 'driver')){
      return(NULL)
    }
    draft(eld$ds$draft_monitor(input$my_fantasy_team, input$draft_browser_type))
  })

  # Configure ESPN Live Draft
  observeEvent(input$configure_draft_button, {
    if(!py_has_attr(draft(), 'driver')){
      return(NULL)
    }
    draft()$configure_draft()

    draft_position(draft()$mypick)
    total_teams(draft()$teams)
    my_picks(which(draft()$pick_order == draft()$myteam))

    if(input$my_fantasy_team == 'Placeholder'){
    updateSelectInput(
      session = session,
      'my_fantasy_team',
      label = 'Fantasy Team Name',
      choices = draft()$team_map |> unlist() |> unname()
    )
    }

  })

  observe({
    tryCatch({
    draft()$team_name <- input$my_fantasy_team
    draft()$configure_draft()
    }, error= function(e) print(paste('Live Draft team name cannot be set', e$message)))
  })


  # Update ESPN Live Draft
  observeEvent(input$update_draft_button, {
    if(!py_has_attr(draft(), 'driver') || length(draft()$rosters) == 0){
      return(NULL)
    }

    tryCatch({
    draft()$update()
    }, error= function(e) print(paste('Error in draft update', e$message)))

    tryCatch({
      drafted_players(
        draft()$pick_history |>
          as_tibble() |>
          mutate(Player=trimws(gsub('D/ST', '', Player))) |>
          mutate(Player=clean_player_names(Player))
      )
    }, error= function(e) print(paste('Error in update drafted_players', e$message)))

    tryCatch({
      draft_board_data(
        df |>
          anti_join(drafted_players(), by=join_by(name == Player)) |>
          arrange(rank)
      ) }, error= function(e) print(paste('Error in update drafted_board_data', e$message)))

    tryCatch({
      roster(draft()$rosters[[draft()$myteam]])
    }, error= function(e) print(paste('Error in update roster', e$message)))
  })

  output$draft_board <- DT::renderDataTable({
    filtered_draft_board() |>
      mutate(adp=round(adp)) |>
      select(name, team, position, rank, ppg, ppg_vor, pos_rank, tier, adp, uncertainty, prob_of_avail_next, prob_of_avail_two_away) |>
      datatable(options = list(scrollX = TRUE, scrollY = '400px')) |>
      formatPercentage(columns=c('uncertainty', 'prob_of_avail_next', 'prob_of_avail_two_away'))
  })


  # Plot Ceiling vs Floor
  output$ld_ceiling_floor_plot <- renderPlotly({

    p <- filtered_draft_board() |> arrange(rank) |>
      slice_head(n=12) |>
      mutate(x=case_when(input$ld_ceiling_floor_x_axis=='Points'~points
                         ,input$ld_ceiling_floor_x_axis=='PPG'~ppg
                         ,input$ld_ceiling_floor_x_axis=='Points VOR'~points_vor
                         ,input$ld_ceiling_floor_x_axis=='PPG VOR'~ppg_vor
                         ,TRUE~points)) |>
      mutate(x_floor=case_when(input$ld_ceiling_floor_x_axis=='Points'~floor
                         ,input$ld_ceiling_floor_x_axis=='PPG'~ppg_floor
                         ,input$ld_ceiling_floor_x_axis=='Points VOR'~floor_vor
                         ,input$ld_ceiling_floor_x_axis=='PPG VOR'~ppg_vor_floor
                         ,TRUE~points)) |>
      mutate(x_ceiling=case_when(input$ld_ceiling_floor_x_axis=='Points'~ceiling
                         ,input$ld_ceiling_floor_x_axis=='PPG'~ppg_ceiling
                         ,input$ld_ceiling_floor_x_axis=='Points VOR'~ceiling_vor
                         ,input$ld_ceiling_floor_x_axis=='PPG VOR'~ppg_vor_ceiling
                         ,TRUE~points)) |>
      ggplot(aes(x = x_ceiling, y = reorder(name, rank, decreasing=TRUE), col=position,
                 text = paste(
                   "Name: ", name,
                   paste0("<br>Ceiling ", input$ld_ceiling_floor_x_axis, ": "), round(x_ceiling,1),
                   paste0("<br>Avg ", input$ld_ceiling_floor_x_axis, ": "), round(x,1),
                   paste0("<br>Floor ", input$ld_ceiling_floor_x_axis, ": "), round(x_floor,1)
                 ))) +
      geom_point(aes(x=x), shape=23, size=3, fill=24) +
      geom_segment(aes(xend = x_floor, yend = name), linewidth = 1) +
      scale_fill_discrete(breaks = c('QB', 'RB', 'WR', 'TE', 'K', 'DST')) +
      scale_colour_manual(values=c("QB"="#F8766D", "RB"="#7CAE00", "WR"="#00B0F6", "TE"="#FF61CC",  "K"='#00BFC4', "DST"="#CD9600"),
                          labels=c("QB", "RB", "WR", "TE", "K", "DST")) +
      labs(title = '', x = input$ld_ceiling_floor_x_axis, y = 'Name') +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none")

    ggplotly(p, tooltip = c('text'))
  })

  # Points vs ADP
  output$ld_points_v_adp <- renderPlotly({

  p <- filtered_live_data() |>
             filter(adp <= 200) |>
             mutate(y=case_when(input$ld_points_v_adp_y_axis=='Points'~points
                               ,input$ld_points_v_adp_y_axis=='PPG'~ppg
                               ,input$ld_points_v_adp_y_axis=='Points VOR'~points_vor
                               ,input$ld_points_v_adp_y_axis=='PPG VOR'~ppg_vor
                               ,TRUE~points)) |>
             ggplot(aes(x=sqrt(adp), y=y^0.5, label=name, label2=y, label3=adp)) +
             geom_point(data=filtered_live_data() |>
                          filter(name %in% draft_board_data()$name) |>
                          mutate(y=case_when(input$ld_points_v_adp_y_axis=='Points'~points
                                             ,input$ld_points_v_adp_y_axis=='PPG'~ppg
                                             ,input$ld_points_v_adp_y_axis=='Points VOR'~points_vor
                                             ,input$ld_points_v_adp_y_axis=='PPG VOR'~ppg_vor
                                             ,TRUE~points))) +
             geom_smooth(method='lm', formula= y~x)  +
             labs(title='', x='ADP', y=input$ld_points_v_adp_y_axis) +
             scale_y_continuous(breaks= trans_breaks(function(x)x^2, "sqrt"),
                                labels = trans_format(function(x)x^2, "sqrt")) +
             scale_x_continuous(breaks= sqrt(c(5,10,15,25,35,45,55,70,85,100,125,150,175,200)),
                                labels = as.character(c(5,10,15,25,35,45,55,70,85,100,125,150,175,200)))

  p <- ggplotly(p, tooltip = c("label", "label2", "label3"))
  p$x$data[[1]]$text <- gsub('/>y:', paste0('/>', input$ld_points_v_adp_y_axis, ':'), p$x$data[[1]]$text)
  p$x$data[[3]]$text <- paste(round(p$x$data[[3]]$x^2,0), ',', round(p$x$data[[3]]$y^2,0))
  p$x$data[[3]]$hoverinfo <- "text"

  p
  })

}


# Run the application
shinyApp(ui = ui, server = server)