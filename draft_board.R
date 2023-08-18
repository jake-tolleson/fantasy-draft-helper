#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Load the required libraries
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(bslib)
library(reticulate)
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

eld <- import("espn_live_draft")


# dataset
df <- readRDS('2023draft.rds')
df <- df %>% 
  mutate(ppg = points/17) %>%
  mutate(ppg_ceiling = ceiling/17) %>%
  mutate(ppg_floor = floor/17) %>%
  mutate(name=paste(first_name, last_name)) %>%
  mutate(cov = sd_pts / points) %>%
  mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
  mutate(name=trimws(gsub('[.]', '',name))) %>%
  mutate(name=trimws(gsub('Jr.', '',name))) %>%
  mutate(name=trimws(gsub('III', '',name)))

df$position <- factor(df$position, levels=c('QB', 'RB', 'WR', 'TE', 'K', 'DST'))
# Extract column names from the dataset
drafted_player_columns <- colnames(df)

# Create an empty data frame with the same columns as the dataset
empty_drafted_players <- data.frame(matrix(ncol = length(drafted_player_columns), nrow = 0))
colnames(empty_drafted_players) <- drafted_player_columns

# Define the UI
ui <- fluidPage(theme = bs_theme(bootswatch = "zephyr"),
                navbarPage("Fantasy Draft Board",
                           tabPanel("Draft Board",
                                    sidebarLayout(
                                      sidebarPanel(
                                        verbatimTextOutput('next_pick_number'),
                                        # Add inputs for filtering or any other controls you need
                                        selectInput("position_filter", "Filter by Position:",
                                                    choices = c("All", 'QB', 'RB', 'WR', 'TE', 'Flex', 'K', 'DST'),
                                                    selected = "All"),
                                        br(),
                                        h5('Live Draft Controls'),
                                        actionButton("start_live_draft", "Start ESPN Draft", width='100%'),
                                        actionButton("configure_draft", "Configure Draft", width='100%'),
                                        actionButton("update_draft", "Update Draft", width='100%')
                                        ,width=2),
                                      mainPanel(
                                        fluidRow(
                                          column(width = 12, div(DT::dataTableOutput("draft_board", height='500px'), style = "font-size:80%")),  # Draft Board (Top Left)
                                          br(), # ADP vs Points (Top Right)
                                        ),       
                                        fluidRow(
                                          column(width = 6, plotlyOutput("ceiling_floor_plot", height='500px')),                 # Additional Plot 1 (Bottom Left)
                                          column(width = 6, plotlyOutput("adp_points_plot"))                  # Additional Plot 2 (Bottom Right)
                                        ), width = 10))
                           ),
                           tabPanel("Roster",
                                    fluidRow(
                                      column(uiOutput("roster"), width=6),
                                      column(dataTableOutput("removed_players"), width=6)
                                    )
                           )
                )
)

# Define the server
server <- function(input, output, session) {
  # Create reactive values to store drafted players and roster
  #drafted_players <- reactiveVal(data.frame(player_id = character(0)))
  roster <- reactiveVal(tibble())
  draft_board_data <- reactiveVal(df %>% arrange(rank))
  draft_position <- reactiveVal(1)
  total_teams <- reactiveVal(12)
  draft <- reactiveVal(py_none())
  drafted_players <- reactiveVal(tibble())
  my_picks <- reactiveVal(c())
  
  
  # Reactive value to store filtered data based on position filter
  filtered_data <- reactive({
    
    data <- draft_board_data() %>%
      mutate(prob_of_avail_next = round(1 - pnorm(next_pick_number(), mean = adp, sd = adp_sd),2)) %>%
      mutate(prob_of_avail_two_away = round(1 - pnorm(two_picks_away(), mean = adp, sd = adp_sd),2))
    
    if (!input$position_filter %in%  c("All", "Flex")) {
      data <- data[data$position == input$position_filter, ]
    }
    
    if (input$position_filter == "Flex"){
      data <- data[data$position %in% c("RB", "WR", "TE"), ]
      
    }
    
    data
  })
  
  next_pick_number <- reactive({
    picks_made <- nrow(roster())
    next_pick <- get_next_pick_number(picks_made, draft_position(), total_teams())
    return(next_pick)
  })
  
  two_picks_away <- reactive({
    picks_made <- nrow(roster()) + 1
    next_pick <- get_next_pick_number(picks_made, draft_position(), total_teams())
    return(next_pick)
  })
  
  three_picks_away <- reactive({
    picks_made <- nrow(roster()) + 2
    next_pick <- get_next_pick_number(picks_made, draft_position(), total_teams())
    return(next_pick)
  })
  
  output$next_pick_number <- renderText({
    paste('Next Picks:\n', next_pick_number(), 
          '\n', two_picks_away(),
          '\n', three_picks_away())
  })
  
  
  output$draft_board <- DT::renderDataTable({
    filtered_data() %>% 
      mutate(adp=round(adp)) %>%
      select(name,team,position,rank,ppg,pos_rank,tier,adp,uncertainty, prob_of_avail_next, prob_of_avail_two_away) %>%
      datatable(options = list(scrollX = TRUE, scrollY = '400px')) %>%
      formatPercentage(columns=c('uncertainty', 'prob_of_avail_next', 'prob_of_avail_two_away'))
  })
  
  # Plot Ceiling vs Floor
  output$ceiling_floor_plot <- renderPlotly({
    
    p <- filtered_data() %>% arrange(rank) %>%
      slice_head(n=12) %>%
      ggplot(aes(x = ppg_ceiling, y = reorder(name, rank, decreasing=TRUE), col=position,
                 text = paste(
                   "Name: ", name,
                   "<br>Ceiling PPG: ", round(ceiling/17,2),
                   "<br>Avg PPG: ", round(ppg,2),
                   "<br>Floor PPG: ", round(floor/17,2)
                 ))) +
      geom_point(aes(x=ppg), shape=23, size=3, fill=24) +
      geom_segment(aes(xend = ppg_floor, yend = name), linewidth = 1) +
      scale_fill_discrete(breaks = c('QB', 'RB', 'WR', 'TE', 'K', 'DST')) +
      scale_colour_manual(values=c("QB"="#F8766D", "RB"="#7CAE00", "WR"="#00B0F6", "TE"="#FF61CC",  "K"='#00BFC4', "DST"="#CD9600"), 
                          labels=c("QB", "RB", "WR", "TE", "K", "DST")) +
      labs(title = 'Ceiling vs Floor', x = 'PPG', y = 'Name') +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none")
    
    ggplotly(p, tooltip = c('text'))
  })
  
  # Plot Tier plot
  output$tier_plot <- renderPlotly({
    p <- filtered_data() %>% filter(tier<10) %>%
      ggplot(aes(x=tier, fill=position)) + 
      geom_bar() +
      scale_fill_discrete(breaks = c('QB', 'RB', 'WR', 'TE', 'K', 'DST')) +
      facet_wrap(facets=~factor(position, levels=c('QB', 'RB', 'WR', 'TE', 'K', 'DST')),ncol=1, scales='free_y') +
      scale_fill_manual(values=c("QB"="#F8766D", "RB"="#7CAE00", "WR"="#00B0F6", "TE"="#FF61CC",  "K"='#00BFC4', "DST"="#CD9600"), 
                        labels=c("QB", "RB", "WR", "TE", "K", "DST")) +
      labs(title='Player Availibility', x='Tier', y='Availibility') +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none",
            axis.ticks.y= element_blank(),
            axis.text.y= element_blank()
      ) 
    
    ggplotly(p)
  })
  
  # Plot ADP vs Points
  output$adp_points_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = adp, y = points , col = position,
                                     text = paste(
                                       "Name: ", name,
                                       "<br>Points: ", round(points),
                                       "<br>ADP: ", round(adp)
                                     ))) +
      geom_point(size = 2) +
      scale_fill_discrete(breaks = c('QB', 'RB', 'WR', 'TE', 'K', 'DST')) +
      scale_colour_manual(values=c("QB"="#F8766D", "RB"="#7CAE00", "WR"="#00B0F6", "TE"="#FF61CC",  "K"='#00BFC4', "DST"="#CD9600"), 
                          labels=c("QB", "RB", "WR", "TE", "K", "DST")) +
      labs(title = "PPG vs ADP", x = "Average Draft Position (ADP)", y = "Projected Points") +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none")
    
    ggplotly(p, tooltip = c('text'))
  })
  
  # Start ESPN Live Draft
  observeEvent(input$start_live_draft, {
    if(py_has_attr(draft(), 'driver')){
      return(NULL)
    }
    draft(eld$ds$draft_monitor('Team TOLLESON', 'edge'))
  })
  
  # Configure ESPN Live Draft
  observeEvent(input$configure_draft, {
    if(!py_has_attr(draft(), 'driver')){
      return(NULL)
    }
    draft()$configure_draft()
    
    draft_position(draft()$mypick)
    total_teams(draft()$teams)
    my_picks(which(draft()$pick_order == draft()$myteam))
  })
  
  # Update ESPN Live Draft
  observeEvent(input$update_draft, {
    if(!py_has_attr(draft(), 'driver') || length(draft()$rosters) == 0){
      return(NULL)
    }
    draft()$update()
    
    tryCatch({
      drafted_players(
      draft()$pick_history %>%
        as_tibble() %>%
        mutate(Player=trimws(gsub('D/ST', '',Player))) %>%
        mutate(Player=trimws(gsub('[.]', '',Player))) %>%
        mutate(Player=trimws(gsub('Jr.', '',Player))) %>%
        mutate(Player=trimws(gsub('III', '',Player)))
    )
    }, error= function(e) print(paste('Error in update drafted_players', e$message)))
    
    tryCatch({
    draft_board_data(
      df %>%
        anti_join(drafted_players(),by=join_by(name == Player)) %>%
        arrange(rank)
    ) }, error= function(e) print(paste('Error in update drafted_board_data', e$message)))

    tryCatch({
    roster(draft()$rosters[[draft()$myteam]])
    }, error= function(e) print(paste('Error in update roster', e$message)))
  })
  
  # Generate the roster HTML based on drafted players
  output$roster <- renderUI({
    if (nrow(roster()) > 0) {
      my_team <- roster() %>%
        mutate(name=player) %>% 
        select(name, position)
      
      roster_html <- paste0(
        "<h2>My Roster</h2>",
        "<h4>QB</h4>",
        "<ul>", paste("<li>", my_team[my_team$position == "QB", 'name'], "</li>", collapse = ""), "</ul>",
        "<h4>RB</h4>",
        "<ul>", paste("<li>", my_team[my_team$position == "RB", 'name'], "</li>", collapse = ""), "</ul>",
        "<h4>WR</h4>",
        "<ul>", paste("<li>", my_team[my_team$position == "WR", 'name'], "</li>", collapse = ""), "</ul>",
        "<h4>TE</h4>",
        "<ul>", paste("<li>", my_team[my_team$position == "TE", 'name'], "</li>", collapse = ""), "</ul>",
        "<h4>K</h4>",
        "<ul>", paste("<li>", my_team[my_team$position == "K", 'name'], "</li>", collapse = ""), "</ul>",
        "<h4>DST</h4>",
        "<ul>", paste("<li>", my_team[my_team$position == "DST", 'name'], "</li>", collapse = ""), "</ul>"
        
      )
      HTML(roster_html)
    } else {
      HTML("<p>Roster: Empty</p>")
    }
  })
  
  output$removed_players <- renderDataTable({
    removed_players_data() %>% 
      select(name,team,position,rank) %>%
      datatable(options = list(scrollX = TRUE, scrollY = '400px'))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

