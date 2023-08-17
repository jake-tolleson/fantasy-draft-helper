my_server <- function(input, output, session) {
  # Create reactive values to store drafted players and roster
  #drafted_players <- reactiveVal(data.frame(player_id = character(0)))
  removed_players_data <- reactiveVal(df %>% slice_head(n=0))
  roster <- reactiveVal(TRUE)
  draft_board_data <- reactiveVal(df %>% arrange(rank))
  draft_position <- reactive(input$draft_position)
  total_teams <- reactive(input$total_teams)
  draft <- reactiveVal(TRUE)
  drafted_players <- reactiveVal(TRUE)


  # Reactive value to store filtered data based on position filter
  filtered_data <- reactive({
    data <- draft_board_data() %>%
      mutate(prob_of_avail_next = round(1 - pnorm(next_pick_number(), mean = adp, sd = adp_sd),2)) %>%
      mutate(prob_of_avail_two_away = round(1 - pnorm(two_picks_away(), mean = adp, sd = adp_sd),2))
    if (input$position_filter != "All") {
      data <- data[data$position == input$position_filter, ]
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

  output$next_pick_number <- renderText({
    paste('Next Picks:\n', next_pick_number(), '\n', two_picks_away())
  })


  # Function to update drafted players and roster
  observeEvent(input$draft_button, {
    # Extract selected player's data
    selected_row <- input$draft_board_rows_selected
    if (!is.null(selected_row)) {
      selected_player <- draft_board_data()[selected_row, ]

      # Create a player_id for the selected player
      player_id <- paste(selected_player$first_name, selected_player$last_name)

      # Update drafted players and roster
      drafted_players(rbind(drafted_players(), data.frame(player_id)))
      roster(c(roster(), player_id))

      # Update removed_player_data by excluding the removed player
      removed_players_data(rbind(removed_players_data(), draft_board_data()[selected_row, ]))


      # Update draft_board_data by excluding the drafted player
      draft_board_data(draft_board_data()[-selected_row, ])

    }
  })


  # Function to clear drafted players
  observeEvent(input$remove_player, {
    # Extract selected player's data
    selected_row <- input$draft_board_rows_selected
    if (!is.null(selected_row)) {
      selected_player <- draft_board_data()[selected_row, ]

      # Update removed_player_data by adding the removed player
      removed_players_data(rbind(removed_players_data(), draft_board_data()[selected_row, ]))

      # Update draft_board_data by excluding the drafted player
      draft_board_data(draft_board_data()[-selected_row, ])
    }
  })

  # Function to add players back to draft board
  observeEvent(input$add_player_in, {
    # Extract selected player's data
    selected_row <- input$removed_players_rows_selected
    if (!is.null(selected_row)) {
      selected_player <- removed_players_data()[selected_row, ]

      # Create a player_id for the selected player
      player_id <- selected_player$name

      # Update draft_board_data by adding the removed player
      draft_board_data(rbind(draft_board_data(), removed_players_data()[selected_row, ]) %>%
                         arrange(rank))

      # Update removed_player_data by excluding the drafted player
      removed_players_data(removed_players_data()[-selected_row, ])

      drafted_players(drafted_players() %>% filter(player_id != player_id))
      roster(roster()[!roster() %in% player_id])
    }

  })


  # Function to clear drafted players
  observeEvent(input$clear_drafts, {
    drafted_players(data.frame(player_id = character(0)))
    roster(character(0))
    draft_board_data(df)
    removed_players_data(df %>% slice_head(n=0))
  })

  output$draft_board <- DT::renderDataTable({
    filtered_data() %>%
      mutate(adp=round(adp)) %>%
      select(name,team,position,rank,ppg,pos_rank,tier,adp, prob_of_avail_next, prob_of_avail_two_away) %>%
      datatable(options = list(scrollX = TRUE, scrollY = '400px')) %>%
      formatPercentage(columns=c('prob_of_avail_next', 'prob_of_avail_two_away'))
  })

  # Plot Ceiling vs Floor
  output$ceiling_floor_plot <- renderPlotly({

    p <- filtered_data() %>% arrange(rank) %>%
      slice_head(n=12) %>%
      ggplot(aes(x = ppg_ceiling, y = reorder(name, rank, decreasing=TRUE), col=position,
                 text = paste(
                   "Name: ", name,
                   "<br>Avg PPG: ", round(ppg,2),
                   "<br>SD PPG: ", round(sd_pts/17,2)
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
    if(py_has_attr(draft, 'driver')){
      return(NULL)
    }
    draft(eld$ds$draft_monitor('Team TOLLESON', 'edge'))
  })

  # Configure ESPN Live Draft
  observeEvent(input$configure_draft, {
    if(!py_has_attr(draft, 'driver')){
      return(NULL)
    }
    draft()$configure_draft()
  })

  # Update ESPN Live Draft
  observeEvent(input$update_draft, {
    if(!py_has_attr(draft, 'driver') | length(draft$rosters) == 0){
      return(NULL)
    }
    draft()$update()

    drafted_players(
      draft$pick_history %>%
        as_tibble() %>%
        mutate(Player=trimws(gsub('D/ST', '',Player))) %>%
        mutate(Player=trimws(gsub('DJ', 'D.J.',Player))) %>%
        mutate(Player=trimws(gsub('Etienne Jr.', 'Etienne',Player)))
    )

    draft_board(
      df %>%
        anti_join(drafted_players,by=join_by(name == Player)) %>%
        arrange(rank)
    )

    roster(draft()$rosters[[draft()$myteam]])
  })

  # Generate the roster HTML based on drafted players
  output$roster <- renderUI({
    if (length(roster()) > 0) {
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