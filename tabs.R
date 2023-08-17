draft_board_tab <- tabPanel("Draft Board",
                            sidebarLayout(
                              sidebarPanel(
                                verbatimTextOutput('next_pick_number'),
                                # Add inputs for filtering or any other controls you need
                                selectInput("position_filter", "Filter by Position:",
                                            choices = c("All", 'QB', 'RB', 'WR', 'TE', 'K', 'DST'),
                                            selected = "All"),
                                numericInput('draft_position', 'Draft Position', 1, min=1,max=12),
                                numericInput('total_teams', 'Total Teams', 12, min = 4, max=12),
                                br(),
                                actionButton("draft_button", "Draft Player", width='100%'),
                                br(),
                                br(),
                                actionButton('remove_player', "Remove Player", width='100%'),
                                br(),
                                br(),
                                actionButton('add_player_in', "Add Player Back In", width='100%'),
                                br(),
                                br(),
                                actionButton("clear_drafts", "Reset Draft", width='100%'),
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
)

roster_tab <- tabPanel("Roster",
                 fluidRow(
                   column(uiOutput("roster"), width=6),
                   column(dataTableOutput("removed_players"), width=6)
                 )
)