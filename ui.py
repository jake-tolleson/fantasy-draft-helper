from shiny import *
from shinywidgets import *

def create_ui():
    ui = ui.page_fluid(
        ui.h2("Fantasy Draft Board",
            ui.navset_pill(
                ui.nav("Draft Board",
                    ui.layout_sidebar(
                        ui.panel_sidebar(
                            ui.output_text_verbatim('next_pick_number'),
                            # Add inputs for filtering or any other controls you need
                            ui.input_select(id="position_filter", 
                                            label="Filter by Position:",
                                            choices=["All", 'QB', 'RB', 'WR', 'TE', 'K', 'DST'],
                                            selected="All",
                                            multiple=True),
                            ui.input_numeric('draft_position', 'Draft Position', value=1, min=1, max=12),
                            ui.input_numeric('total_teams', 'Total Teams', value=12, min=4, max=12),
                            ui.br(),
                            ui.input_action_button("draft_button", "Draft Player", width='100%'),
                            ui.br(),
                            ui.br(),
                            ui.input_action_button('remove_player', "Remove Player", width='100%'),
                            ui.br(),
                            ui.br(),
                            ui.input_action_button('add_player_in', "Add Player Back In", width='100%'),
                            ui.br(),
                            ui.br(),
                            ui.input_action_button("clear_drafts", "Reset Draft", width='100%')
                        ),
                        ui.panel_main(
                            ui.row(
                                # Draft Board (Top Left)
                                ui.column(12, ui.output_data_frame("draft_board")),
                                # ADP vs Points (Top Right)
                                ui.br()
                            ),
                            ui.row(
                                # Additional Plot 1 (Bottom Left)
                                ui.column(6, output_widget("ceiling_floor_plot")),
                                # Additional Plot 2 (Bottom Right)
                                ui.column(6, output_widget("adp_points_plot"))
                            )
                        )
                    )
                )
                # Roster tab content goes here
                
            )
        )
    )
    return ui