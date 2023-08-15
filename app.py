import pandas as pd
import numpy as np
from shiny import *
import plotly.express as px
from bs4 import BeautifulSoup
from scipy.stats import norm
import plotly.express as px
import uvicorn
from shinywidgets import *

# Helper function
def get_next_pick_number(total_drafted, starting_pick, total_teams):
    current_round = total_drafted + 1
    if current_round % 2 == 0:
        next_pick = current_round * total_teams - starting_pick + 1
    else:
        next_pick = (current_round-1)*total_teams+starting_pick
    return next_pick

# dataset
df = pd.read_csv(r'/Users/jaketolleson/Library/CloudStorage/OneDrive-Personal/Documents/Draft Kings/2023 draft.csv')
df['ppg'] = df['points']/17
df['ppg_ceiling'] = df['ceiling']/17
df['ppg_floor'] = df['floor']/17
df['name'] = df['first_name'] + ' ' + df['last_name']
df['cov'] = df['sd_pts'] / df['points']
df = df.round(2)

df['position'] = pd.Categorical(df['position'], categories=['QB', 'RB', 'WR', 'TE', 'K', 'DST'], ordered=True)
# Extract column names from the dataset
drafted_player_columns = list(df.columns)

# Create an empty data frame with the same columns as the dataset
empty_drafted_players = pd.DataFrame(columns=drafted_player_columns)

app_ui = ui.page_fluid(
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
                            ui.column(12, ui.dataframe.output_data_frame("draft_board")),
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


def server(input, output, session):
    # Create reactive values to store drafted players and roster
    drafted_players = reactive.Value(pd.DataFrame(columns=['player_id']))
    removed_players_data = reactive.Value(df.copy().head(0))
    roster = reactive.Value([])
    draft_board_data = reactive.Value(df.copy().sort_values(by='rank'))

    # Reactive value to store filtered data based on position filter
    @reactive.Calc()
    def filtered_data():
        data = draft_board_data().assign(
            prob_of_avail_next=lambda x: np.round(1 - norm.cdf(next_pick(), loc=x['adp'], scale=x['adp_sd']), 2),
            prob_of_avail_two_away=lambda x: np.round(1 - norm.cdf(two_picks_away(), loc=x['adp'], scale=x['adp_sd']), 2)
        )
        if not set(('All',)).issubset(input.position_filter()):
            data = data[data['position'].isin(input.position_filter())]
        return data

    @reactive.Calc()
    def next_pick():
        picks_made = len(roster())
        next_pick = get_next_pick_number(picks_made, input.draft_position(), input.total_teams())
        return next_pick
    
    @reactive.Calc()
    def two_picks_away():
        picks_made = len(roster()) + 1
        next_pick = get_next_pick_number(picks_made, input.draft_position(), input.total_teams())
        return next_pick

    @output
    @render.text()
    def next_pick_number():
        return f"Next Picks:\n{next_pick()}\n{two_picks_away()}"

    # Function to update drafted players and roster
    @reactive.Effect
    @reactive.event(input.draft_button)
    def update_drafted_players():
        # Extract selected player's data
        selected_row = input.draft_board_selected_rows()
        if selected_row:
            selected_player = draft_board_data().iloc[selected_row[0]]

            # Create a player_id for the selected player
            player_id = f"{selected_player['first_name']} {selected_player['last_name']}"

            # Update drafted players and roster
            drafted_players.set(pd.concat([drafted_players(), pd.DataFrame({'player_id': [player_id]})]))
            roster.set(roster() + [player_id])

            # Update removed_player_data by excluding the removed player
            removed_players_data.set(pd.concat([removed_players_data(), draft_board_data().iloc[selected_row]]))

            # Update draft_board_data by excluding the drafted player
            draft_board_data.set(draft_board_data().drop(selected_row))

    # Function to clear drafted players
    @reactive.Effect
    @reactive.event(input.remove_player)
    def clear_drafted_players():
        # Extract selected player's data
        selected_row = input.draft_board_selected_rows()
        if selected_row:
            selected_player = draft_board_data().iloc[selected_row[0]]

            # Update removed_player_data by adding the removed player
            removed_players_data.set(pd.concat([removed_players_data(), draft_board_data().iloc[selected_row]]))

            # Update draft_board_data by excluding the drafted player
            draft_board_data.set(draft_board_data().drop(selected_row))

    # Function to add players back to draft board
    @reactive.Effect
    @reactive.event(input.add_player_in)
    def add_player_back_to_draft():
        # Extract selected player's data
        selected_row = input.draft_board_selected_rows()
        if selected_row:
            selected_player = removed_players_data().iloc[selected_row[0]]

            # Create a player_id for the selected player
            player_id = selected_player['name']

            # Update draft_board_data by adding the removed player
            draft_board_data.set(pd.concat([draft_board_data(), removed_players_data().iloc[selected_row]]).sort_values(by='rank'))

            # Update removed_player_data by excluding the drafted player
            removed_players_data.set(removed_players_data().drop(selected_row))

            drafted_players.set(drafted_players()[drafted_players()['player_id'] != player_id])
            roster.set([x for x in roster() if x not in player_id])
            
    @reactive.Effect
    @reactive.event(input.clear_drafts)
    def clear_drafted_players():
        drafted_players.set(pd.DataFrame(columns=['player_id']))
        roster.set([])
        draft_board_data.set(df)
        removed_players_data.set(df.head(0))

    @output
    @render.data_frame()
    def draft_board():
        return render.DataTable(filtered_data().assign(adp=lambda x: np.round(x['adp']))[[
            'name', 'team', 'position', 'rank', 'ppg', 'pos_rank', 'tier', 'adp', 'prob_of_avail_next', 'prob_of_avail_two_away'
        ]],row_selection_mode='multiple')
    
    # Plot Ceiling vs Floor
    @output
    @render_widget()
    def ceiling_floor_plot():
        color_discrete_map = {'QB':'#F8766D', 'RB': '#7CAE00', "WR": "#00B0F6", "TE": "#FF61CC",  "K": '#00BFC4', "DST": "#CD9600"}
        data = filtered_data().sort_values(by='rank').head(12)
        data['position'] = data['position'].cat.remove_unused_categories()
        fig = px.line(data, x='ppg_ceiling', y='name', color='position',
                         text=data.apply(lambda x: f"Name: {x['name']}<br>Avg PPG: {round(x['ppg'], 2)}<br>SD PPG: {round(x['sd_pts']/17, 2)}", axis=1),
                         color_discrete_map=color_discrete_map)
        fig.update_traces(marker=dict(symbol='square'))
        fig.update_layout(title='Ceiling vs Floor', xaxis_title='PPG', yaxis_title='Name')
        return fig

    # Plot Tier plot
    @output
    @render_widget()
    def tier_plot():
        data = filtered_data()[filtered_data()['tier'] < 10]
        fig = px.histogram(data, x='tier', color='position')
        fig.update_layout(title='Player Availibility', xaxis_title='Tier', yaxis_title='Availibility')
        return fig

    # Plot ADP vs Points
    @output
    @render_widget()
    def adp_points_plot():
        data = filtered_data()
        fig = px.scatter(data, x='adp', y='points', color='position',
                         text=data.apply(lambda x: f"Name: {x['name']}<br>Points: {round(x['points'])}<br>ADP: {round(x['adp'])}", axis=1))
        fig.update_layout(title="PPG vs ADP", xaxis_title="Average Draft Position (ADP)", yaxis_title="Projected Points")
        return fig

    # @output
    # @render.html
    # def roster():
    #     if len(roster()) > 0:
    #         my_team = df[df.apply(lambda x: f"{x['first_name']} {x['last_name']}" in roster(), axis=1)]
    #         my_team = my_team.assign(name=my_team['first_name'] + ' ' + my_team['last_name'])[['name', 'position']]

    #         roster_html = f"""
    #             <h2>My Roster</h2>
    #             <h4>QB</h4>
    #             <ul>{''.join([f"<li>{x}</li>" for x in my_team[my_team['position'] == "QB"]['name']])}</ul>
    #             <h4>RB</h4>
    #             <ul>{''.join([f"<li>{x}</li>" for x in my_team[my_team['position'] == "RB"]['name']])}</ul>
    #             <h4>WR</h4>
    #             <ul>{''.join([f"<li>{x}</li>" for x in my_team[my_team['position'] == "WR"]['name']])}</ul>
    #             <h4>TE</h4>
    #             <ul>{''.join([f"<li>{x}</li>" for x in my_team[my_team['position'] == "TE"]['name']])}</ul>
    #             <h4>K</h4>
    #             <ul>{''.join([f"<li>{x}</li>" for x in my_team[my_team['position'] == "K"]['name']])}</ul>
    #             <h4>DST</h4>
    #             <ul>{''.join([f"<li>{x}</li>" for x in my_team[my_team['position'] == "DST"]['name']])}</ul>
    #         """
    #         return BeautifulSoup(roster_html, 'html.parser')
    #     else:
    #         return BeautifulSoup("<p>Roster: Empty</p>", 'html.parser')

    # @output
    # @render.data_table
    # def removed_players():
    #     return removed_players_data()[['name', 'team', 'position', 'rank']]
    
app = App(app_ui, server)
uvicorn.run(app)

# # Assuming you have a file named app.py, with a Shiny app named app.
# uvicorn app:app --host 0.0.0.0 --port 80