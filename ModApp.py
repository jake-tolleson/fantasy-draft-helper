import pandas as pd
import uvicorn
from shiny import *
from ui import create_ui
from server import create_server

# Helper function
def get_next_pick_number(total_drafted, starting_pick, total_teams):
    current_round = total_drafted + 1
    if current_round % 2 == 0:
        next_pick = current_round * total_teams - starting_pick + 1
    else:
        next_pick = (current_round-1)*total_teams+starting_pick
    return next_pick

# dataset
df = pd.read_csv('2023draft.csv')
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

if __name__ == "__main__":
    app_ui = create_ui()
    app_server = create_server(app_ui)

    app = shiny.App(app_ui, app_server)
    uvicorn.run(app)
