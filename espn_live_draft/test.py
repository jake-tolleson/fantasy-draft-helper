import espn_live_draft as eld
import pandas as pd
def start_draft(team_name,driver='firefox'):
    print('launching driver - this will take a minute')
    d = eld.ds.draft_monitor(team_name, driver=driver)
    print('done')
    
team_name = 'TEAM TOLLESON'
browser = 'edge'

draft = eld.ds.draft_monitor(team_name, driver=browser)
draft.team_name = team_name
# sign into espn and go to your fantasy league
# join draft room
# leave both tabs OPEN

draft.configure_draft() # don't run this function until draft is set up

draft.update()

# see which functions work and which don't
dir(draft)
draft.team_map
draft.current_pick
draft.rosters
draft.pick_history 

