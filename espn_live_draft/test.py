import espn_live_draft as eld

def start_draft(team_name,driver='firefox'):
    print('launching driver - this will take a minute')
    d = eld.ds.draft_monitor(team_name, driver=driver)
    print('done')
    
team_name = 'TEAM LASTNAME'
browser = 'firefox'

draft = eld.ds.draft_monitor(team_name, driver=browser)

# sign into espn and go to your fantasy league
# join draft room
# leave both tabs OPEN

draft.configure_draft() # don't run this function until draft is set up

draft.update()

# see which functions work and which don't
dir(draft)
