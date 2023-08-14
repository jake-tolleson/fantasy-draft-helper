from selenium import webdriver 
import pandas as pd 
import numpy as np
import re


flex_positions = ['RB','WR','TE']

class draft_monitor:
    def __init__(self, team_name, driver='firefox', driver_path=None):
        """
        initializing the draft monitor will open a selenium window and define
        most of the parameters for your draft.
        
        Log into your ESPN account and open the draft lobby using the selenium
        window. 
        
        Parameters
        ----------
        team_name : STRING
            team_name needs to match your ESPN draft name. Unless you've changed
            your team name, it defaults to "Team Your-Last-Name"
        driver : STRING, optional
            Used to indicate which driver used by Selenium. The default is 'firefox'.
            Valid entries are:
                'firefox'
                'chrome'
                'edge'
                'safari'
            All other entries will raise an exception.

        Returns
        -------
        draft_monitor class object.

        """
        self.team_name = team_name.upper()
        ddriver = driver.lower().strip()
        if driver_path is None:
            if ddriver == 'firefox':
                self.driver = webdriver.Firefox()
            elif ddriver == 'chrome':
                self.driver = webdriver.Chrome()
            elif ddriver == 'edge':
                self.driver = webdriver.Edge()
            elif ddriver == 'safari':
                self.driver = webdriver.Safari()
            else:
                raise NotImplementedError(f"driver '{driver}' does not match any Selenium drivers.")
        else:
            if ddriver == 'firefox':
                self.driver = webdriver.Firefox(executable_path=driver_path)
            elif ddriver == 'chrome':
                self.driver = webdriver.Chrome(executable_path=driver_path)
            elif ddriver == 'edge':
                self.driver = webdriver.Edge(executable_path=driver_path)
            elif ddriver == 'safari':
                self.driver = webdriver.Safari(executable_path=driver_path)
            else:
                raise NotImplementedError(f"driver '{driver}' does not match any Selenium drivers.")
    
        self.driver.get(r'https://www.espn.com')
        self.rosters = {}
        self.team_map = {}
        self.pick_history = pd.DataFrame(columns=['Player', 'Team', 'Position', 'Round', 'Pick', 'Selecting Team'])
        pass
    
    def configure_draft(self):
        """
        Do not run this function until your draft room is up.
        
        This function will:
            populate the initial rosters for each team (with nans b/c theyre empty)
            figure out the number of teams
            figure out your draft picks
            figure out what positions are required for your league
        
        """
        
        # Navigate to draft tab
        if self.driver.current_url.find('draft') < 0:
            urls = []
            for window in self.driver.window_handles:
                self.driver.switch_to.window(window)
                urls.append(self.driver.current_url)
            draft_window = self.driver.window_handles[np.argmax([url.find('draft?') for url in urls])]
            self.driver.switch_to.window(draft_window)
        
        self.update_lineups()
        self.teams = len(self.team_map)
        self.myteam = list(self.team_map.keys())[list(self.team_map.values()).index(self.team_name)]
        self.mypick = list(self.team_map.values()).index(self.team_name) + 1
        self.open_positions()
        self.rounds = self.empty_positions.sum()
        self.pick_order = (np.arange(1,self.teams+1).tolist() + np.arange(self.teams,0,-1).tolist()) * (self.rounds // 2)
        if self.rounds % 2 == 1:
            self.pick_order += list(range(1,self.teams+1))
        self.current_pick = 1
        pass

    def round_(self):
        return self.current_pick - 1 // self.teams + 1
    
    def pick_(self):
        return self.current_pick - 1 % self.teams + 1
    
    def update_lineups(self):
        teams = self.driver.find_elements("xpath", "/html/body/div[1]/div[1]/section/div/div[2]/main/div/div/div[3]/div[1]/div[1]/div[2]/div[1]/div/select/option")
        for t in teams:
            t.click()
            tnum = int(t.get_property('value'))
            team_ele = self.driver.find_elements("xpath","/html/body/div[1]/div[1]/section/div/div[2]/main/div/div/div[3]/div[1]/div[1]/div[2]/div[2]/div/div/div/div/div[2]/table/tbody/tr")
            team = pd.DataFrame([r.text.splitlines() for r in team_ele])[[0,1]].rename(columns={0:'position',1:'player'})
            team['player'] = team['player'].replace('Empty', np.nan)
            self.rosters[t.text] = team
            self.team_map[tnum] = t.text
            #{v: k for k, v in self.team_map.items()}
            
    # def update_rosters(self,specific_team=None):
    #     """
    #     This function scrapes the current team and rosters to keep track of 
    #     who has drafted who.
        
    #     If you only want to update 1 team (like yours), use the optional arg.
    #     "specific_team" which accepts an integer and correspond to the team's
    #     index within self.teams.
    #     """
    #     teams = self.driver.find_elements("xpath", "/html/body/div[1]/div[1]/section/div/div[2]/main/div/div/div[3]/div[1]/div[1]/div[2]/div[1]/div/select/option")
    #     if specific_team is None:
    #         for t in teams:
    #             t.click()
    #             tnum = int(t.get_property('value'))
    #             team_ele = self.driver.find_elements("xpath","/html/body/div[1]/div[1]/section/div/div[2]/main/div/div/div[3]/div[1]/div[1]/div[2]/div[2]/div/div/div/div/div[2]/table/tbody/tr")
    #             team = pd.DataFrame([r.text.splitlines() for r in team_ele])[[0,1]].rename(columns={0:'position',1:'player'})
    #             team['player'] = team['player'].replace('Empty', np.nan)
    #             self.rosters[tnum] = team
    #             self.team_map[tnum] = t.text
    #     else:
    #         st = specific_team
    #         t = teams[np.argmin(np.array([t.text for t in teams])  == st)]
    #         t.click()
    #         tnum = int(t.get_property('value'))
    #         team_ele = self.driver.find_elements("xpath","/html/body/div[1]/div[1]/section/div/div[2]/main/div/div/div[3]/div[1]/div[1]/div[2]/div[2]/div/div/div/div/div[2]/table/tbody/tr")
    #         team = pd.DataFrame([r.text.splitlines() for r in team_ele])[[0,1]].rename(columns={0:'position',1:'player'})
    #         team['player'] = team['player'].replace('Empty', np.nan)
    #         self.rosters[tnum] = team
    #         self.team_map[tnum] = t.text
    #     pass
    
    def update_pick_history(self):
        self.driver.find_element('xpath', '//*[@id="fitt-analytics"]/div/div[3]/div[3]/div/div[1]/div/div/label[3]').click()
        data = self.driver.find_element('xpath', '//*[@id="fitt-analytics"]/div/div[3]/div[3]/div/div[2]/div/ul').text.splitlines()
        
        players = []
        teams = []
        for item in data:
            match = re.match(r'^(.*?) / (\w{2,4}) (\w{1,3})', item)
            if match:
                name = match.group(1)
                team_abbr = match.group(2).upper()
                position = match.group(3)
                players.append((name, team_abbr, position))
                
            match = re.match(r'^(.*?), (\w{2,}) - (.*)$', item)
            if match:
                draft_round = match.group(1).removeprefix('R')
                pick = match.group(2).removeprefix('P')
                team = match.group(3).upper()
                teams.append((draft_round, pick, team))
        
        df = pd.DataFrame(zip(players, teams))
        
        # Unpack the DataFrame into 6 columns
        unpacked_df = df.apply(lambda row: pd.Series(row[0] + row[1]), axis=1)

        # Rename columns
        unpacked_df.columns = ['Player', 'Team', 'Position', 'Round', 'Pick', 'Selecting Team']
        
        self.pick_history = pd.concat([self.pick_history, unpacked_df])

        pass

    
    def open_positions(self):
        """
        Finds and counts your unfilled starting roster positions.
        """
        self.empty_positions = self.rosters[self.myteam][self.open_mask()]['position'].value_counts().rename({'D/ST':'DST'})
        if 'FLEX' in self.empty_positions.index:
            self.need_flex = True
        else:
            self.need_flex = False
        pass
    
    def open_mask(self):
        return self.rosters[self.myteam]['player'].isna()

    def update(self):
        """
        Scrapes the pick history to get a complete record of the drafting.
        
        Collects the entire draft history on every call to prevent errors in the
        history.        
        """
        self.update_lineups()
        self.current_pick = self.get_current_pick()
        self.update_pick_history()
        self.open_positions()
        pass

    
    def get_current_pick(self, exception=None):
        "calculate the current pick number"
        pick = 1
        for k,v in self.rosters.items():
            pick += v['player'].count()
        return pick

    
    def map_empty_positions(self,df):
        self.open_positions()
        df['needs'] = df['position'].map(self.empty_positions)
        df['needs'] = df['needs'].fillna(0)
        if self.need_flex:
            df['needs'] += np.where(df['position'].isin(flex_positions), 1, 0)
        
        return df
    


