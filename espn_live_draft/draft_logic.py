import pandas as pd
import numpy as np

# df = pd.read_csv(r'C:\Users\james.whiting\OneDrive - Shell\Documents\ffa_customrankings2020-0.csv')
oc_mult = dict(zip(['QB','RB','WR','TE','K','DST'],[.3,.5,.5,.3,.15,.15]))

map_mult = lambda df: df['position'].map(oc_mult)

def next_picks(pick_order, mypick, thispick):
    next_pick = pick_order.index(mypick,thispick) + 1
    next_pick2 = pick_order.index(mypick,next_pick) + 1
    return next_pick, next_pick2

def p2f(x):
    return float(x.strip('%'))/100

def expected_max(df):
    df['pbest'] = df['probpicked'].cumprod().shift().fillna(1)
    df['emaxw'] = df['pbest'] * df['%']
    df['emax'] = df['points'] * df['emaxw']
    return df

def adj_probs(players,probs, next_pick, next_pick2):
    players['Name'] = players['player'].astype(str)
    probs['Name'] = probs['probname'].astype(str)
    for i, pick in enumerate([next_pick,next_pick2]):
        pmask = probs['pick'] == pick
        df = players.merge(probs[pmask], how='left', left_on='Name', right_on='Name')
        df.index = df['Name']
        df.index.name = None
        df['%'] = df['%'].fillna(1)
        df = df.sort_values('%')
        df['%'] = np.where(df['picked'] | df['blacklist'], 0, df['%'])
        df['probpicked'] = 1 - df['%']
        df = df.sort_values(['probpicked','points'],ascending=False)
        df = df.groupby('position').apply(expected_max)
        palt = df.groupby('position')['emax'].sum()
        print(f'P+{i+1}: Alternatives')
        print(palt)
        print('')
        df['opp'] = df['position'].map(palt)
        players[f'oc_raw_{i}'] = (df['points'] - df['opp']) * ~df['picked'] * ~df['blacklist']
        players[f'oc_adj_{i}'] = players[f'oc_raw_{i}'] * players['oc_adj'] * df['probpicked']
        players[f'pb_{i}'] = df['probpicked']
    return players.copy()



def unblacklist_player(name,team,df):
    name_match = df['player'] == name
    team_match = df['team'] == team
    mask = name_match & team_match
    print("Blacklisting these players:")
    print(df[mask][['player','team','position']])
    df['blacklist'] = np.where(mask, False, df['blacklist'])
    return df

def blacklist_player(name,team,df):
    name_match = df['player'] == name
    team_match = df['team'] == team
    mask = name_match & team_match
    print("Blacklisting these players:")
    print(df[mask][['player','team','position']])
    df['blacklist'] = np.where(mask, True, df['blacklist'])
    return df

def top_picks(df):
    cols = ['player','team','position','points','adp','oc_adj_0','oc_adj_1']
    print(df.sort_values('oc_adj_1',ascending=False).head(10)[cols])

