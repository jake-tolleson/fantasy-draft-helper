library(reticulate)
library(tidyverse)
library(ffanalytics)
library(nflreadr)
library(plotly)

setwd("~/Documents/fantasy-draft-helper")
espn <- import('espn_api')


### League Keys
league_id <- 12345 # Your league ID
season <- 2024 # Season to connect.
espn_s2 <- 's2_key' # Your personal espn s2 id
swid <- 'swid' # swid

py_run_string(paste("from espn_api.football import League"))
py_run_string(paste0("league = League(",league_id, ",", season, ",'", espn_s2, "','", swid, "')"))


league <- py$league 
league$refresh()
free_agents <- league$free_agents(size=1000) 
league$year

league$standings()

teams <- lapply(league$teams, function(x) x$team_name) |> unlist()
my_team <- which(teams == 'Your Team Name')

league$teams[[my_team]]$playoff_pct

my_players  <- lapply(league$teams[[my_team]]$roster, function(x) x$name) |> 
  unlist() |> 
  str_replace('D/ST', '') |> 
  clean_player_names() |> 
  as_tibble_col(column_name='player') 


# set season and week
season = league$year
week = league$current_week

# Scrape season data
my_scrape <- scrape_data(src = c( "ESPN", "CBS", "FantasyPros", "FantasySharks", "FFToday", "NumberFire", "NFL", "RTSports", "Walterfootball"), 
                         pos = c("QB", "RB", "WR", "TE", "K" ,"DST"),
                         season = season, week = week)

scoring_rules <- list(
  pass = list(
    pass_att = 0, pass_comp = 0, pass_inc = 0, pass_yds = 0.04, pass_tds = 4,
    pass_int = -2, pass_40_yds = 0,  pass_300_yds = 0, pass_350_yds = 0,
    pass_400_yds = 0
  ),
  rush = list(
    all_pos = TRUE,
    rush_yds = 0.1,  rush_att = 0, rush_40_yds = 0, rush_tds = 6,
    rush_100_yds = 0, rush_150_yds = 0, rush_200_yds = 0),
  rec = list(
    all_pos = TRUE,
    rec = 1, rec_yds = 0.1, rec_tds = 6, rec_40_yds = 0, rec_100_yds = 0,
    rec_150_yds = 0, rec_200_yds = 0
  ),
  misc = list(
    all_pos = TRUE,
    fumbles_lost = -2, fumbles_total = 0,
    sacks = 0, two_pts = 2
  ),
  kick = list(
    xp = 1.0, fg_0019 = 3.0,  fg_2029 = 3.0, fg_3039 = 3.0, fg_4049 = 4.0,
    fg_50 = 5.0,  fg_miss = -1.0
  ),
  ret = list(
    all_pos = TRUE,
    return_tds = 6, return_yds = 0
  ),
  idp = list(
    all_pos = TRUE,
    idp_solo = 0, idp_asst = 0, idp_sack = 1, idp_int = 2,  idp_fum_force = 0,
    idp_fum_rec = 2,  idp_pd = 1, idp_td = 6,  idp_safety = 2
  ),
  dst = list(
    dst_fum_rec = 2,  dst_int = 2, dst_safety = 2, dst_sacks = 1, dst_td = 6,
    dst_blk = 2, dst_ret_yds = 0, dst_pts_allowed = 0
  ),
  pts_bracket = list(
    list(threshold = 0, points = 5),
    list(threshold = 6, points = 4),
    list(threshold = 13, points = 3),
    list(threshold = 17, points = 1),
    list(threshold = 27, points = 0),
    list(threshold = 34, points = -1),
    list(threshold = 45, points = -3),
    list(threshold = 99, points = -5)
  )
)

my_weights <- c(ESPN=0,
                CBS = 1/3, 
                NFL = 1/3,
                FFToday = 0,
                NumberFire = 1/3, 
                FantasyPros = 0,
                FantasySharks= 0,
                WalterFootball = 0,
                RTSports= 0) 

my_projections <- projections_table(my_scrape,
                                    scoring_rules = scoring_rules) |>
  add_player_info() |> 
  add_ecr() |> 
  add_uncertainty() 



projections <- my_projections |> 
  filter(avg_type == "average") |>
  mutate(player = clean_player_names(case_when(position=='DST'~last_name
                                               ,TRUE~paste(first_name, last_name)))) |> 
  select(player
         ,team
         ,position
         ,points
         ,sd_pts
         ,floor
         ,ceiling
         ,pos_ecr
         ,sd_ecr
         ,uncertainty)


my_roster <- my_players |> 
  inner_join(projections, 'player') |> 
  arrange(desc(points))

league_free_agents <- free_agents |> 
  lapply(function(x) x$name) |> 
  unlist() |> 
  str_replace('D/ST','') |> 
  clean_player_names() |> 
  as_tibble_col(column_name='player') |> 
  inner_join(projections, 'player')

available_players <- my_roster |> 
  bind_rows(league_free_agents) |> 
  mutate(my_player = case_when(player %in% my_players$player ~ 1,
                               TRUE ~ 0)) |> 
  arrange(desc(points))

p <- ggplot(my_roster, aes(x = ceiling, y = reorder(player, points), col=position,
                           text = paste(
                             "Name: ", player,
                             paste0("<br>Ceiling: "), round(ceiling,1),
                             paste0("<br>Avg: "), round(points,1),
                             paste0("<br>Floor: "), round(floor,1)
                           ))) +
  geom_point(aes(x=points), shape=23, size=3, fill=24) +
  geom_segment(aes(xend = floor, yend = player), linewidth = 1) +
  scale_fill_discrete(breaks = c('QB', 'RB', 'WR', 'TE', 'K', 'DST')) +
  scale_colour_manual(values=c("QB"="#F8766D", "RB"="#7CAE00", "WR"="#00B0F6", "TE"="#FF61CC",  "K"='#00BFC4', "DST"="#CD9600"), 
                      labels=c("QB", "RB", "WR", "TE", "K", "DST")) +
  labs(title = '', x = 'points', y = 'Name') +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

ggplotly(p, tooltip = c('text'))



p <- ggplot(available_players |> filter(position %in% c('WR', 'RB', 'TE')) |> slice_head(n=15), 
            aes(x = ceiling, y = reorder(player, points), col=position,
                text = paste(
                  "Name: ", player,
                  paste0("<br>Ceiling: "), round(ceiling,1),
                  paste0("<br>Avg: "), round(points,1),
                  paste0("<br>Floor: "), round(floor,1)
                ))) +
  geom_point(aes(x=points), shape=23, size=3, fill=24) +
  geom_segment(aes(xend = floor, yend = player), linewidth = 1) +
  scale_fill_discrete(breaks = c('QB', 'RB', 'WR', 'TE', 'K', 'DST')) +
  scale_colour_manual(values=c("QB"="#F8766D", "RB"="#7CAE00", "WR"="#00B0F6", "TE"="#FF61CC",  "K"='#00BFC4', "DST"="#CD9600"), 
                      labels=c("QB", "RB", "WR", "TE", "K", "DST")) +
  labs(title = '', x = 'points', y = 'Name') +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

ggplotly(p, tooltip = c('text'))
