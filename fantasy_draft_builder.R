# Load packages in
library(ffanalytics)
library(tidyverse)
library(writexl)
library(stats)

# set season and week
season = 2023
week = 0
#saveRDS(my_scrape, '2023_week0.rds')
my_scrape <- readRDS('2023_week0.rds')
# Establish scoring rules for league
scoring_rules <- list(
  pass = list(
    pass_att = 0, pass_comp = 0, pass_inc = 0, pass_yds = 0.04, pass_tds = 4,
    pass_int = -1, pass_40_yds = 0,  pass_300_yds = 0, pass_350_yds = 0,
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
    list(threshold = 0, points = 10),
    list(threshold = 6, points = 7),
    list(threshold = 13, points = 4),
    list(threshold = 20, points = 1),
    list(threshold = 27, points = 0),
    list(threshold = 34, points = -1),
    list(threshold = 99, points = -4)
  )
)



# Scrape season data
my_scrape <- scrape_data(src = c( "ESPN", "CBS", "FantasyPros", "FantasySharks", "FFToday", "NumberFire", "NFL", "RTSports", "Walterfootball"), 
                         pos = c("QB", "RB", "WR", "TE", "K" ,"DST"),
                         season = season, week = week)

# Set weights for weighted average
my_weights <- c(ESPN=0.25,
                CBS = 0.15, 
                NFL = 0.075,
                FFToday = 0.1,
                NumberFire = 0.025, 
                FantasyPros = 0.25,
                FantasySharks= 0.025,
                WalterFootball = 0.025,
                RTSports= 0.1) 

my_weights <- c(ESPN=1/9,
                CBS = 1/9, 
                NFL = 1/9,
                FFToday = 1/9,
                NumberFire = 1/9, 
                FantasyPros = 1/9,
                FantasySharks= 1/9,
                WalterFootball = 1/9,
                RTSports= 1/9) 
df |> filter(adp - adp_sd * 0.8 <= 109) |> group_by(position) |> summarise(n=n())
# set replacement player
my_baseline <- c(QB = 13, RB = 39, WR = 43, TE = 11, K = 3, DST = 3)

# set cohen's D for tiers
my_tiers <- c(QB = 1, RB = 1, WR = 1, TE = 1, K = 1, DST = 0.1)

my_projections <- projections_table(my_scrape,
                                    scoring_rules = scoring_rules,
                                    src_weights = my_weights,
                                    vor_baseline = my_baseline,
                                    tier_thresholds = my_tiers) |>
                  add_player_info() |> 
                  add_ecr() |> 
                  add_uncertainty() |> 
                  add_adp(sources = c("RTS", "CBS", "Yahoo", "NFL", "FFC")) 

my_projections$adp_sd <- ifelse(is.na(my_projections$adp_sd), 1, my_projections$adp_sd)
my_projections$adp <- ifelse(is.na(my_projections$adp), 231, my_projections$adp)

projections <- my_projections |> 
                filter(avg_type == "average") |>
                select(first_name
                       ,last_name
                       ,team
                       ,position
                       ,points
                       ,sd_pts
                       ,dropoff
                       ,floor
                       ,ceiling
                       ,points_vor
                       ,floor_vor
                       ,ceiling_vor
                       ,rank
                       ,floor_rank
                       ,ceiling_rank
                       ,pos_rank
                       ,tier
                       ,overall_ecr
                       ,pos_ecr
                       ,sd_ecr
                       ,uncertainty
                       ,adp
                       ,adp_sd
                       ,adp_diff) |> 
                mutate(sleeper_score = adp-rank) |> 
                arrange(rank) |>
                arrange(adp) |> 
                mutate(mock_draft = row_number()) |>
                mutate(prob_of_availibility=1 - pnorm(floor(adp), mean = adp, sd = adp_sd)) 



projections |> write_xlsx('2023 draft.xlsx')
projections |> write_csv('2023 draft.csv')
#projections |> saveRDS('2023draft.rds')
