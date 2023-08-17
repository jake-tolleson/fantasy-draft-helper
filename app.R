source('ui.R', local = TRUE)
source('server.R')

library(shiny)
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(bslib)
library(reticulate)

# Helper function
get_next_pick_number <- function(total_drafted, starting_pick, total_teams){
  current_round <- total_drafted + 1
  if(current_round %% 2 == 0){
    next_pick <- current_round * total_teams - starting_pick + 1
  } else {
    next_pick <- (current_round-1)*total_teams+starting_pick
  }

  return(next_pick)
}

eld <- import("espn_live_draft")


# dataset
df <- readRDS('2023draft.rds')
df <- df %>%
  mutate(ppg = points/17) %>%
  mutate(ppg_ceiling = ceiling/17) %>%
  mutate(ppg_floor = floor/17) %>%
  mutate(name=paste(first_name, last_name)) %>%
  mutate(cov = sd_pts / points) %>%
  mutate(across(where(is.numeric), \(x) round(x, 2)))

df$position <- factor(df$position, levels=c('QB', 'RB', 'WR', 'TE', 'K', 'DST'))
# Extract column names from the dataset
drafted_player_columns <- colnames(df)

# Create an empty data frame with the same columns as the dataset
empty_drafted_players <- data.frame(matrix(ncol = length(drafted_player_columns), nrow = 0))
colnames(empty_drafted_players) <- drafted_player_columns

shinyApp(
  ui = my_ui,
  server = my_server
)
