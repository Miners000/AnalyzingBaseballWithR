library(tidyverse)
library(Lahman)
library(mlbplotR)

teams = Teams |> filter(yearID == 2022) |> 
  mutate(winpct = W / (W+L), ERA = ERA, teamAB = teamIDBR) |>
  left_join(team_abbr_lookup, by = "teamID")

ggplot(teams, aes(x = ERA, y = winpct)) +
  geom_mlb_scoreboard_logos(aes(team_abbr = teamAB), width = 0.07) + 
  scale_x_reverse() + geom_smooth()
ggsave("ERAvsWinpct2022.png")
