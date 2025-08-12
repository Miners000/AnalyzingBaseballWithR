library(abdwr3edata)
library(tidyverse)
library(Lahman)

players = People |> filter(playerID %in% c(
  "cobbty01", "willite01", "rosepe01")) |> 
  mutate(mlbBirthYear = if_else(birthMonth >= 7, birthYear + 1, birthYear), 
         Player = paste(nameFirst, nameLast)) |>
  select(playerID, Player, mlbBirthYear)

hitData = Batting |> inner_join(players, by = "playerID") |>
  mutate(Age = yearID - mlbBirthYear) |>
  select(Player, Age, H) |>
  group_by(Player) |>
  mutate(cH = cumsum(H))

ggplot(hitData, aes(x = Age, y = cH, color = Player)) + geom_line()
ggsave("RoseCobbWilliamsCareerHits.png")
