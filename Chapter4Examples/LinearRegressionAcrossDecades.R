library(tidyverse)
library(Lahman)
library(broom)

teams1960s = Teams |> filter(yearID >= 1960, yearID <= 1969) |> select(teamID, yearID, lgID, G, W, L, R, RA)
teams1970s = Teams |> filter(yearID >= 1970, yearID <= 1979) |> select(teamID, yearID, lgID, G, W, L, R, RA)
teams1980s = Teams |> filter(yearID >= 1980, yearID <= 1989) |> select(teamID, yearID, lgID, G, W, L, R, RA)
teams1990s = Teams |> filter(yearID >= 1990, yearID <= 1999) |> select(teamID, yearID, lgID, G, W, L, R, RA)

teams1960s = teams1960s |> mutate(runDiff = R - RA, Wpct = W / (W+L))
teams1970s = teams1970s |> mutate(runDiff = R - RA, Wpct = W / (W+L))
teams1980s = teams1980s |> mutate(runDiff = R - RA, Wpct = W / (W+L))
teams1990s = teams1990s |> mutate(runDiff = R - RA, Wpct = W / (W+L))

runDiff1960s = ggplot(teams1960s, aes(x = runDiff, y = Wpct)) + geom_point() + 
  scale_x_continuous("Run Differential") + scale_y_continuous("Win Percentage")
runDiff1970s = ggplot(teams1970s, aes(x = runDiff, y = Wpct)) + geom_point() + 
  scale_x_continuous("Run Differential") + scale_y_continuous("Win Percentage")
runDiff1980s = ggplot(teams1980s, aes(x = runDiff, y = Wpct)) + geom_point() + 
  scale_x_continuous("Run Differential") + scale_y_continuous("Win Percentage")
runDiff1990s = ggplot(teams1990s, aes(x = runDiff, y = Wpct)) + geom_point() + 
  scale_x_continuous("Run Differential") + scale_y_continuous("Win Percentage")

linFit1960s = lm(Wpct ~ runDiff, data = teams1960s)
linFit1960s
linFit1970s = lm(Wpct ~ runDiff, data = teams1970s)
linFit1970s
linFit1980s = lm(Wpct ~ runDiff, data = teams1980s)
linFit1980s
linFit1990s = lm(Wpct ~ runDiff, data = teams1990s)
linFit1990s
