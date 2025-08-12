library(tidyverse)
library(Lahman)
library(broom)
library(ggrepel)
library(skimr)

managers = Managers |> filter(yearID >= 1998, yearID <= 2023, yearID != 2020) |> select(playerID, yearID, teamID, W, L)
teams = Teams |> filter(yearID >= 1998, yearID <= 2023) |> select(yearID, teamID, G, W, L, R, RA)
teams = teams |> mutate(Wpct = W / (W+L), runDiff = R - RA, pythagWpct = R ^ 2 / (R^2+RA^2), pythagResidual = Wpct - pythagWpct)
teams = teams |> inner_join(managers)
linFit = lm(Wpct ~ runDiff, data = teams)
teamsAug= augment(linFit, data = teams)
basePlot = ggplot(teamsAug, aes(x = runDiff, y = .resid)) + geom_point(alpha = 0.3) + 
  geom_hline(yintercept = 0, linetype = 3) + xlab("Run Differential") + ylab("Residual")
highlightTeams = teamsAug |> arrange(desc(abs(.resid))) |> slice_head(n = 30)
basePlot + geom_point(data = highlightTeams, color = "blue") + geom_text_repel(
  data = highlightTeams, color = "blue", aes(label = paste(teamID, yearID, playerID)), size = 2)
highlightTeams |> select(yearID, teamID, playerID, Wpct, pythagWpct, pythagResidual) |> print(n = 30)
highlightTeams |> count(playerID) |> print(n = 24)

avgManagerResiduals = teams |> group_by(playerID) |> summarize(avgPythagResdiual = mean(pythagResidual, na.rm = TRUE), seasonsManaged = n()) |> filter(seasonsManaged > 5) |> arrange(desc(avgPythagResdiual))
avgmanagerResiduals

totalManagerResiduals = teams |> group_by(playerID) |> summarize(totalPythagResdiual = sum(pythagResidual, na.rm = TRUE)) |> arrange(desc(totalPythagResdiual))
totalManagerResiduals


# remove desc() to get underperformers in each category