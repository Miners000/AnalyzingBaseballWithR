library(tidyverse)
library(Lahman)
library(broom)

teams = Teams |> filter(yearID <= 1900) |> mutate(Wpct = W / (W+L), runDiff = R - RA)
teams = teams |> mutate(pythagWpct = R ^ 2 / (R ^ 2 + RA ^ 2))
teams = teams |> mutate(pythagResidual = Wpct - pythagWpct)
teams = teams |> mutate(logWratio = log(W/L), logRratio = log(R/RA))
teamsFiltered = teams |> filter(!is.na(logWratio) & !is.na(logRratio))
teamsFiltered = teamsFiltered |> filter(is.finite(logWratio), is.finite(logRratio))
pythagFit = lm(logWratio ~ 0 + logRratio, data = teamsFiltered)
pythagFit

teamsAug = augment(pythagFit, data = teamsFiltered)
basePlot = ggplot(teamsAug, aes(x = runDiff, y = .resid)) + geom_point(alpha = 0.3) + 
  geom_hline(yintercept = 0, linetype = 3) + xlab("Run Differential") + ylab("Residual")
highlightTeams = teamsAug |> arrange(desc(abs(.resid))) |> slice_head(n = 10)
worstAndBest = teamsAug |> arrange(.resid) |> filter(runDiff < -450 | runDiff > 360)
basePlot + geom_point(data = highlightTeams, color = "blue") + geom_text_repel(
  data = highlightTeams, color = "blue", aes(label = paste(teamID, yearID))) + 
  geom_text_repel(data = worstAndBest, color = "red", aes(label = paste(teamID, yearID)))
  

