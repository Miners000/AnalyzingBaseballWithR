library(abdwr3edata)
library(tidyverse)
library(Lahman)
library(purrr)

crc_fc = c("#2905a1", "#e41a1c", "#4daf4a", "#984ea3")

#Set up data
retro2016 = read_csv("~/AnalyzingBaseballWithR/data_large/retrosheet/download.folder/unzipped/all2016.csv")
retro2016 = retro2016 |> retrosheet_add_states()
retro2016 = retro2016 |> mutate(
  bases = paste0(
    if_else(is.na(base1_run_id), 0, 1),
    if_else(is.na(base2_run_id), 0, 1),
    if_else(is.na(base3_run_id), 0, 1)
  ), state = paste(bases, outs_ct)
)
halfInnings = retro2016 |> mutate(runs = away_score_ct + home_score_ct, halfInningID = paste(game_id, inn_ct, bat_home_id)) |>
  group_by(halfInningID) |> summarize(outsInning = sum(event_outs_ct), runsInning = sum(runs_scored), runsStart = first(runs), maxruns = runsInning + runsStart)
retro2016Complete = retro2016 |> mutate(halfInningID = paste(game_id, inn_ct, bat_home_id)) |> inner_join(halfInnings, join_by(halfInningID)) |> filter(state != new_state | runs_scored > 0) |> 
  filter(outsInning == 3, bat_event_fl)
retro2016Complete = retro2016Complete |> mutate(new_state = str_replace(new_state, "[0-1]{3} 3", "3"))

#Start Matrixes
tMatrix = retro2016Complete |> select(state, new_state) |> table()
dim(tMatrix)

pMatrix = prop.table(tMatrix, 1)
dim(pMatrix)
pMatrix = pMatrix |> rbind("3" = c(rep(0, 24), 1))
dim(pMatrix)
pMatrix |> apply(MARGIN = 1, FUN = sum)

#Compare Transition Probabilities
pMatrix |> as_tibble(rownames = "state") |> filter(state == "000 0") |> pivot_longer(cols = -state, names_to = "new_state", values_to = "Prob") |> 
  filter(Prob > 0)
pMatrix |> as_tibble(rownames = "state") |> filter(state == "010 2") |> pivot_longer(cols = -state, names_to = "new_state", values_to = "Prob") |> 
  filter(Prob > 0)

#Simulating the Markov Chain
numNotScored = function(s)
{
  s |> str_split("") |> pluck(1) |> as.numeric() |> sum(na.rm = TRUE)
}

runnersOut = tMatrix |> row.names() |> set_names() |> map_int(numNotScored)
rRuns = outer(runnersOut + 1, runnersOut, FUN = "-") |> cbind("3" = rep(0, 24))

  #Probability transition matrix, run matrix, and state as inputs
simHalfInning = function(P, R, start = 1)
{
  s = start
  path = NULL
  runs = 0
  
  while(s < 25)
  {
    sNew = sample(1:25, size = 1, prob = P[s, ])
    path = c(path, sNew)
    runs = runs + R[s, sNew]
    s = sNew
  }
  runs
}

set.seed(111653)
simulatedRuns = 1:10000 |> map_int(~simHalfInning(tMatrix, rRuns))
table(simulatedRuns)
sum(simulatedRuns >= 5) / 10000
mean(simulatedRuns)

  #Get mean runs from state j
runsJ = function(J)
{
  1:10000 |> map_int(~simHalfInning(tMatrix, rRuns, J)) |> mean()
}

erm2016MC = tibble(state = row.names(tMatrix), meanRunValue = map_dbl(1:24, runsJ)) |> 
  mutate(bases = str_sub(state, 1, 3), outs_ct = as.numeric(str_sub(state, 5, 5))) |> select(-state)
erm2016MC |> pivot_wider(names_from = outs_ct, values_from = meanRunValue)


#Effect on non-batting plays
erm2016 = read_rds(here::here("~/AnalyzingBaseballWithR/re2016.rds"))
erm2016 |> inner_join(erm2016MC, join_by(bases, outs_ct)) |> mutate(runValueDiff = round(meanRunValue.x - meanRunValue.y, 2)) |> 
  select(bases, outs_ct, runValueDiff) |> pivot_wider(names_from = outs_ct, values_from = runValueDiff)

#After three PAs
pMatrix3 = pMatrix %*% pMatrix %*% pMatrix
p3Sorted = pMatrix3 |> as_tibble(rownames = "state") |> filter(state == "000 0") |> pivot_longer(cols = -state, names_to = "new_state", values_to = "Prob") |> 
  arrange(desc(Prob))
p3Sorted |> slice_head(n = 6)

#Average number of times state is the state
qMatrix = pMatrix[-25, -25]
nMatrix = solve(diag(rep(1, 24)) - qMatrix)
n000 = round(nMatrix["000 0", ], 2)
head(n000, n=6)
sum(n000)
avgNumPlaysFrmState = nMatrix %*% rep(1, 24) |> t() |> round(2)
avgNumPlaysFrmState[,1:8]

#Transition Matrix by Team
retro2016Complete = retro2016Complete |> mutate(home_team_id = str_sub(game_id, 1, 3), battingTeam = if_else(bat_home_id == 0, away_team_id, home_team_id))
tTeam = retro2016Complete |> group_by(battingTeam, state, new_state) |> count()
tTeam |> filter(battingTeam == "ANA") |> slice_head(n=6)

tTeamS = retro2016Complete |> filter(state == "100 2") |> group_by(battingTeam, state, new_state) |> tally()
tTeamS |> ungroup() |> sample_n(size = 6)
tWAS = tTeamS |> filter(battingTeam == "WAS") |> mutate(p = n / sum(n))
tALL = retro2016Complete |> filter(state == "100 2") |> group_by(new_state) |> tally() |> mutate(p = n / sum(n))
tWAS |> inner_join(tALL, by = "new_state") |> mutate(pEST = (n.x / (1274 + n.x)) * p.x + (1274 / (1274 + n.x)) * p.y) |> select(battingTeam, new_state, p.x, p.y, pEST)

#Make schedule
makeSchedule = function(teams, k)
{
  numTeams = length(teams)
  Home = rep(rep(teams, each = numTeams), k)
  Visitor = rep(rep(teams, numTeams), k)
  tibble(Home = Home, Visitor = Visitor) |> filter(Home != Visitor)
}

teams1968 = Teams |> filter(yearID == 1968) |> select(teamID, lgID) |> mutate(teamID = as.character(teamID)) |> group_by(lgID)
schedule1968 = teams1968 |> group_split() |> set_names(pull(group_keys(teams1968), "lgID")) |> map(~makeSchedule(teams = .x$teamID, k = 9)) |> list_rbind(names_to = "lgID")
dim(schedule1968)

#Sim talents and win probs
sTalent = 0.20 #Standard deviation
teams1968 = teams1968 |> mutate(talent = rnorm(10, 0, sTalent))
scheduleTalent = schedule1968 |> inner_join(teams1968, join_by(lgID, Home == teamID)) |> rename(talentHome = talent) |> inner_join(teams1968, join_by(lgID, Visitor == teamID)) |> rename(talentVisitor = talent)
scheduleTalent = scheduleTalent |> mutate(probHome = exp(talentHome) / (exp(talentHome) + exp(talentVisitor)))
slice_head(scheduleTalent, n=6)

#Sim regular season
scheduleTalent = scheduleTalent |> mutate(outcome = rbinom(nrow(scheduleTalent), 1, probHome), winner = if_else(outcome == 1, Home, Visitor))
scheduleTalent |> select(Visitor, Home, probHome, outcome, winner)
results1968 = scheduleTalent |> group_by(winner) |> summarize(Wins = n()) |> inner_join(teams1968, by = c("winner" = "teamID"))

#Sim postseason
winLeague = function(res)
{
  res |> group_by(lgID) |> mutate(tiebreaker = runif(n = length(talent)), 
                                  winsTotal = Wins + tiebreaker, rank = min_rank(desc(winsTotal)), isWinnerLg = winsTotal == max(winsTotal))
}

simOne1968 = winLeague(results1968)
wsWinner = simOne1968 |> filter(isWinnerLg) |> ungroup() |> mutate(outcome = as.numeric(rmultinom(1, 7, exp(talent))), isWinnerWS = outcome > 3) |> 
  filter(isWinnerWS) |> select(winner, isWinnerWS)
simOne1968 |> left_join(wsWinner, by = c("winner")) |> replace_na(list(isWinnerWS = 0))

set.seed(111653)
resultsWS1968 = one_simulation_68(0.20)
resultsWS1968

displayStandings = function(data, league)
{
  data |> filter(League == league) |> select(Team, Wins) |> mutate(Losses = 162 - Wins) |> arrange(desc(Wins))
}
map(1:2, displayStandings, data = resultsWS1968) |> bind_cols()

resultsWS1968 |> filter(Winner.Lg == 1) |> select(Team, Winner.WS)

#Sim many seasons
set.seed(111653)
manyResults = rep(0.20, 1000) |> map(one_simulation_68) |> list_rbind()
ggplot(manyResults, aes(Talent, Wins)) + geom_point(alpha = 0.05)

manyResults |> filter(Talent > -0.05, Talent < 0.05) |> ggplot(aes(Wins)) + geom_histogram(color = "blue", fill = "white")

fit1 = glm(Winner.Lg ~ Talent, data = manyResults, family = binomial)
fit2 = glm(Winner.WS ~ Talent, data = manyResults, family = binomial)
tdf = tibble(Talent = seq(-0.4, 0.4, length.out = 100))
tdf |> mutate(Pennant = predict(fit1, newdata = tdf, type = "response"), 'World Series' = predict(fit2, newdata = tdf, type = "response")) |> 
  pivot_longer(cols = -Talent, names_to = "Outcome", values_to = "Probability") |> ggplot(aes(Talent, Probability, color = Outcome)) + geom_line() + 
  ylim(0, 1) + scale_color_manual(values = crc_fc)
ggsave("OddsToWinByTalent.png")


#Exercise 1
P = matrix(c(.3, .7, 0, 0, 0, .3, .7, 0, 0, 0, .3, .7, 0, 0, 0, 1), 4, 4, byrow = TRUE)
P2 = P %*% P
P2 #Odds of 0 to 1 outs after 2 PAs is 0.42

N = solve(diag(c(1, 1, 1)) - P[-4, -4])
N #Avg num of ABs is 4.28

#Exercise 2
simulateHalfInning = function(P)
{
  s = 1
  path = NULL
  while(s < 4)
  {
    sNew = sample(1:4, 1, prob = P[s, ])
    path = c(path, sNew)
    s = sNew
  }
  length(path)
}
lengths = map(1:1000, ~ simulateHalfInning(P)) |> unlist()
meanLength = lengths |> mean()
lengths = lengths |> as.tibble() |> rename(plateAppearances = 1) |> group_by(plateAppearances) |> summarize(N = n())
lengths
meanLength

#Exercise 3
retroData = baseballr::retrosheet_data(here::here("AnalyzingBaseballWithR/data_large/retrosheet"), 1968)
retro1968 = read_csv("~/AnalyzingBaseballWithR/data_large/retrosheet/download.folder/unzipped/all1968.csv")
retro1968 = retro1968 |> retrosheet_add_states()
retro1968 = retro1968 |> mutate(
  bases = paste0(
    if_else(is.na(base1_run_id), 0, 1),
    if_else(is.na(base2_run_id), 0, 1),
    if_else(is.na(base3_run_id), 0, 1)
  ), state = paste(bases, outs_ct)
)
halfInnings1968 = retro1968 |> mutate(runs = away_score_ct + home_score_ct, halfInningID = paste(game_id, inn_ct, bat_home_id)) |>
  group_by(halfInningID) |> summarize(outsInning = sum(event_outs_ct), runsInning = sum(runs_scored), runsStart = first(runs), maxruns = runsInning + runsStart)
retro1968Complete = retro1968 |> mutate(halfInningID = paste(game_id, inn_ct, bat_home_id)) |> inner_join(halfInnings1968, join_by(halfInningID)) |> filter(state != new_state | runs_scored > 0) |> 
  filter(outsInning == 3, bat_event_fl)
retro1968Complete = retro1968Complete |> mutate(new_state = str_replace(new_state, "[0-1]{3} 3", "3"))

tMatrix1968 = retro1968Complete |> select(state, new_state) |> table()
runnersOut1968 = tMatrix1968 |> row.names() |> set_names() |> map_int(numNotScored)
rRuns1968 = outer(runnersOut1968 + 1, runnersOut1968, FUN = "-") |> cbind("3" = rep(0, 24))

simulatedRuns1968 = 1:10000 |> map_int(~simHalfInning(tMatrix1968, rRuns1968))
table(simulatedRuns1968)

erm1968MC = tibble(state = row.names(tMatrix1968), meanRunValue = map_dbl(1:24, runsJ)) |> 
  mutate(bases = str_sub(state, 1, 3), outs_ct = as.numeric(str_sub(state, 5, 5))) |> select(-state)
erm1968MC |> pivot_wider(names_from = outs_ct, values_from = meanRunValue)

#Exercise 4
teams1950 = Teams |> filter(yearID == 1950, lgID == "NL") |> select(teamID, lgID) |> mutate(teamID = as.character(teamID)) |> group_by(lgID)
schedule1950 = teams1950 |> group_split() |> set_names(pull(group_keys(teams1950), "lgID")) |> map(~makeSchedule(teams = .x$teamID, k = 11)) |> list_rbind(names_to = "lgID")

sTalent1950 = 0.25
teams1950 = teams1950 |> mutate(talent = rnorm(8, 0, sTalent1950))
scheduleTalent1950 = schedule1950 |> inner_join(teams1950, join_by(lgID, Home == teamID)) |> rename(talentHome = talent) |> inner_join(teams1950, join_by(lgID, Visitor == teamID)) |> rename(talentVisitor = talent)
scheduleTalent1950 = scheduleTalent1950 |> mutate(probHome = exp(talentHome) / (exp(talentHome) + exp(talentVisitor)))

scheduleTalent1950 = scheduleTalent1950 |> mutate(outcome = rbinom(nrow(scheduleTalent1950), 1, probHome), winner = if_else(outcome == 1, Home, Visitor))
scheduleTalent1950 |> select(Visitor, Home, probHome, outcome, winner)

results1950 = scheduleTalent1950 |> group_by(winner) |> summarize(Wins = n()) |> inner_join(teams1950, by = c("winner" = "teamID"))
results1950 |> arrange(desc(Wins))

#Exercise 5
simSeasons1950 = function(teams, k)
{
  sch = teams |> group_split() |> set_names(pull(group_keys(teams1950), "lgID")) |> map(~makeSchedule(teams = .x$teamID, k)) |> list_rbind(names_to = "lgID")
  
  teams = teams |> mutate(talent = rnorm(8, 0, 0.25))
  st = sch |> inner_join(teams, join_by(lgID, Home == teamID)) |> rename(talentHome = talent) |> inner_join(teams, join_by(lgID, Visitor == teamID)) |> rename(talentVisitor = talent)
  st = st |> mutate(probHome = exp(talentHome) / (exp(talentHome) + exp(talentVisitor)))
  
  st = st |> mutate(outcome = rbinom(nrow(st), 1, probHome), winner = if_else(outcome == 1, Home, Visitor))
  st |> select(Visitor, Home, probHome, outcome, winner)
  
  res = st |> group_by(winner) |> summarize(Wins = n()) |> inner_join(teams, by = c("winner" = "teamID"))
  
  maxTalent = res |> filter(talent == max(talent)) |> slice_sample(n = 1) |> pull(winner)
  maxWins = res |> filter(Wins == max(Wins)) |> slice_sample(n = 1) |> pull(winner)
  
  tibble(bestTalent = maxTalent, bestWins = maxWins)
}

sims1950 = map_dfr(1:1000, ~simSeasons1950(teams1950, 11))
sims1950
talentVsWins = sims1950 |> filter(bestTalent == bestWins) |> nrow()
talentVsWins

#Exercise 6
simWS = function(pAL)
{
  gameResults = c()
  gameCount = 1
  
  while(gameCount <= 7)
  {
    gameResult = ifelse(rbinom(1, 1, pAL), "AL", "NL")
    gameResults = c(gameResults, gameResult)
    gameCount = gameCount + 1
  }
  
  winner = if_else(sum(gameResults == "AL") >= 4, "AL", "NL")
  tibble(winner)
}

simsALBetter = map_dfr(1:1000, ~simWS(0.53742))
alBetterRows = simsALBetter |> filter(winner == "AL") |> nrow()
alBetterRows

simsEqual = map_dfr(1:1000, ~simWS(0.5))
equalRows = simsEqual |> filter(winner == "AL") |> nrow()
equalRows
