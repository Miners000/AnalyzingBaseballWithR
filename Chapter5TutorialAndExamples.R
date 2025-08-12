library(abdwr3edata)
library(tidyverse)
library(Lahman)
library(broom)
library(ggrepel)
library(skimr)

crc_fc = c("#2905a1", "#e41a1c", "#4daf4a", "#984ea3")

retro2016 = read_csv(here::here("~/AnalyzingBaseballWithR/data_large/retrosheet/download.folder/unzipped/all2016.csv"))

#Create unique id for every half inning
retro2016 = retro2016 |> mutate(
  runsBefore = away_score_ct + home_score_ct,
  halfInning = paste(game_id, inn_ct, bat_home_id),
  runsScored = (bat_dest_id > 3) + (run1_dest_id > 3)
  + (run2_dest_id > 3) + (run3_dest_id > 3))

#Get max total runs
halfInnings = retro2016 |> group_by(halfInning) |> summarize(
  outsInning = sum(event_outs_ct),
  runsInning = sum(runsScored),
  runsStart = first(runsBefore),
  maxRuns = runsInning + runsStart
)

#Get current bases and outs
retro2016 = retro2016 |> inner_join(halfInnings, by = "halfInning") |> mutate(runsROI = maxRuns - runsBefore)
retro2016 = retro2016 |> mutate(
  bases = paste0(
    if_else(is.na(base1_run_id), 0, 1),
    if_else(is.na(base2_run_id), 0, 1),
    if_else(is.na(base3_run_id), 0, 1)
  ), state = paste(bases, outs_ct)
)

#Track change of outs, runners, or runs
retro2016 = retro2016 |> mutate(
  isRunner1st = as.numeric(run1_dest_id == 1 | bat_dest_id == 1),
  isRunner2nd = as.numeric(run1_dest_id == 2 | run2_dest_id == 2 | bat_dest_id == 2),
  isRunner3rd = as.numeric(run1_dest_id == 3 | run2_dest_id == 3 | run3_dest_id == 3 | bat_dest_id == 3),
  newOuts = outs_ct + event_outs_ct,
  newBases = paste0(isRunner1st, isRunner2nd, isRunner3rd),
  newState = paste(newBases, newOuts)
)

#Restrict to plays where change of baserunners or runs
changes2016 = retro2016 |> filter(state != newState | runsScored > 0)

#Adjust for inning with less than three outs (walk-offs, etc.)
changes2016Complete = changes2016 |> filter(outsInning == 3)

#Compute run expectancy
runExpectancy2016 = changes2016Complete |> group_by(bases, outs_ct) |> summarize(meanRunValue = mean(runsROI))
runExpectancy2016 |> pivot_wider(names_from = outs_ct, values_from = meanRunValue, names_prefix = "Outs = ")

#Add 2002 run expectancy and compare (fix later; 0s were NAs in old state var, meaning vector had to be reversed to compare)
runExpectancy2002 = tibble(
  "OLD = 0" = c(2.33, 1.51, 1.84, .90, 1.96, 1.14, 1.40, .51),
  "OLD = 1" = c(1.51, .94, 1.18, .54, 1.36, .68, .94, .27),
  "OLD = 2" = c(.78, .45, .52, .23, .63, .32, .36, .10)
)
out = runExpectancy2016 |> pivot_wider(names_from = outs_ct, values_from = meanRunValue, names_prefix = "NEW = ") |> bind_cols(runExpectancy2002)
out

#Get run value
retro2016 = retro2016 |> left_join(runExpectancy2016, join_by("bases", "outs_ct")) |> rename(rvStart = meanRunValue) |> 
  left_join(runExpectancy2016, join_by(newBases == bases, newOuts == outs_ct)) |> rename(rvEnd = meanRunValue) |> 
  replace_na(list(rvEnd = 0)) |> mutate(runValue = rvEnd - rvStart + runsScored)

#Get Jose Altuve Values
altuveID = People |> filter(nameFirst == "Jose", nameLast == "Altuve") |> pull(retroID)
altuve = retro2016 |> filter(bat_id == altuveID, bat_event_fl == TRUE)
altuve |> select(state, newState, runValue) |> slice_head(n = 3)
altuve |> group_by(bases) |> summarize(N = n())
ggplot(altuve, aes(bases, runValue)) + geom_jitter(width = 0.25, alpha = 0.5) + geom_hline(yintercept = 0, color = "red") + xlab("Runners on base")
ggsave("AltuveRunValueByBases.png")
runsAltuve = altuve |> group_by(bases) |> summarize(PA = n(), totalRunValues = sum(runValue))
runsAltuve
runsAltuve |> summarize(RE24 = sum(totalRunValues))

#Compare run expectancies across league
retro2016Bat = retro2016 |> filter(bat_event_fl == TRUE)
runExp = retro2016Bat |> group_by(bat_id) |> summarize(
  RE24 = sum(runValue), PA = length(runValue), runsStart = sum(rvStart)
)
runExp400 = runExp |> filter(PA >= 400)
runExp400 |> slice_head(n=6)
plot1 = ggplot(runExp400, aes(runsStart, RE24)) + geom_point() + geom_smooth() + geom_hline(yintercept = 0, color = "red")
plot1
runExp400 = runExp400 |> inner_join(People, by = c("bat_id" = "retroID"))
plot1 + geom_text_repel(data = filter(runExp400, RE24 >= 40), aes(label = nameLast))
ggsave("runExpectancy400BestLabeled.png")

#Get players' most common batting positions
regulars = retro2016 |> inner_join(runExp400, by = "bat_id")
positions = regulars |> group_by(bat_id, bat_lineup_id) |> summarize(N = n()) |> arrange(desc(N)) |> mutate(position = first(bat_lineup_id))
runExp400 = runExp400 |> inner_join(positions, by = "bat_id")
ggplot(runExp400, aes(runsStart, RE24, label = position)) + geom_text() + geom_hline(yintercept = 0, color = "red") + 
  geom_point(data = filter(runExp400, bat_id == altuveID), size = 4, shape = 16, color = "blue")
ggsave("runExpectancy400ByBattingPosition.png")

#Get run values of kinds of home runs
homeRuns = retro2016 |> filter(event_cd == 23)
homeRuns |> select(state) |> table() |> prop.table() |> round(3)
meanHrValue = homeRuns |> summarize(meanRunValue = mean(runValue))
meanHrValue
ggplot(homeruns, aes(runValue)) + geom_histogram() + geom_vline(data = meanHrValue, aes(xintercept = meanRunValue), color = "red", linewidth = 1.5) + 
  annotate("text", 1.7, 2000, label = "Mean Run \nValue", color = "red")
homeRuns |> arrange(desc(runValue)) |> select(state, newState, runValue) |> slice_head(n = 1)
ggsave("homeRunRunValues.png")

#Get run values of kinds of singles
singles = retro2016 |> filter(event_cd == 20)
meanSingleValue = singles |> summarize(meanRunValue = mean(runValue))
ggplot(singles, aes(runValue)) + geom_histogram(bins = 40) + geom_vline(data = meanSingleValue, color = "red", aes(xintercept = meanRunValue), linewidth = 1.5) + 
  annotate("text", 0.8, 4000, label = "Mean Run \nValue", color = "red")
singles |> select(state) |> table()
singles |> arrange(desc(runValue)) |> select(state, newState, runValue) |> slice_head(n = 1)
singles |> arrange(runValue) |> select(state, newState, runValue) |> slice_head(n = 1)

#Get run value of steals
stealing = retro2016 |> filter(event_cd %in% c(4, 6))
stealing |> group_by(event_cd) |> summarize(N = n()) |> mutate(pct = N / sum(N))
stealing |> group_by(state) |> summarize(N = n())
ggplot(stealing, aes(runValue, fill = factor(event_cd))) + geom_histogram() + scale_fill_manual(
  name = "event_cd", values = crc_fc, labels = c("Stolen Base (SB)", "Caught Stealing CS)")
)
ggsave("StolenBaseAttemptsByResultAndState.png")
stealing1001 = stealing |> filter(state == "100 1")
stealing1001 |> group_by(event_cd) |> summarize(N = n()) |> mutate(pct = N / sum(N))
stealing1001 |> group_by(newState) |> summarize(N = n()) |> mutate(pct = N / sum(N))
stealing1001 |> filter(event_cd == 6) |> summarize(mean = mean(runValue))

#Exercise One: run value of doubles and triples
doubles = retro2016 |> filter(event_cd == 21)
meanDoubleValue = doubles |> summarize(meanRunValue = mean(runValue))
meanDoubleValue
triples = retro2016 |> filter(event_cd == 22)
meanTripleValue = triples |> summarize(meanRunValue = mean(runValue))
meanTripleValue

#Exercise Two: run value of singles, walks, and hbps
walksRF = retro2016 |> filter(event_cd %in% c(14, 15) & (state == "100 0" | state == "100 1" | state == "100 2"))
meanWalkRFValue = walksRF |> summarize(meanRunValue = mean(runValue))
meanWalkRFValue
walksRF |> select(state) |> table()

hbpsRF = retro2016 |> filter(event_cd == 16 & (state == "100 0" | state == "100 1" | state == "100 2"))
meanHBPRFValue = hbpsRF |> summarize(meanRunValue = mean(runValue))
meanHBPRFValue
hbpsRF |> select(state) |> table()

singlesRF = retro2016 |> filter(event_cd == 20 & (state == "100 0" | state == "100 1" | state == "100 2"))
meanSinglesRFValue = singlesRF |> summarize(menaRunValue = mean(runValue))
meanSinglesRFValue

#Exercise Three: Adam Eaton vs Starling Marte
eatonID = "eatoa002"
eaton = retro2016 |> filter(bat_id == eatonID, bat_event_fl == TRUE)
runsEaton = eaton |> summarize(PA = n(), totalRunValues = sum(runValue))
runsEaton
positionsEaton = eaton |> group_by(bat_lineup_id) |> summarize(N = n()) |> arrange(desc(N)) |> mutate(position = first(bat_lineup_id))
positionsEaton |> slice_head(n = 1)

marteID = "marts002"
marte = retro2016 |> filter(bat_id == marteID, bat_event_fl == TRUE)
runsMarte = marte |> summarize(PA = n(), totalRunValues = sum(runValue))
runsMarte
positionsMarte = marte |> group_by(bat_lineup_id) |> summarize(N = n()) |> arrange(desc(N)) |> mutate(position = first(bat_lineup_id))
positionsMarte |> slice_head(n = 1)

#Exercise Four: Probability of Scoring Matrix
scoringProb2016 = changes2016Complete |> group_by(bases, outs_ct) |> summarize(percentScored = sum(runsROI > 0) / n())
scoringProb2016 |> pivot_wider(names_from = outs_ct, values_from = percentScored, names_prefix = "Outs = ")

#Exercise Five: Advancement with Single
singlesEx5 = changes2016 |> filter(event_cd == 20)
singlesEx5 = singlesEx5 |> group_by(state, newState) |> summarize(N = n())
print(singlesEx5, n = 192)

#Exercise Six: Players by Run Values (Trout, Ortiz, Votto)
troutID = People |> filter(nameFirst == "Mike", nameLast == "Trout") |> pull(retroID)
trout = retro2016 |> filter(bat_id == troutID, bat_event_fl == TRUE)
trout |> group_by(bases) |> summarize(N = n())
ggplot(trout, aes(bases, runValue)) + geom_jitter(width = 0.25, alpha = 0.5) + geom_hline(yintercept = 0, color = "red") + xlab("Runners on base")

ortizID = People |> filter(nameFirst == "David", nameLast == "Ortiz") |> pull(retroID)
ortiz = retro2016 |> filter(bat_id == ortizID, bat_event_fl == TRUE)
ortiz |> group_by(bases) |> summarize(N = n())
ggplot(ortiz, aes(bases, runValue)) + geom_jitter(width = 0.25, alpha = 0.5) + geom_hline(yintercept = 0, color = "red") + xlab("Runners on base")

vottoID = People |> filter(nameFirst == "Joey", nameLast == "Votto") |> pull(retroID)
votto = retro2016 |> filter(bat_id == vottoID, bat_event_fl == TRUE)
votto |> group_by(bases) |> summarize(N = n())
ggplot(votto, aes(bases, runValue)) + geom_jitter(width = 0.25, alpha = 0.5) + geom_hline(yintercept = 0, color = "red") + xlab("Runners on base")
