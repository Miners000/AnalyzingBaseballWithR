library(abdwr3edata)
library(tidyverse)
library(Lahman)
library(broom)
library(ggrepel)
library(skimr)
library(fs)
library(purrr)
library(data.table)
library(mgcv)
library(modelr)
library(lme4)

crc_fc = c("#2905a1", "#e41a1c", "#4daf4a", "#984ea3")

#Gather 2022 Statcast Data
dataDir = here::here("~/AnalyzingBaseballWithR/data_large")
statcastDir = path(dataDir, "statcastCSV")
if(!dir.exists(statcastDir))
{
  dir.create(statcastDir)
}
mlb2022Dates = as_date(ymd("2022-01-01") + days(97:278 - 1))
head(mlb2022Dates)
walk(mlb2022Dates, ~ statcast_daily(.x, "~/AnalyzingBaseballWithR/data_large/statcastCSV"))
statcast_season(2022, dir = statcastDir)

#Combine and validate
sc2022 = statcastDir |> statcast_read_csv(pattern = "2022.+\\.csv")
sc2022 |> group_by(game_type) |> summarize(numGames = n_distinct(game_pk), numPitches = n(), numHR = sum(events == "home_run", na.rm = TRUE))

#Get random sample of taken pitches
sc2022 <- sc2022 |> 
  mutate(outcome = case_match(
      description, 
      c("ball", "blocked_ball", "pitchout", "hit_by_pitch") ~ "ball", 
      c("swinging_strike", "swinging_strike_blocked", "foul", "foul_bunt", 
        "foul_tip", "hit_into_play", "missed_bunt") ~ "swing", 
      "called_strike" ~ "called_strike"
    ),home = ifelse(inning_topbot == "Bot", 1, 0), count = paste(balls, strikes, sep = "-")
  )

takenPitches = sc2022 |> filter(outcome != "swing")
takenSelect = select(takenPitches, pitch_type, release_speed, description, stand, p_throws, outcome, plate_x, plate_z, 
                     pitcher, batter, count, home, zone, fielder_2)
fwrite(takenSelect, here::here("~/AnalyzingBaseballWithR/data_large/statcastCSV/scTaken2022.csv"), compress = "gzip")
scTaken = fread(here::here("~/AnalyzingBaseballWithR/data_large/statcastCSV/scTaken2022.csv"))
set.seed(12345)
takenPitches = sample_n(scTaken, 50000)

#Get strike zone and plot
plateWidth = 17 + 2 * (9/pi)
kZonePlot = ggplot(NULL, aes(x = plate_x, y = plate_z)) + geom_rect(
  xmin = -(plateWidth/2)/12, xmax = (plateWidth/2)/12, ymin = 1.5, ymax = 3.6, color = "#2905a1", alpha = 0) +
  coord_equal() + scale_x_continuous("Horizontal location (ft.)", limits = c(-2, 2)) + scale_y_continuous("Vertical location (ft.)", limits = c(0, 5))
kZonePlot %+% sample_n(takenPitches, size = 2000) + aes(color = outcome) + geom_point(alpha = 0.2) + scale_color_manual(values = crc_fc)
ggsave("takenBallsAndStrikes2022.png")

#Get strike % by zone
zones = takenPitches |> group_by(zone) |> summarize(N = n(), rightEdge = min(1.5, max(plate_x)), leftEdge = max(-1.5, min(plate_x)), topEdge = min(5, quantile(plate_z, 0.95, na.rm = TRUE)), bottomEdge = max(0, quantile(plate_z, 0.05, na.rm = TRUE)), strikePct = sum(outcome == "called_strike") / n(), plate_x = mean(plate_x), plate_z = mean(plate_z))
kZonePlot %+% zones + geom_rect(aes(xmax = rightEdge, xmin = leftEdge, ymax = topEdge, ymin = bottomEdge, fill = strikePct, alpha = strikePct), color = "lightgray") +
  geom_text_repel(size = 3, aes(label = round(strikePct, 2), color = strikePct < 0.5)) + scale_fill_gradient(low = "gray70", high = "#2905a1") + scale_color_manual(values = crc_fc) + guides(color = FALSE, alpha = FALSE)

#Called Strike % Model (cont., points)
strikeMod = gam(outcome == "called_strike" ~ s(plate_x, plate_z), family = binomial, data = takenPitches)
hats = strikeMod |> augment(type.predict = "response")
kZonePlot %+% sample_n(hats, 10000) + geom_point(aes(color = .fitted), alpha = 0.1) + scale_color_gradient(low = "gray70", high = "#2905a1")                
ggsave("strikeCallPct.png")

#Visualizing estimated surface (fine surface)
grid = takenPitches |> data_grid(plate_x = seq_range(plate_x, n = 100), plate_z = seq_range(plate_z, n = 100))
gridHats = strikeMod |> augment(type.predict = "response", newdata = grid)
tilePlot = kZonePlot %+% gridHats + geom_tile(aes(fill = .fitted), alpha = 0.7) + scale_fill_gradient(low = "gray92", high = "#2905a1")
tilePlot
ggsave("calledStrikePctTile.png")

#Controlling for handedness
handMod = gam(outcome == "called_strike" ~ p_throws + stand + s(plate_x, plate_z), family = binomial, data = takenPitches)
handGrid = takenPitches |> data_grid(plate_x = seq_range(plate_x, n = 100), plate_z = seq_range(plate_z, n = 100), p_throws, stand)
handGridHats = handMod |> augment(type.predict = "response", newdata = handGrid)
tilePlot %+% handGridHats + facet_grid(p_throws ~ stand)
ggsave("calledStrikePctByHands.png")
diffs = handGridHats |> group_by(plate_x, plate_z) |> summarize(N = n(), .fitted = sd(.fitted), .groups = "drop")
tilePlot %+% diffs
ggsave("calledStrikeSD.png")

#Modeling Catcher Framing (Catcher Only)
takenPitches = takenPitches |> filter(is.na(plate_x) == FALSE, is.na(plate_z) == FALSE) |> mutate(strikeProb = predict(strikeMod, type = "response"))
modA = glmer(outcome == "called_strike" ~ strikeProb + (1|fielder_2), data = takenPitches, family = binomial)
fixed.effects(modA)
VarCorr(modA)
cEffects = modA |> ranef() |> as_tibble() |> transmute(id = as.numeric(levels(grp)), effect = condval)
masterID = baseballr::chadwick_player_lu() |> mutate(mlbName = paste(name_first, name_last), mlbID = key_mlbam) |> select(mlbID, mlbName) |> filter(!is.na(mlbID))
cEffects = cEffects |> left_join(select(masterID, mlbID, mlbName), join_by(id == mlbID)) |> arrange(desc(effect))
cEffects |> slice_head(n = 6)
cEffects |> slice_tail(n = 6)

#Modeling Catcher Framing (with resp to pitcher and batter)
modB = glmer(outcome == "called_strike" ~ strikeProb + (1|fielder_2) + (1|batter) + (1|pitcher), data = takenPitches, family = binomial)
VarCorr(modB)
cEffects = modB |> ranef() |> as_tibble() |> filter(grpvar == "fielder_2") |> transmute(id = as.numeric(as.character(grp)), effect = condval)
cEffects = cEffects |> left_join(select(masterID, mlbID, mlbName), join_by(id == mlbID)) |> arrange(desc(effect))
cEffects |> slice_head(n = 6)
cEffects |> slice_tail(n = 6)

#Exercise 1 (run lines 33 and 43 first)
seqX = seq(-1.4, 1.4, by = 0.4)
seqZ = seq(1.1, 3.9, by = 0.4)
takenPitches = takenPitches |> mutate(plate_x = cut(plate_x, seqX), plate_z = cut(plate_z, seqZ))
takenPitches = takenPitches |> group_by(plate_x, plate_z) |> summarize(numPitches = n(), totalStrikes = sum(ifelse(outcome == 'called_strike', 1, 0)), pctStrikes = totalStrikes / numPitches)
takenPitches |> print(n = 64)

#Exercise 2 (run lines 33 and 43 first)
takenPitches = takenPitches |> mutate(plate_x = cut(plate_x, seqX), plate_z = cut(plate_z, seqZ))
takenPitches = takenPitches |> group_by(plate_x, plate_z, stand) |> summarize(numPitches = n(), totalStrikes = sum(ifelse(outcome == 'called_strike', 1, 0)), pctStrikes = totalStrikes / numPitches)
takenPitches |> print(n = 128)

#Exercise 3
takenPitches = takenPitches |> mutate(plate_x = cut(plate_x, seqX), plate_z = cut(plate_z, seqZ))
takenPitches = takenPitches |> group_by(plate_x, plate_z, p_throws) |> summarize(numPitches = n(), totalStrikes = sum(ifelse(outcome == 'called_strike', 1, 0)), pctStrikes = totalStrikes / numPitches)
takenPitches |> print(n = 128)

#Exercise 4
fit = glm(outcome == "called_strike" ~ count, data = takenPitches, family = binomial)
fit

#Exercise 5
fit = glm(outcome == "called_strike" ~ home, data = takenPitches, family = binomial)
fit
