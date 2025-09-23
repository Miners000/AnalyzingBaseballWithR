library(abdwr3edata)
library(tidyverse)
library(Lahman)
library(broom)
library(ggrepel)
library(ggplot2)

#Get Mickey Mantle's ID and Stats
mantleID = People |> filter(nameFirst == 'Mickey', nameLast == "Mantle") |> pull(playerID)
batting = Batting |> replace_na(list(SF = 0, HBP = 0))

getStats = function(player_id) {
  batting |>
    filter(playerID == player_id) |>
    inner_join(People, by = "playerID") |>
    mutate(
      birthyear = if_else(birthMonth >= 7, birthYear + 1, birthYear),
      Age = yearID - birthyear,
      SLG = (H - X2B - X3B - HR + 2 * X2B + 3 * X3B + 4 * HR) / AB,
      OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
      OPS = SLG + OBP
    ) |>
    select(Age, SLG, OBP, OPS)
}

Mantle = getStats(mantleID)
ggplot(Mantle, aes(Age, OPS)) + geom_point()

#Plot a fitting cruve for OPS
fitModel = function(data)
{
  fit = lm(OPS ~ I(Age - 30) + I((Age - 30)^2), data = data)
  b = coef(fit)
  ageMax = 30 - b[2] / b[3] / 2
  Max = b[1] - b[2] ^ 2 / b[3] / 4
  list(fit = fit, ageMax = ageMax, Max = Max)
}

F2 = fitModel(Mantle)
F2 |> pluck("fit") |> coef()
c(F2$ageMax, F2$Max)


ggplot(Mantle, aes(Age, OPS)) + geom_point() + geom_smooth(method = "lm", se = FALSE, linewidth = 1.5, formula = y ~ poly(x, 2, raw = TRUE)) + 
  geom_vline(xintercept = F2$ageMax, linetype = "dashed", color = "red") + geom_hline(yintercept = F2$Max, linetype = "dashed", color = "red") + 
  annotate(geom = "text", x = c(29, 20), y = c(0.72, 1.1), label = c("Peak Age", "Max"), size = 5, color = "red")
ggsave("MantleOPSBestFitCurve.png")
F2 |> pluck("fit") |> summary()

#Career Trajectories by Position
batting2000 = batting |> group_by(playerID) |> summarize(abCareer = sum(AB, na.rm = TRUE)) |> inner_join(batting, by = "playerID") |> filter(abCareer >= 2000)
positions = Fielding |> group_by(playerID, POS) |> summarize(games = sum(G)) |> arrange(playerID, desc(games)) |> filter(POS == first(POS))
batting2000 = batting2000 |> inner_join(positions, by = "playerID")

vars = c("G", "AB", "R", "H", "X2B", "X3B", "HR", "RBI", "BB", "SO", "SB")
cTotals = batting2000 |> group_by(playerID) |> summarize(across(all_of(vars), ~sum(.x, na.rm = TRUE)))
cTotals = cTotals |> mutate(AVG = H / AB, SLG = (H - X2B - X3B - HR + 2 * X2B + 3 * X3B + 4 * HR) / AB)
cTotals = cTotals |> inner_join(positions, by = "playerID") |> mutate(valuePOS = case_when(
  POS == "C" ~ 240, POS == "SS" ~ 168, POS == "2B" ~ 132, POS == "3B" ~ 84, POS == "OF" ~ 48, POS == "1B" ~ 12, TRUE ~ 0
))

#Get similarity scores
similar = function(p, number = 10)
{
  P = cTotals |> filter(playerID == p)
  cTotals |> mutate(simScore = 1000 - 
                      floor(abs(G - P$G) / 20) -
                      floor(abs(AB - P$AB) / 75) -
                      floor(abs(R - P$R) / 10) -
                      floor(abs(H - P$H) / 15) -
                      floor(abs(X2B - P$X2B) / 5) -
                      floor(abs(X3B - P$X3B) / 4) -
                      floor(abs(HR - P$HR) / 2) -
                      floor(abs(RBI - P$RBI) / 10) -
                      floor(abs(BB - P$BB) / 25) -
                      floor(abs(SO - P$SO) / 150) -
                      floor(abs(SB - P$SB) / 20) - 
                      floor(abs(AVG - P$AVG) / 0.001) - 
                      floor(abs(SLG - P$SLG) / 0.002) -
                      abs(valuePOS - P$valuePOS)) |> arrange(desc(simScore)) |> slice_head(n = number)
}

similar(mantleID, 6)

#Defining Age, SLG, OBP, and OPS
batting2000 = batting2000 |> group_by(playerID, yearID) |> summarize(
  G = sum(G), AB = sum(AB), R = sum(R),
  H = sum(H), X2B = sum(X2B), X3B = sum(X3B),
  HR = sum(HR), RBI = sum(RBI), SB = sum(SB),
  CS = sum(CS), BB = sum(BB), SH = sum(SH),
  SF = sum(SF), HBP = sum(HBP),
  abCareer = first(abCareer),
  POS = first(POS)
) |> mutate(SLG = (H - X2B - X3B - HR + 2 * X2B + 3 * X3B + 4 * HR) / AB,
            OBP = (H + BB + HBP) / (AB + BB + HBP + SF), OPS = SLG + OBP)
batting2000 = batting2000 |> inner_join(People, by = "playerID") |> mutate(
  birthyear = if_else(birthMonth >= 7, birthYear + 1, birthYear), Age = yearID - birthyear
)
batting2000 = batting2000 |> drop_na(Age)

#Fitting and Plotting Trajectories
plotTrajectories = function(player, nSimilar = 5, ncol)
{
  flnames = unlist(str_split(player, " "))
  
  player = People |> filter(nameFirst == flnames[1], nameLast == flnames[2]) |> select(playerID)
  playerList = player |> pull(playerID) |> similar(nSimilar) |> pull(playerID)
  
  newBatting = batting2000 |> filter(playerID %in% playerList) |> mutate(name = paste(nameFirst, nameLast))
  
  ggplot(newBatting, aes(Age, OPS)) + geom_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1.5) + 
    facet_wrap(vars(name), ncol = ncol) + theme_bw()
}

plotTrajectories("Mickey Mantle", 6, 2)
ggsave("MantleSimilarPlotted.png")

jeterPlot = plotTrajectories("Derek Jeter", 9, 3)
jeterPlot
ggsave("JeterSimilarPlotted.png")

dataGrouped = jeterPlot$data |> group_by(name)
playerNames = dataGrouped |> group_keys() |> pull(name)
regressions = dataGrouped |> group_split() |> map(~lm(OPS ~ I(Age - 30) + I((Age - 30) ^ 2), data = .)) |> 
  map(tidy) |> set_names(playerNames) |> bind_rows(.id = "name")
regressions |> slice_head(n = 6)
S = regressions |> group_by(name) |> summarize(b1 = estimate[1], b2 = estimate[2], curvature = estimate[3], ageMax = round(30 - b2 / curvature / 2, 1), Max = round(b1 - b2 ^ 2 / curvature / 4, 3))
ggplot(S, aes(ageMax, curvature, label = name)) + geom_point() + geom_label_repel()
ggsave("JeterSimilarMax.png")

#Patterns of Peak
notActivePlayerID = People |> filter(finalGame < "2021-11-01") |> pull(playerID)
batting2000 = batting2000 |> filter(playerID %in% notActivePlayerID)
midCareers = batting2000 |> group_by(playerID) |> summarize(midYear = (min(yearID) + max(yearID)) / 2, abTotal = first(abCareer))
batting2000 = batting2000 |> inner_join(midCareers, by = "playerID")

batting2000Grouped = batting2000 |> group_by(playerID)
ids = batting2000Grouped |> group_keys() |> pull(playerID)
models = batting2000Grouped |> group_split() |> map(~lm(OPS ~ I(Age - 30) + I((Age - 30) ^ 2), data = .)) |> map(tidy) |> set_names(ids) |> bind_rows(.id = "playerID")
betaCoefs = models |> group_by(playerID) |> summarize(A = estimate[1], B = estimate[2], C = estimate[3]) |> mutate(peakAge = 30 - B / 2 / C) |> inner_join(midCareers, by = "playerID")
agePlot = ggplot(betaCoefs, aes(midYear, peakAge)) + geom_point(alpha = 0.5) + geom_smooth(color = "red", method = 'loess') + ylim(20, 40) + xlab('Mid Career') + ylab("Peak Age")
agePlot
ggsave("PeakAgevsMidCareer.png")

agePlot + aes(x = log2(abTotal)) + xlab("Log2 of Career AB")
ggsave("PeakAgevsCareerAB.png")

batting2000Mid90 = batting2000 |> filter(midYear >= 1985, midYear <= 1995)
batting2000Mid90Grouped = batting2000Mid90 |> group_by(playerID)
ids = batting2000Mid90Grouped |> group_keys() |> pull(playerID)
models = batting2000Mid90Grouped |> group_split() |> map(~lm(OPS ~ I(Age - 30) + I((Age - 30)^2), data = .)) |> map(tidy) |> set_names(ids) |> bind_rows(.id = "playerID")
betaEsts = models |> group_by(playerID) |> summarize(A = estimate[1], B = estimate[2], C = estimate[3]) |> mutate(peakAge = 30 - B / 2 / C) |> inner_join(midCareers) |> inner_join(positions) |> rename(Position = POS)
betaFielders = betaEsts |> filter(Position %in% c("1B", "2B", "3B", "SS", "C", "OF")) |> inner_join(People)
ggplot(betaFielders, aes(Position, peakAge)) + geom_jitter(width = 0.2) + ylim(20, 40) + geom_label_repel(data = filter(betaFielders, peakAge > 37), aes(Position, peakAge, label = nameLast))
ggsave("LatePeaks1985to1995.png")

#Exercise 1
Mays = getStats("mayswi01")
ggplot(Mays, aes(Age, OPS)) + geom_point()

fitMays = fitModel(Mays)
fitMays |> pluck("fit") |> coef()
c(fitMays$ageMax, fitMays$Max)
ggplot(Mays, aes(Age, OPS)) + geom_point() + geom_smooth(method = "lm", se = FALSE, linewidth = 1.5, formula = y ~ poly(x, 2, raw = TRUE)) + 
  geom_vline(xintercept = fitMays$ageMax, linetype = "dashed", color = "red") + geom_hline(yintercept = fitMays$Max, linetype = "dashed", color = "red") + 
  annotate(geom = "text", x = c(29, 20), y = c(0.72, 1.1), label = c("Peak Age", "Max"), size = 5, color = "red")
ggsave("MaysOPSBestFitCurve.png")

#Exercise 2
maysSimilar = similar("mayswi01", 6)
maysSimilar
plotTrajectories("Willie Mays", 6, 2)
ggsave("MaysSimilarPlotted.png")

#Exercise 3
batting3200 = batting |> group_by(playerID) |> summarize(hCareer = sum(H, na.rm = TRUE)) |> inner_join(batting, by = "playerID") |> filter(hCareer >= 3200)
batting3200 = batting3200 |> group_by(playerID, yearID) |> summarize(
  G = sum(G), AB = sum(AB), R = sum(R),
  H = sum(H), X2B = sum(X2B), X3B = sum(X3B),
  HR = sum(HR), RBI = sum(RBI), SB = sum(SB),
  CS = sum(CS), BB = sum(BB), SH = sum(SH),
  SF = sum(SF), HBP = sum(HBP), hCareer = first(hCareer)
) |> mutate(AVG = H/AB)
batting3200 = batting3200 |> inner_join(People, by = "playerID") |> mutate(
  birthyear = if_else(birthMonth >= 7, birthYear + 1, birthYear), Age = yearID - birthyear, name = paste(nameFirst, nameLast)
)
batting3200 = batting3200 |> drop_na(Age)
ggplot(batting3200, aes(Age, AVG)) + geom_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1.5) + 
  facet_wrap(vars(name), ncol = 4) + theme_bw()

#Exercise 4
topHRs = batting |> group_by(playerID) |> summarize(hrCareer = sum(HR, na.rm = TRUE)) |> inner_join(batting, by = "playerID") |> filter(hrCareer >= 585)
topHRs = topHRs |> group_by(playerID, yearID) |> summarize(
  G = sum(G), AB = sum(AB), R = sum(R),
  H = sum(H), X2B = sum(X2B), X3B = sum(X3B),
  HR = sum(HR), RBI = sum(RBI), SB = sum(SB),
  CS = sum(CS), BB = sum(BB), SH = sum(SH),
  SF = sum(SF), HBP = sum(HBP), hrCareer = first(hrCareer)
) |> mutate(hrRate = HR/AB)
topHRs = topHRs |> inner_join(People, by = "playerID") |> mutate(
  birthyear = if_else(birthMonth >= 7, birthYear + 1, birthYear), Age = yearID - birthyear, name = paste(nameFirst, nameLast)
)
topHRs = topHRs |> drop_na(Age)
ggplot(topHRs, aes(Age, hrRate)) + geom_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1.5) + 
  facet_wrap(vars(name), ncol = 4) + theme_bw()

#Exercise 5
batting4045 = batting2000 |> filter(min(yearID) >= 1940, min(yearID) <= 1945)
ids = batting4045 |> group_keys() |> pull(playerID)
models = batting4045 |> group_split() |> map(~lm(OPS ~ I(Age - 30) + I((Age - 30) ^ 2), data = .)) |> map(tidy) |> set_names(ids) |> bind_rows(.id = "playerID")
betaCoefs = models |> group_by(playerID) |> summarize(A = estimate[1], B = estimate[2], C = estimate[3]) |> mutate(peakAge = 30 - B / 2 / C) |> inner_join(midCareers, by = "playerID")
agePlot4045 = ggplot(betaCoefs, aes(midYear, peakAge)) + geom_point(alpha = 0.5) + geom_smooth(color = "red", method = 'loess') + ylim(20, 40) + xlab('Mid Career') + ylab("Peak Age")
agePlot4045

batting7075 = batting2000 |> filter(min(yearID) >= 1970, min(yearID) <= 1975)
ids = batting7075 |> group_keys() |> pull(playerID)
models = batting7075 |> group_split() |> map(~lm(OPS ~ I(Age - 30) + I((Age - 30) ^ 2), data = .)) |> map(tidy) |> set_names(ids) |> bind_rows(.id = "playerID")
betaCoefs = models |> group_by(playerID) |> summarize(A = estimate[1], B = estimate[2], C = estimate[3]) |> mutate(peakAge = 30 - B / 2 / C) |> inner_join(midCareers, by = "playerID")
agePlot7075 = ggplot(betaCoefs, aes(midYear, peakAge)) + geom_point(alpha = 0.5) + geom_smooth(color = "red", method = 'loess') + ylim(20, 40) + xlab('Mid Career') + ylab("Peak Age")
agePlot7075
