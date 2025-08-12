library(abdwr3edata)
library(tidyverse)

hofPitching = hof_pitching |> mutate(
  BFgroup = cut(BF, c(0, 10000, 15000, 20000, 30000),
                labels = c("Less than 10 Thousand", "10 to 15 Thousand", "15 to 20 Thousand", "More Than 20 Thousand"))
)

pitchingByBF = hofPitching |> group_by(BFgroup) |> summarize(N= n())
ggplot(hofPitching, aes(BFgroup)) + geom_bar() + ylab("Number of Pitchers") + xlab("Batters Faced")
ggplot(pitchingByBF, aes(BFgroup, N)) + geom_point(color = "red") + coord_flip() + xlab("Batters Faced") +
  ylab("Number of Pitchers")

ggplot(hofPitching, aes(x = WAR)) + geom_histogram(
  color = "white", fill = "orange")
over125War = hofPitching |> filter(WAR > 125) |> select(2, WAR)
over125War

hofPitching = hofPitching |> mutate(WarPerSeason = WAR / Yrs)
ggplot(hofPitching, aes(BFgroup, WarPerSeason)) + geom_point() + coord_flip()
ggplot(hofPitching, aes(BFgroup, WarPerSeason)) + geom_boxplot() + coord_flip()

hofPitching = hofPitching |> mutate(midYear = (From + To) / 2)
hofPitchingRecent = hofPitching |> filter(midYear > 1960) |> arrange(desc(WarPerSeason))
ggplot(hofPitching.recent, aes(WarPerSeason)) + geom_dotplot()
recentStandouts = hofPitchingRecent |> filter(WarPerSeason > 4.25) |> select(2, WarPerSeason)
recentStandouts

hofPitchingOld = hofPitching |> filter(midYear < 1901 & WarPerSeason < 2) |> arrange(WarPerSeason) |> select(2, WarPerSeason, midYear)
hofPitchingOld |> slice(1:2)
ggplot(hofPitchingRecent, aes(midYear, WarPerSeason)) + geom_point() +
  geom_smooth() + geom_text(data = hofPitchingOld, aes(midYear, WarPerSeason, label = ...2,), size = 2, hjust = -0.01)

