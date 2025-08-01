library(abdwr3edata)
library(tidyverse)
library(Lahman)

careerPitching = Pitching |>
  group_by(playerID) |>
  summarize(
    SO = sum(SO, na.rm = TRUE),
    BB = sum(BB, na.rm = TRUE),
    IPouts = sum(IPouts, na.rm = TRUE),
    medYear = median(yearID, na.rm = TRUE)
  )

inner_join(Pitching, careerPitching)
career10000IPouts = careerPitching |> filter(IPouts >= 10000)
ggplot(career10000IPouts, aes(x = medYear, y = SO/BB, label = playerID)) + geom_point() + geom_text(vjust = -0.5, size = 2)