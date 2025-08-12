library(tidyverse)

retro1998 = read_csv(here::here("~/AnalyzingBaseballWithR/data_large/retrosheet/download.folder/unzipped/all1998.csv"))
sosaID = People |> filter(nameFirst == "Sammy", nameLast == "Sosa") |>
  pull(retroID)
mcgwireID = People |> filter(nameFirst == "Mark", nameLast == "McGwire") |>
  pull(retroID)

mcgwireData = retro1998 |> filter(bat_id %in% mcgwireID)
mcgwireData = filter(mcgwireData, bat_event_fl == TRUE)
mcgwireData = mutate(mcgwireData, PA = row_number())

sosaData = retro1998 |> filter(bat_id %in% sosaID)
sosaData = filter(sosaData, bat_event_fl == TRUE)
sosaData = mutate(sosaData, PA = row_number())

mcGwireHRPA = mcgwireData |> filter(event_cd == 23) |> pull(PA)
sosaHRPA = sosaData |> filter(event_cd == 23) |> pull(PA)

mcGwireSpacings = diff(c(0, mcGwireHRPA))
sosaSpacings = diff(c(0, sosaHRPA))

HRSpacing = tibble(
  Player = c(rep("McGwire", length(mcGwireSpacings)),
             rep("Sosa", length(sosaSpacings))),
  Spacing = c(mcGwireSpacings, sosaSpacings)
)

HRSpacing |> group_by(Player) |> summarize(meanSpacing = mean(Spacing),
                                           medSpacing = median(Spacing),
                                           HR = n())
ggplot(HRSpacing, aes(x = Spacing, fill = Player)) + 
  geom_histogram(color = "white", position = "dodge", binwidth = 5) + 
  labs(title = "Spacing Between Home Runs in 1998",
       y = "Count", x = "PAs Between HRs")
ggsave("HomeRunRace1998Spacing.png")
