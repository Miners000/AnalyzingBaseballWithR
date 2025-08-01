library(abdwr3edata)
library(tidyverse)

SB = c(1406, 938, 896, 808, 741, 738, 689, 580, 514, 509, 506, 504, 474)
CS = c(335, 307, 178, 146, 173, 92, 162, 148, 141, 117, 136, 131, 114)
G = c(3081, 2616, 3035, 2502, 2826, 2476, 2649, 2573, 2986, 2653, 2601, 2683, 2379)
Name = c("Rickey Henderson", "Lou Brock", "Ty Cobb", "Tim Raines", "Eddie Collins", "Max Carey", "Joe Morgan", "Ozzie Smith", "Barry Bonds", "Ichiro Suzuki", "Luis Aparicio", "Paul Molitor", "Roberto Alomar")

SBAttempts = SB + CS
SuccessRate = SB / SBAttempts
SBPG = SB / G
AllStats = tibble(Name = Name, SuccessRate = SuccessRate, SBPG = SBPG)
ggplot(AllStats, aes(x = SBPG, y = SuccessRate, label = Name)) + geom_point() + geom_text(vjust = -0.5)