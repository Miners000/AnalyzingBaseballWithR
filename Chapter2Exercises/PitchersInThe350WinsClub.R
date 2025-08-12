library(abdwr3edata)
library(tidyverse)

W = c(373, 354, 365, 417, 355, 373, 362, 363, 511)
L = c(208, 184, 310, 279, 227, 188, 208, 245, 315)
Name = c("Pete Alexander", "Roger Clemens", "Pud Galvin", "Walter Johnson", "Greg Maddux", "Christy Mathewson", "Kid Nichols", "Warren Spahn", "Cy Young")
winpct = W / (W + L) * 100
wins350 = tibble(Name, W, L, winpct)
wins350 |> arrange(desc(winpct))

SO = c(2198, 4672, 1807, 3509, 3371, 2507, 1881, 2583, 2803)
BB = c(951, 1580, 745, 1363, 999, 848, 1272, 1434, 1217)
KBBRatio = SO / BB
KBB = tibble(Name, SO, BB, KBBRatio)
KBB |> filter(KBBRatio >= 2.8) |> arrange(desc(KBBRatio))
