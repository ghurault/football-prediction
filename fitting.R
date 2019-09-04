# Notes -------------------------------------------------------------------

#

# Initialisation ----------------------------------------------------------

rm(list = ls())

seed <- 1559354162
set.seed(seed) # Reproducibility

data_file <- "Data/PremierLeague1819.csv"

# Data --------------------------------------------------------------------

df0 <- read.csv(data_file) # sone columns with weird names might be lost at the end but not important for the moment

df <- df0[, c("Div", "Date", "HomeTeam", "AwayTeam", "HTHG", "HTAG", "FTHG", "FTAG", "FTR")]

teams <- with(df, sort(unique(c(as.character(HomeTeam), as.character(AwayTeam)))))

# Visualisation -----------------------------------------------------------

library(ggplot2)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(data = df, aes(x = AwayTeam, y = HomeTeam, fill = FTR)) +
  geom_raster() +
  scale_fill_manual(values = cbbPalette[c(3, 1, 2)]) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 90))



