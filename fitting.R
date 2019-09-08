# Notes -------------------------------------------------------------------

# Do prior predictive checks
# Do Fake data checks
# Add test to model

# Initialisation ----------------------------------------------------------

rm(list = ls())

seed <- 1559354162
set.seed(seed) # Reproducibility

library(ggplot2)
library(rstan)
rstan_options(auto_write = TRUE) # Save compiled model
options(mc.cores = parallel::detectCores()) # Parallel computing

data_file <- "Data/PremierLeague1819.csv"

# Data --------------------------------------------------------------------

df0 <- read.csv(data_file) # some columns with weird names might be lost at the end but not important for the moment

# Processing
df <- df0[, c("Div", "Date", "HomeTeam", "AwayTeam", "HTHG", "HTAG", "FTHG", "FTAG", "FTR")]
df$FTR <- factor(df$FTR, levels = c("A", "D", "H"), ordered = TRUE)
df$Date <- as.Date(df$Date, "%d/%m/%Y")
df <- df[order(df$Date), ]

teams <- with(df, sort(unique(c(as.character(HomeTeam), as.character(AwayTeam)))))

# Visualisation -----------------------------------------------------------

# Heatmap results
ggplot(data = df, aes(x = AwayTeam, y = HomeTeam, fill = FTR)) +
  geom_raster() +
  scale_fill_manual(values = c("#fc8d59", "#ffffbf", "#91bfdb")) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 90))

# Fit Stan model ----------------------------------------------------------

run <- FALSE

n_chains <- 4
n_it <- 2000

stan_code <- "Model/mdl1.stan"
res_file <- "Results/fit_mdl1.rds"

param_pop <- c("b", "home_advantage", "sigma_ability")
param_ind <- c("attack", "defence")
param_obs <- c("home_goals_rep", "away_goals_rep")
param <- c("param_pop", "param_ind", "param_obs")

data_stan <- list(
  N_teams = length(teams),
  N_games = nrow(df),
  home_goals = df[["FTHG"]],
  away_goals = df[["FTAG"]],
  home_id = sapply(df[["HomeTeam"]], function(x) {which(x == teams)}),
  away_id = sapply(df[["AwayTeam"]], function(x) {which(x == teams)}),
  run = 0
)

if (run) {
  fit <- stan(file = stan_code, data = data_stan, pars = param,
              iter = n_it, chains = n_chains, seed = seed)
  saveRDS(fit, file = res_file)
} else {
  fit <- readRDS(res_file)
}

# Check fit ---------------------------------------------------------------

if (FALSE) {
  
  # shinystan::launch_shinystan(fit)
  
  pairs(fit, pars = param_pop)
  plot(fit, pars = param_pop, plotfun = "trace")
  plot(fit, pars = param_pop)
  plot(fit, pars = param_ind)
  
  # Posterior predictive checks
}

