# Notes -------------------------------------------------------------------

#

# Initialisation ----------------------------------------------------------

rm(list = ls())

seed <- 1559354162
set.seed(seed) # Reproducibility

library(HuraultMisc)
source("functions.R")

library(ggplot2)
library(rstan)
rstan_options(auto_write = TRUE) # Save compiled model
options(mc.cores = parallel::detectCores()) # Parallel computing

data_file <- "Data/PremierLeague1819.csv"

stan_code <- "Model/DC_model.stan"

run <- TRUE
res_file <- "Results/fit_mdl1.rds"
par_prior <- readRDS("Results/par_prior_mdl1.rds")

# MCMC options
n_chains <- 4
n_it <- 2000

# Parameters of interest
param_pop <- c("b", "home_advantage", "sigma_ability")
param_rep <- c(
  # "win_home_rep", "win_away_rep",
  # "draw_home_rep", "draw_away_rep",
  # "lose_home_rep", "lose_away_rep",
  # "goal_tot_home_rep", "goal_tot_away_rep",
  # "goal_diff_home_rep", "goal_diff_away_rep",
  "win_rep", "draw_rep", "lose_rep",
  "goal_tot_rep", "goal_diff_rep", "point_rep"
)
param_ind <- c("attack", "defence", param_rep)
param_obs <- c("home_goals_rep", "away_goals_rep")
param <- c(param_pop, param_ind, param_obs)

# Data --------------------------------------------------------------------

df0 <- read.csv(data_file) # some columns with weird names might be lost at the end but not important for the moment

# Processing
df <- df0[, c("Div", "Date", "HomeTeam", "AwayTeam", "HTHG", "HTAG", "FTHG", "FTAG", "FTR")]
df$FTR <- factor(df$FTR, levels = c("A", "D", "H"), ordered = TRUE)

# Teams
teams <- with(df, sort(unique(c(as.character(HomeTeam), as.character(AwayTeam)))))

# Associate a unique ID to each game
id <- game_id(teams)
df <- merge(df, id, by = c("HomeTeam", "AwayTeam"))

# Order by date
df$Date <- as.Date(df$Date, "%d/%m/%Y")
df <- df[order(df$Date), ]

# heatmap_results(df) # Visualisation
fstats <- football_stats(df)

# Fit Stan model ----------------------------------------------------------

format_stan_data <- function(df) {
  list(
    N_teams = length(teams),
    N_games = nrow(df),
    home_goals = df[["FTHG"]],
    away_goals = df[["FTAG"]],
    home_id = sapply(df[["HomeTeam"]], function(x) {which(x == teams)}),
    away_id = sapply(df[["AwayTeam"]], function(x) {which(x == teams)}),
    run = 1
  )
}

data_stan <- format_stan_data(df)

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
  
  # Plot estimates
  par <- extract_parameters(fit, param, param_ind, param_obs, teams, df$Game, data_stan)
  HuraultMisc::plot_prior_posterior(par, par_prior, param_pop)
  plot_abilities(par)

  # Posterior predictive checks
  PPC_football_stats(fit, "win_rep", fstats, teams)
  PPC_football_stats(fit, "lose_rep", fstats, teams)
  PPC_football_stats(fit, "goal_tot_rep", fstats, teams)
  PPC_football_stats(fit, "point_rep", fstats, teams)
  PPC_football_stats(fit, "rank_rep", fstats, teams, order = TRUE)
  
  # Posterior rank
  stackhist_rank(compute_rank(fit, "rep"), teams)
}

