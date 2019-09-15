# Notes -------------------------------------------------------------------

#

# Initialisation ----------------------------------------------------------

rm(list = ls())

seed <- 1559354162
set.seed(seed) # Reproducibility

source("functions.R")

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

# heatmap_results(df) # Visualisation

# Fit Stan model ----------------------------------------------------------

run <- TRUE

n_chains <- 4
n_it <- 2000

stan_code <- "Model/mdl1.stan"
res_file <- "Results/fit_mdl1.rds"

param_pop <- c("b", "home_advantage", "sigma_ability")
param_ind <- c("attack", "defence",
               # "win_home_rep", "win_away_rep",
               # "draw_home_rep", "draw_away_rep",
               # "lose_home_rep", "lose_away_rep",
               # "goal_home_rep", "goal_away_rep",
               # "goal_diff_home_rep", "goal_diff_away_rep",
               "win_rep", "draw_rep", "lose_rep",
               "goal_rep", "goal_diff_rep", "point_rep",
               # "win_home_test", "win_away_test",
               # "draw_home_test", "draw_away_test",
               # "lose_home_test", "lose_away_test",
               # "goal_home_test", "goal_away_test",
               # "goal_diff_home_test", "goal_diff_away_test",
               "win_test", "draw_test", "lose_test",
               "goal_test", "goal_diff_test", "point_test")
param_obs <- c("home_goals_rep", "away_goals_rep")
param <- c(param_pop, param_ind, param_obs)

get_game_id <- function(df_train, teams) {
  # Return training, testing id and id for all games
  #
  # Args:
  # df: Dataframe of games to train the model with
  # teams: vector of team names
  #
  # Returns:
  # List containing Game id for training, testing and all
  
  # All games
  id <- expand.grid(HomeTeam = 1:length(teams), AwayTeam = 1:length(teams))
  id <- id[id$HomeTeam != id$AwayTeam, ]
  
  # Games use for training
  id_train <- data.frame(HomeTeam = sapply(df_train[["HomeTeam"]], function(x) {which(x == teams)}),
                         AwayTeam = sapply(df_train[["AwayTeam"]], function(x) {which(x == teams)}))
  
  # Games remaining
  id_test <- id_test <- dplyr::anti_join(id, id_train, by = c("HomeTeam", "AwayTeam"))
  
  # Reformat id
  id <- rbind(id_train, id_test)
  
  return(list(Train = id_train, Test = id_test, All = id))
}

format_stan_data <- function(df_train, id) {
  n_team <- max(id$All)
  list(
    N_teams = n_team,
    N_games = nrow(df_train),
    home_goals = df_train[["FTHG"]],
    away_goals = df_train[["FTAG"]],
    home_id = id$Train$HomeTeam,
    away_id = id$Train$AwayTeam,
    run = 1,
    N_games_test = nrow(id$Test),
    home_id_test = id$Test$HomeTeam,
    home_id_test = id$Test$AwayTeam
  )
}

df_train <- df[1:200, ] #
id <- get_game_id(df_train, teams)
data_stan <- format_stan_data(df_train, id)

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

