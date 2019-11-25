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

stan_code <- "Model/mdl1.stan"

run <- FALSE
res_file <- "Results/val_mdl1.rds"

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

# Week number (training unit)
df[["WeekNumber"]] <- strftime(df[["Date"]], format = "%Y-%V")
weeks <- unique(df[["WeekNumber"]])

# Forward chaining --------------------------------------------------------

# IN PROGRESS


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

# validation function?


if (run) {
  library(foreach)
  library(doParallel)
  
  duration <- Sys.time()
  cl <- makeCluster(n_cluster)
  registerDoParallel(cl)
  writeLines(c(""), "log.txt")
  
  compiled_model <- rstan::stan_model(stan_code)
  
  out <- foreach(w = 1:(length(weeks) - 1)) %dopar% {

    source("functions.R")
    library(rstan)
    rstan_options(auto_write = TRUE) # Save compiled model
    options(mc.cores = parallel::detectCores()) # Parallel computing
    
    sink("log.txt", append = TRUE)
    cat(paste("Starting training at week ", w, " \n", sep = ""))
    
    # validation_iteration(w, f, score_char$MCID)
    
    sub_df <- df[df$WeekNumber <= weeks[w], ]
    
    data_stan <- format_stan_data(sub_df)
    
    fit <- sampling(compiled_model, data = data_stan, pars = param,
                    iter = n_it, chains = n_chains, seed = seed)
    
    par <- extract_parameters(fit, param, param_ind, param_obs, teams, sub_df[["Game"]], data_stan)
    pred <- process_predictions(fit_pred, id)
    
    # Add weeknumber to dataframes
    # as it is, need test_games for compute metrics
    
    
    
    list(Results = pred, Parameters = par)
  }
  stopCluster(cl)
  (duration = Sys.time() - duration)
  
  saveRDS(out, file = res_file)
} else {
  out <- readRDS(res_file)
}

# Analysis ----------------------------------------------------------------

flag <- FALSE
if (flag) {
  
  # Evolution of RPS (FTR, FTG) as a function of weeks
  
  

  
  
  
  
  

  # Refit model at the mid-season
  sub_df <- df[df[["WeekNumber"]] <= median(weeks), ]
  fit <- sampling(compiled_model, data = data_stan, pars = param,
                  iter = n_it, chains = n_chains, seed = seed)
  pred <- process_predictions(fit, id)
  # Statistics predictions
  PPC_football_stats(fit_pred, "win_test", fstats, teams)
  PPC_football_stats(fit_pred, "lose_test", fstats, teams)
  PPC_football_stats(fit_pred, "point_test", fstats, teams)
  stackhist_rank(compute_rank(fit_pred, "test"), teams)
  # Calibration and lift for FTR
  l1 <- prepare_predictions(pred = pred, act = df, test_game = df[df[["WeekNumber"]] > median(weeks), "Game"], var = "FTR")
  plot_lift(l1) + theme(legend.position = "top")
  plot_calibration(l1, CI = NULL)
  plot_calibration(l1, CI = 0.95, pool = TRUE)
  # Calibration and lift for FTHG/FTAG
  l2 <- prepare_predictions(pred = pred0, act = fd, test_game = setdiff(fd$Game, fd_train$Game), var = "FTHG")
  plot_lift(l2, best_bet = TRUE)
  plot_calibration(l2, CI = NULL)

}
