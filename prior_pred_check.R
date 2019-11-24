# Notes -------------------------------------------------------------------

# 

# Initialisation ----------------------------------------------------------

rm(list = ls())

set.seed(1559354162) # Reproducibility (different seed use in Stan)

library(HuraultMisc)
source("functions.R")

library(ggplot2)
library(rstan)
rstan_options(auto_write = TRUE) # Save compiled model
options(mc.cores = parallel::detectCores()) # Parallel computing

run_prior <- TRUE
run_fake <- TRUE
run_pred <- TRUE

stan_code <- "Model/mdl1.stan"

prior_file <- "Results/prior_mdl1.rds"
fake_file <- "Results/fake_mdl1.rds"
pred_file <- "Results/fake_pred_mdl1.rds"

# Data parameters
n_teams <- 20
teams <- LETTERS[1:n_teams]
id <- game_id(teams)

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
param_test <- c(
  # "win_home_test", "win_away_test",
  # "draw_home_test", "draw_away_test",
  # "lose_home_test", "lose_away_test",
  # "goal_tot_home_test", "goal_tot_away_test",
  # "goal_diff_home_rep", "goal_diff_away_test",
  "win_test", "draw_test", "lose_test",
  "goal_tot_test", "goal_diff_test", "point_test"
)
param_ind <- c("attack", "defence", param_rep)
param_obs <- c("home_goals_rep", "away_goals_rep")
param <- c(param_pop, param_ind, param_obs) # "home_goals_test", "away_goals_test"

# Simulate from prior ----------------------------------------------------------

data_prior <- list(
  N_teams = n_teams,
  N_games = n_teams * (n_teams - 1),
  home_goals = rep(1, n_teams * (n_teams - 1)), # doesn't matter
  away_goals = rep(1, n_teams * (n_teams - 1)), # doesn't matter
  home_id = sapply(id[["HomeTeam"]], function(x) {which(x == teams)}),
  away_id = sapply(id[["AwayTeam"]], function(x) {which(x == teams)}),
  run = 0
)

if (run_prior) {
  fit_prior <- stan(file = stan_code, data = data_prior, pars = param,
                    iter = n_it, chains = n_chains)
  saveRDS(fit_prior, file = prior_file)
} else {
  fit_prior <- readRDS(prior_file)
}

# shinystan::launch_shinystan(fit_prior)
par_prior <- extract_parameters(fit_prior, param, param_ind, param_obs, teams, id$Game, data_stan)
# pairs(fit_prior, pars = param_pop)
# plot(fit_prior, pars = param_pop, plotfun = "trace")
# saveRDS(par_prior, file = "Results/par_prior_mdl1.rds")

# Prior predictive check ---------------------------------------------------------------

if (FALSE) {
  
  plot(fit_prior, pars = c(param_pop, paste(param_ind[1:2], "[1]", sep = "")), plotfun = "hist")
  
  # Exponentiate abilities
  lapply(paste(param_ind[1:2], "[1]", sep = ""),
         function(x) {
           tmp <- extract(fit_prior, pars = x)[[1]]
           hist(exp(tmp), breaks = 40, main = paste(x, "rate"))
         })
  
  # Number of goals (same distribution regardless of home/away and games)
  goals <- extract(fit_prior, pars = c("home_goals_rep[1]"))[[1]]
  summary(goals)
  hist(goals, breaks = 40)
  hist(goals[goals < 20], breaks = 20)
  quantile(goals, probs = c(.25, .5 , .75, .9, .99, .999))
  mean(goals >= 20) # proportion of games with home/away goals greater than 20
  
  # Probability of wins/draws/lose
  lapply(c("win", "lose", "draw"),
         function(x) {
           otc <- extract(fit_prior, pars = paste0(x, "_rep[1]"))[[1]]
           otc <- table(otc) / length(otc)
           ggplot(data = data.frame(otc), aes(x = otc, y = Freq)) +
             geom_bar(stat = "identity") +
             scale_x_discrete(breaks = 1:(2 * (n_teams - 1))) +
             labs(x = paste0("Number of ", x), y = "Prior probability") +
             theme_bw(base_size = 15)
         })
  
  # Ranks
  rk <- compute_rank(fit_prior, "rep")[, 1]
  rk <- table(rk) / length(rk)
  ggplot(data =  data.frame(rk), aes(x = rk, y = Freq)) +
    geom_bar(stat = "identity") +
    scale_x_discrete(breaks = 1:n_teams) +
    labs(x = "Rank", y = "Prior probability") +
    theme_bw(base_size = 15)
  
}

# Generate fake data ------------------------------------------------------

draw <- 2019 #

# True parameters
true_param_pop <- lapply(extract(fit_prior, pars = param_pop), function(x) {x[draw]})
true_param_ind <- lapply(extract(fit_prior, pars = param_ind), function(x) {x[draw, ]})
true_param <- rbind(
  do.call(rbind,
          lapply(1:length(true_param_ind),
                 function(i) {
                   data.frame(Variable = names(true_param_ind)[i],
                              True = true_param_ind[[i]],
                              Team = teams)
                 })),
  do.call(rbind,
          lapply(1:length(true_param_pop),
                 function(i) {
                   data.frame(Variable = names(true_param_pop)[i],
                              True = true_param_pop[[i]],
                              Team = NA)
                 }))
)

fd <- cbind(id,
            data.frame(FTHG = extract(fit_prior, pars = "home_goals_rep")[[1]][draw, ],
                       FTAG = extract(fit_prior, pars = "away_goals_rep")[[1]][draw, ],
                       FTR = NA))
fd$FTR[fd$FTHG == fd$FTAG] <- "D"
fd$FTR[fd$FTHG > fd$FTAG] <- "H"
fd$FTR[fd$FTHG < fd$FTAG] <- "A"
fd$FTR <- factor(fd$FTR, levels = c("A", "D", "H"), ordered = TRUE)

heatmap_results(fd)

fstats <- football_stats(fd)

# Fit fake data -----------------------------------------------------------

data_fake <- list(
  N_teams = n_teams,
  N_games = n_teams * (n_teams - 1),
  home_goals = fd$FTHG,
  away_goals = fd$FTAG,
  home_id = sapply(fd[["HomeTeam"]], function(x) {which(x == teams)}),
  away_id = sapply(fd[["AwayTeam"]], function(x) {which(x == teams)}),
  run = 1
)

if (run_fake) {
  fit_fake <- stan(file = stan_code, data = data_fake, pars = param,
                   iter = n_it, chains = n_chains)
  saveRDS(fit_fake, file = fake_file)
} else {
  fit_fake <- readRDS(fake_file)
}

# Fake data check -------------------------------------------------------------

if (FALSE) {
  
  # shinystan::launch_shinystan(fit_fake)
  
  check_hmc_diagnostics(fit_fake)
  pairs(fit_fake, pars = param_pop)
  plot(fit_fake, pars = param_pop, plotfun = "trace")
  
  print(fit_fake, pars = param_pop)
  par_fake <- extract_parameters(fit_fake, param, param_ind, param_obs, teams, fd$Game, data_stan)
  
  # Compare prior to posterior
  HuraultMisc::plot_prior_posterior(par_fake, par_prior, param_pop)
  
  # Can we retrieve parameters?
  check_estimates(par_fake, true_param, param_pop, param_ind[1:2])
  # Coverage of team parameters
  HuraultMisc::plot_coverage(do.call(cbind, extract(fit_fake, pars = c("attack", "defence"))),
                             do.call(c, true_param_ind[c("attack", "defence")]))
  
  # Posterior predictive checks
  PPC_football_stats(fit_fake, "win_rep", fstats, teams)
  PPC_football_stats(fit_fake, "lose_rep", fstats, teams)
  PPC_football_stats(fit_fake, "goal_tot_rep", fstats, teams)
  PPC_football_stats(fit_fake, "point_rep", fstats, teams)
  PPC_football_stats(fit_fake, "rank_rep", fstats, teams, order = TRUE)
  
  # Posterior rank
  stackhist_rank(compute_rank(fit_fake, "rep"), teams)
  
}

# Fit fake data to test predictions ---------------------------------------

fd_train <- fd[which(rbinom(nrow(fd), 1, 0.7) == 1), ]

data_pred <- list(
  N_teams = n_teams,
  N_games = nrow(fd_train),
  home_goals = fd_train$FTHG,
  away_goals = fd_train$FTAG,
  home_id = sapply(fd_train[["HomeTeam"]], function(x) {which(x == teams)}),
  away_id = sapply(fd_train[["AwayTeam"]], function(x) {which(x == teams)}),
  run = 1
)

param_ind <- c("attack", "defence", param_test)
param_obs <- c()
param <- c(param_pop, param_ind, param_obs, "home_goals_test", "away_goals_test")

if (run_pred) {
  fit_pred <- stan(file = stan_code, data = data_pred, pars = param,
                   iter = n_it, chains = n_chains)
  saveRDS(fit_pred, file = pred_file)
} else {
  fit_pred <- readRDS(pred_file)
}

# Check model and evaluate predictions ------------------------------------

if (FALSE) {
  
  # shinystan::launch_shinystan(fit_pred)
  
  check_hmc_diagnostics(fit_pred)
  pairs(fit_pred, pars = param_pop)
  plot(fit_pred, pars = param_pop, plotfun = "trace")
  
  print(fit_pred, pars = param_pop)
  par_pred <- extract_parameters(fit_pred, param, param_ind, param_obs, teams, fd_train$Game, data_stan)
  
  # Compare prior to posterior
  HuraultMisc::plot_prior_posterior(par_pred, par_prior, param_pop)
  
  # Statistics predictions
  PPC_football_stats(fit_pred, "win_test", fstats, teams)
  PPC_football_stats(fit_pred, "lose_test", fstats, teams)
  PPC_football_stats(fit_pred, "point_test", fstats, teams)
  stackhist_rank(compute_rank(fit_pred, "test"), teams)
  
  pred0 <- process_predictions(fit_pred, id)
  
  # Evaluate FTR predictions
  m1 <- compute_metrics(pred = pred0, act = fd, test_game = setdiff(fd$Game, fd_train$Game), var = "FTR")
  l1 <- prepare_predictions(pred = pred0, act = fd, test_game = setdiff(fd$Game, fd_train$Game), var = "FTR")
  plot_lift(l1) + theme(legend.position = "top")
  plot_calibration(l1, CI = NULL)
  plot_calibration(l1, CI = 0.95, pool = TRUE)
  
  # Evaluate goals predictions
  l2 <- prepare_predictions(pred = pred0, act = fd, test_game = setdiff(fd$Game, fd_train$Game), var = "FTHG")
  # plot_lift(l2, best_bet = TRUE)
  # plot_calibration(l2, CI = NULL) # problem with loess in plot_calibration for l2 (probably because of few outcomes)
  
}
