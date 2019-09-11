# Notes -------------------------------------------------------------------

# Probably need to adjust some priors

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

run_prior <- TRUE
run_fake <- TRUE

stan_code <- "Model/mdl1.stan"

prior_file <- "Results/prior_mdl1.rds"
fake_file <- "Results/fake_mdl1.rds"

n_chains <- 4
n_it <- 2000

param_pop <- c("b", "home_advantage", "sigma_ability")
param_ind <- c("attack", "defence",
               # "win_home_rep", "win_away_rep",
               # "draw_home_rep", "draw_away_rep",
               # "lose_home_rep", "lose_away_rep",
               # "goal_home_rep", "goal_away_rep",
               # "goal_diff_home_rep", "goal_diff_away_rep",
               "win_rep", "draw_rep", "lose_rep",
               "goal_rep", "goal_diff_rep", "point_rep")
param_obs <- c("home_goals_rep", "away_goals_rep")
param <- c(param_pop, param_ind, param_obs)

# Simulate from prior ----------------------------------------------------------

n_teams <- 10
teams <- LETTERS[1:n_teams]

id <- expand.grid(Home = 1:n_teams, Away = 1:n_teams)
id <- id[id$Home != id$Away, ]

data_prior <- list(
  N_teams = n_teams,
  N_games = n_teams * (n_teams - 1),
  home_goals = rep(1, n_teams * (n_teams - 1)), # doesn't matter
  away_goals = rep(1, n_teams * (n_teams - 1)), # doesn't matter
  home_id = id$Home,
  away_id = id$Away,
  run = 0
)

if (run_prior) {
  fit_prior <- stan(file = stan_code, data = data_prior, pars = param,
                    iter = n_it, chains = n_chains, seed = seed)
  saveRDS(fit_prior, file = prior_file)
} else {
  fit_prior <- readRDS(prior_file)
}

# shinystan::launch_shinystan(fit_prior)
par_prior <- extract_parameters(fit_prior, param, param_ind, param_obs, teams, data_stan)
# pairs(fit_prior, pars = param_pop)
# plot(fit_prior, pars = param_pop, plotfun = "trace")

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
  
}

# Generate fake data ------------------------------------------------------

draw <- 2019 # 2019

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

fd <- data.frame(HomeTeam = teams[id$Home],
                 AwayTeam = teams[id$Away],
                 FTHG = extract(fit_prior, pars = "home_goals_rep")[[1]][draw, ],
                 FTAG = extract(fit_prior, pars = "away_goals_rep")[[1]][draw, ],
                 FTR = NA)
fd$Game <- 1:nrow(fd)
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
                   iter = n_it, chains = n_chains, seed = seed)
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
  par_fake <- extract_parameters(fit_fake, param, param_ind, param_obs, teams, data_stan)
  
  # Compare prior to posterior
  plot_prior_posterior(par_fake, par_prior, param_pop)
  
  ## Can we retrieve parameters?
  check_estimates(par_fake, true_param, param_pop, param_ind[1:2])
  
  # Posterior predictive checks
  PPC_football_stats(fit_fake, "win", fstats, teams)
  PPC_football_stats(fit_fake, "lose", fstats, teams)
  PPC_football_stats(fit_fake, "goal", fstats, teams)
  PPC_football_stats(fit_fake, "point", fstats, teams)
  PPC_football_stats(fit_fake, "rank", fstats, teams, order = TRUE)
  
  # Posterior rank
  stackhist_rank(compute_rank(fit_fake), teams)
  
  # Posterior win probability
  home_goals <- extract(fit_fake, pars = "home_goals_rep")[[1]]
  away_goals <- extract(fit_fake, pars = "away_goals_rep")[[1]]
  fd$HomeWinProb <- apply(home_goals - away_goals, 2, function(x) {mean(x > 0)})
  fd$AwayWinProb <- apply(home_goals - away_goals, 2, function(x) {mean(x < 0)})
  fd$DrawProb <- apply(home_goals - away_goals, 2, function(x) {mean(x == 0)})

}

