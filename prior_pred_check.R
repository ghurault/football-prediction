# Notes -------------------------------------------------------------------

# Need to finish fake data check

# Probably need to adjust some priors

# Initialisation ----------------------------------------------------------

rm(list = ls())

seed <- 1559354162
set.seed(seed) # Reproducibility

source("functions.R")

library(ggplot2)
library(rstan)
rstan_options(auto_write = TRUE) # Save compiled model
options(mc.cores = parallel::detectCores()) # Parallel computing

run_prior <- TRUE
run_fake <- FALSE

stan_code <- "Model/mdl1.stan"

prior_file <- "Results/prior_mdl1.rds"
fake_file <- "Results/fake_mdl1.rds"

n_chains <- 4
n_it <- 2000

param_pop <- c("b", "home_advantage", "sigma_ability")
param_ind <- c("attack", "defence")
param_obs <- c("home_goals_rep", "away_goals_rep")
param <- c(param_pop, param_ind, param_obs)

# Simulate from prior ----------------------------------------------------------

n_teams <- 10

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
  fit <- readRDS(prior_file)
}

# Prior predictive check ---------------------------------------------------------------

if (FALSE) {
  
  # shinystan::launch_shinystan(fit)
  
  # pairs(fit_prior, pars = param_pop)
  # plot(fit_prior, pars = param_pop, plotfun = "trace")
  
  plot(fit_prior, pars = c(param_pop, paste(param_ind, "[1]", sep = "")), plotfun = "hist")
  
  # Exponentiate abilities
  lapply(paste(param_ind, "[1]", sep = ""),
         function(x) {
           tmp <- extract(fit_prior, pars = x)[[1]]
           hist(exp(tmp), breaks = 40, main = paste(x, "rate"))
         })
  
  # Number of goals (same distribution regardless of home/away and games)
  goals <- extract(fit_prior, pars = c("home_goals_rep[1]"))[[1]]
  summary(goals)
  hist(goals, breaks = 40)
  hist(goals[goals < 20], breaks = 20)
  mean(goals >= 20) # proportion of games with home/away goals greater than 20
}

# Generate fake data ------------------------------------------------------

draw <- 2019

true_param_pop <- lapply(extract(fit_prior, pars = param_pop), function(x) {x[draw]})
true_param_ind <- lapply(extract(fit_prior, pars = param_ind), function(x) {x[draw, ]})

fd <- data.frame(HomeTeam = id$Home,
                 AwayTeam = id$Away,
                 HomeGoals = extract(fit_prior, pars = "home_goals_rep")[[1]][draw, ],
                 AwayGoals = extract(fit_prior, pars = "away_goals_rep")[[1]][draw, ],
                 FTR = NA)
fd$FTR[fd$HomeGoals == fd$AwayGoals] <- "D"
fd$FTR[fd$HomeGoals > fd$AwayGoals] <- "H"
fd$FTR[fd$HomeGoals < fd$AwayGoals] <- "A"
fd$FTR <- factor(fd$FTR, levels = c("A", "D", "H"), ordered = TRUE)

heatmap_results(fd)

# Fit fake data -----------------------------------------------------------

data_fake <- list(
  N_teams = n_teams,
  N_games = n_teams * (n_teams - 1),
  home_goals = fd$HomeGoals,
  away_goals = fd$AwayGoals,
  home_id = fd$HomeTeam,
  away_id = fd$AwayTeam,
  run = 1
)

if (run_fake) {
  fit_fake <- stan(file = stan_code, data = data_fake, pars = param,
                    iter = n_it, chains = n_chains, seed = seed)
  saveRDS(fit_fake, file = fake_file)
} else {
  fit <- readRDS(fake_file)
}

# Fake data check -------------------------------------------------------------


