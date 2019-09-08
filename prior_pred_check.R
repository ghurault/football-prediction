# Notes -------------------------------------------------------------------

# Probably need to adjust some priors

# Formalise posterior predictive checks and make appropriate functions
# Like comparing number of wins, can compare number of lose and compare number of goals
# Potentially stratify by HomeWin vs AwayWin (HomeGoals vs AwayGoals)

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

run_prior <- FALSE
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
                 HomeGoals = extract(fit_prior, pars = "home_goals_rep")[[1]][draw, ],
                 AwayGoals = extract(fit_prior, pars = "away_goals_rep")[[1]][draw, ],
                 FTR = NA)
fd$Game <- 1:nrow(fd)
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
  home_id = sapply(df[["HomeTeam"]], function(x) {which(x == teams)}),
  away_id = sapply(df[["AwayTeam"]], function(x) {which(x == teams)}),
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

if (FALSE) {
  
  # shinystan::launch_shinystan(fit)
  
  pairs(fit_prior, pars = param_pop)
  plot(fit_prior, pars = param_pop, plotfun = "trace")
  
  print(fit, pars = param_pop)
  
  ## Can we retrieve parameters?
  par_fake <- extract_parameters(fit_fake, param, param_ind, param_obs, teams, data_stan)
  tmp <- merge(subset(par_fake, Variable %in% c(param_pop, param_ind)),
               true_param,
               by = c("Variable", "Team"))
  # Population parameters
  ggplot(data = subset(tmp, Variable %in% param_pop),
         aes(x = Variable)) +
    geom_pointrange(aes(y = Mean, ymin = `5%`, ymax = `95%`)) +
    geom_point(aes(y = True), col = "#E69F00", size = 2) +
    coord_flip() +
    labs(x = "", y = "Estimate") +
    theme_bw(base_size = 20)
  # Team parameters
  lapply(param_ind,
         function(par_name) {
           
           library(ggplot2)
           
           a <- subset(tmp, Variable == par_name)
           a$Team <- factor(a$Team, levels = a$Team[order(a$True)]) # order by true value
           
           ggplot(data = a, aes(x = Team)) +
             geom_pointrange(aes(y = Mean, ymin = `5%`, ymax = `95%`)) +
             geom_point(aes(y = True), col = "#E69F00", size = 2) +
             coord_flip() +
             labs(y = par_name) +
             theme_bw(base_size = 15)
         })
  
  ## Posterior predictive checks
  
  home_goals <- extract(fit_fake, pars = "home_goals_rep")[[1]]
  away_goals <- extract(fit_fake, pars = "away_goals_rep")[[1]]
  
  # Win probabilities
  fd$HomeWinProb <- apply(home_goals - away_goals, 2, function(x) {mean(x > 0)})
  fd$AwayWinProb <- apply(home_goals - away_goals, 2, function(x) {mean(x < 0)})
  fd$DrawProb <- apply(home_goals - away_goals, 2, function(x) {mean(x == 0)})
  
  # Probability that given team win x games
  wins <- do.call(rbind,
                  lapply(teams,
                         function(teamName) {
                           tmp <- cbind(
                             (home_goals - away_goals)[, fd$Game[fd$HomeTeam == teamName]],
                             (away_goals - home_goals)[, fd$Game[fd$AwayTeam == teamName]]
                           )
                           n_wins <- apply(tmp, 1, function(x) {sum(x > 0)})
                           prob_wins <- table(factor(n_wins, levels = 0:(2 * (n_teams - 1)))) / length(n_wins)
                           data.frame(Team = teamName,
                                      NumberWins = as.numeric(names(prob_wins)),
                                      Probability = as.numeric(prob_wins))
                         }))
  # Observed number of wins
  wins$Actual <- FALSE
  act <- sapply(teams,
         function(teamName) {
           nrow(subset(fd, (HomeTeam == teamName & FTR == "H") |
                         (AwayTeam == teamName & FTR == "A")))
         })
  for (i in 1:length(act)) {
    wins$Actual[wins$Team == names(act)[i] & wins$NumberWins == as.numeric(act[i])] <- TRUE
  }
  
  # Order team by mode
  a <- sapply(teams,
         function(teamName) {
           tmp <- subset(wins, Team == teamName)
           tmp$NumberWins[which.max(tmp$Probability)]
         })
  wins$Team <- factor(wins$Team, levels = names(sort(a)))
  
  # Number of wins probability (and true value in colour)
  ggplot(data = wins,
         aes(x = NumberWins, y = Probability, fill = Actual)) +
    scale_fill_manual(values = c("#000000", "#E69F00")) +
    geom_bar(stat = "identity") +
    facet_grid(rows = vars(Team)) +
    theme_bw(base_size = 15) +
    theme(legend.position = "none")
  
  
  
}

