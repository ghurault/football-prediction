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

run_prior <- FALSE
run_fake <- FALSE
run_pred <- FALSE

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
  
  #
  #
  #
  #
  #
  # Evaluate prediction
  # Ongoing work
  #
  #
  #
  #
  #
  
  process_predictions <- function(fit, id) {
    # Compute predicted probability for goals scored and game results
    #
    # Args:
    # fit: stanfit object
    # id: Dataframe of game id
    #
    # Returns:
    # Dataframe in tall format
    
    home_goals <- rstan::extract(fit, pars = "home_goals_test")[[1]]
    away_goals <- rstan::extract(fit, pars = "away_goals_test")[[1]]
    
    # FTR
    pred1 <- id
    pred1$H <- apply(home_goals - away_goals, 2, function(x) {mean(x > 0)})
    pred1$A <- apply(home_goals - away_goals, 2, function(x) {mean(x < 0)})
    pred1$D <- apply(home_goals - away_goals, 2, function(x) {mean(x == 0)})
    pred1 <- reshape2::melt(pred1, id.vars = c("Game", "HomeTeam", "AwayTeam"), variable.name = "Value", value.name = "Probability")
    pred1$Variable <- "FTR"
    
    # Goals
    pred_goals <- function(goals, lbl) {
      # Compute dataframe for predicted number of goals
      n_post <- nrow(goals) # Number of samples
      out <- do.call(rbind,
                     lapply(1:nrow(id),
                            function(i) {
                              tmp <- table(goals[, i]) / n_post
                              tmp <- as.data.frame(tmp)
                              colnames(tmp) <- c("Value", "Probability")
                              cbind(id[i, ], tmp)
                            }))
      out$Variable <- lbl
      return(out)
    }
    pred2 <- pred_goals(home_goals, "FTHG")
    pred3 <- pred_goals(away_goals, "FTAG")
    
    rbind(pred1, pred2, pred3)
  }
  
  prepare_predictions <- function(pred, act, test_game, var = "FTR") {
    # Select games in testing set,
    # and compute forecast and actual dataframe/matrices for variable var.
    # This function is an intermediate step for computing metrics.
    #
    # Args:
    # pred: prediction dataframe
    # act: actual (observed outcome) dataframe
    # test_game: vector of test game ID
    # var: character corresponding to the variable to consider: FTR, FTHG or FTAG
    #
    # Returns:
    # List containing Forecast (prediction) and Actual dataframe
    
    if (!(var %in% c("FTR", "FTHG", "FTAG"))) {
      stop("var should be either `FTR`, `FTHG` or `FTAG`.")
    }
    
    # Select game
    pred <- pred[(pred$Game %in% test_game) & (pred$Variable == var), ]
    act <- act[act$Game %in% test_game, ]
    
    if (var == "FTR") {
      # Order FTR outcomes
      pred$Value <- factor(pred$Value, levels = c("A", "D", "H"))
    } else {
      # Convert number of goals to numeric
      pred$Value <- as.numeric(as.character(pred$Value))
      max_pred_goal <- max(pred$Value)
      # Pad with 0 if some goal values are missing between 1:max_pred_goal so correct 0 padding later 
      pred <- rbind(pred,
                    do.call(rbind,
                            lapply(setdiff(1:max_pred_goal, unique(pred$Value)),
                                   function(i) {
                                     tmp <- pred[1, ]
                                     tmp[, c("Value", "Probability")] <- c(i, 0)
                                     return(tmp)
                                   })))
    }
    
    # Reshape dataframe
    pred <- reshape2::dcast(pred, Game + HomeTeam + AwayTeam ~ Value, value.var = "Probability")
    if (var != "FTR") {
      # Pad with 0 probabilities
      pred[is.na(pred)] <- 0
      # Add extra 0 columns if max predicted goals below max observed goals
      max_obs_goal <- max(act[, var])
      if (max_pred_goal < max_obs_goal) {
        for (i in (max_pred_goal + 1):max_obs_goal) {
          pred[, as.character(i)] <- rep(0, nrow(pred))
        }
      }
    }
    
    # Generate similar matrix for actual outcomes
    act <- merge(act[, c("Game", "HomeTeam", "AwayTeam", var)],
                 pred,
                 by = c("Game", "HomeTeam", "AwayTeam"))
    outcome_id <-  !(colnames(act) %in%  c("Game", "HomeTeam", "AwayTeam", var))
    act[, outcome_id] <- 0
    for (i in 1:nrow(act)) {
      act[i, as.character(act[i, var])] <- 1
    }
    act[[var]] <- NULL
    
    # Reorder
    pred <- pred[with(pred, order(Game, HomeTeam, AwayTeam)), ]
    act <- act[with(act, order(Game, HomeTeam, AwayTeam)), ]
    
    return(list(Forecast = pred, Actual = act))
  }
  
  compute_metrics <- function(pred, act, test_game, var) {
    # Compute metrics
    #
    # Args:
    # pred: prediction dataframe
    # act: actual (observed outcome) dataframe
    # test_game: vector of test game ID
    # var: character corresponding to the variable to consider: FTR, FTHG or FTAG
    #
    # Returns:
    # Dataframe of metrics
    
    if (var == "FTG") {
      # Combine FTHG and FTAG
      l1 <- prepare_predictions(pred, act, test_game, "FTHG")
      l2 <- prepare_predictions(pred, act, test_game, "FTAG")
      f1 <- l1$Forecast
      a1 <- l1$Actual
      f2 <- l2$Forecast
      a2 <- l2$Actual
      
      mg1 <- ncol(f1) - 3 - 1 # max goal for 1
      mg2 <- ncol(f2) - 3 - 1 # max goal for 2
      if (mg1 > mg2) {
        # Pad f2 and a2 with 0
        f2[, as.character(mg2 + 1):mg1] <- 0
        a2[, as.character(mg2 + 1):mg1] <- 0
      } else if (mg2 > mg1) {
        # Pad f1 and a1 with 0
        f1[, as.character((mg1 + 1):mg2)] <- 0
        a1[, as.character((mg1 + 1):mg2)] <- 0
      }
      f <- rbind(f1, f2)
      a <- rbind(a1, a2)
    } else {
      l <- prepare_predictions(pred, act, test_game, var)
      f <- l$Forecast
      a <- l$Actual
    }
    
    col_id <- (colnames(f) %in% c("Game", "HomeTeam", "AwayTeam"))
    Forecast <- as.matrix(f[, !col_id])
    Actual <- as.matrix(a[, !col_id])
    CumForecast <- t(apply(Forecast, 1, cumsum))
    CumActual <- t(apply(Actual, 1, cumsum))
    
    K <- ncol(Forecast) # somewhat arbitrary for goals
    d <- min(Forecast[Forecast != 0]) / 100 # to avoid log(0)
    
    RPS <- apply((CumActual - CumForecast)^2, 1, sum) / (K - 1)
    CumLogLoss <- -apply(CumActual * log(pmax(CumForecast, d)), 1, sum) / (K - 1)
    
    BrierScore <- apply((Actual - Forecast)^2, 1, mean) # Between 0 and 2 (regardless of the number of categories)
    LogLoss <- -apply(Actual * log(Forecast + d), 1, mean)
    
    # Return metric per prediction (need to include additional info for FTG then) or average?
    data.frame(Metric = c("RPS", "CumLogLoss", "BrierScore", "LogLoss"),
               Value = sapply(list(RPS, CumLogLoss, BrierScore, LogLoss), mean),
               Variable = var)
    # cbind(f[, col_id],
    #       data.frame(RPS, CumLogLoss, BrierScore, LogLoss))
  }
  
  plot_lift <- function(prep_pred, best_bet = FALSE) {
    # Plot lift curve
    #
    # Args:
    # prep_pred: List containing Forecast and Actual dataframe (output from prepare_predictions)
    # best_bet: whether to compute the lift for the best bet (regardless of whether it's A, D or H; 0, 1, 2, ...)
    #
    # Returns:
    # Ggplot
    
    library(ggplot2)
    id_lbl <- c("Game", "HomeTeam", "AwayTeam")
    val <- setdiff(colnames(prep_pred$Forecast), id_lbl)
    
    compute_lift <- function(f, a, p0) {
      # Compute lift
      #
      # Args:
      # f: dataframe of forecast
      # a: dataframe of actual
      # p0: base rate
      #
      # Returns:
      # Dataframe of lift
      
      tmp <- merge(f, a, by = id_lbl)
      tmp <- tmp[order(tmp$Forecast, decreasing = TRUE), ]
      
      PropBet <- (1:nrow(tmp)) / nrow(tmp)
      PropWin <- cumsum(tmp$Actual) / (1:nrow(tmp))
      Lift <- PropWin / p0
      
      data.frame(Game = tmp$Game, PropBet, PropWin, Lift)
    }
    
    if (!best_bet) {
      
      if (length(val) < 9) {
        palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
      } else {
        getPalette <- colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Set1"))) # to extend colour palette
        palette <- getPalette(length(val))
      }
      
      lift <- do.call(rbind,
                      lapply(val,
                             function(x) {
                               f <- prep_pred$Forecast[, c(id_lbl, x)]
                               f <- change_colnames(f, x, "Forecast")
                               a <- prep_pred$Actual[, c(id_lbl, x)]
                               a <- change_colnames(a, x, "Actual")
                               
                               tmp <- compute_lift(f, a, p0 = mean(a$Actual))
                               tmp$Value <- x
                               return(tmp)
                             }))
      lift$Value <- factor(lift$Value, levels = val)
      
      p <- ggplot(data = lift, aes(x = PropBet, y = Lift, colour = Value)) +
        scale_colour_manual(values = palette)
      
    } else {
      id <- apply(prep_pred$Forecast[, val], 1, which.max) # Bet on highest probability outcome
      f <- prep_pred$Forecast[, id_lbl]
      Value <- val[id]
      f$Forecast <- NA
      a <- prep_pred$Actual[, id_lbl]
      a$Actual <- NA
      
      for (i in 1:nrow(f)) {
        f$Forecast[i] <- prep_pred$Forecast[i, Value[i]]
        a$Actual[i] <- prep_pred$Actual[i, Value[i]]
      }
      
      lift <- compute_lift(f, a, p0 = 1 / length(val)) # p0 might be subject to discussion here
      lift$Value <- Value
      
      p <- ggplot(data = lift, aes(x = PropBet, y = Lift))
    }
    
    p +
      geom_line() +
      geom_hline(yintercept = 1) +
      labs(colour = "") +
      theme_bw(base_size = 15)
    
  }
  
  plot_calibration <- function(prep_pred, CI = NULL, pool = FALSE) {
    # Plot calibration
    #
    # Args:
    # prep_pred: List containing Forecast and Actual dataframe (output from prepare_predictions)
    # CI: confidence level in %. If NULL, confidence intervals are not computed.
    # pool: whether to pool/combine the values for the plot
    #
    # Returns:
    # Ggplot
    
    library(ggplot2)
    id_lbl <- c("Game", "HomeTeam", "AwayTeam")
    val <- setdiff(colnames(prep_pred$Forecast), id_lbl)
    
    if (!pool) {
      cal <- do.call(rbind,
                     lapply(val,
                            function(x) {
                              tmp <- HuraultMisc::compute_calibration(prep_pred$Forecast[, x],
                                                                      prep_pred$Actual[, x],
                                                                      method = "smoothing",
                                                                      CI = CI)
                              tmp$Value <- x
                              return(tmp)
                            }))
      
      if (length(val) < 8) {
        palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
      } else {
        getPalette <- colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Set1"))) # to extend colour palette
        palette <- getPalette(length(val))
      }
      
      p <- ggplot(data = cal, aes(x = Forecast, y = Frequency, colour = Value)) +
        scale_colour_manual(values = palette)
      
    } else {
      
      f <- prep_pred$Forecast[, val]
      a <- prep_pred$Actual[, val]
      cal <- HuraultMisc::compute_calibration(c(as.matrix(f)),
                                              c(as.matrix(a)),
                                              method = "smoothing",
                                              CI = CI)
      
      p <- ggplot(data = cal, aes(x = Forecast, y = Frequency))
      
    }
    
    p <- p +
      geom_line() +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
      labs(colour = "", fill = "") +
      theme_bw(base_size = 15)
    
    if (!is.null(CI)) {
      if (!pool) {
        p <- p +
          geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Value), alpha = 0.5) +
          scale_fill_manual(values = palette)
      } else {
        p <- p +
          geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.5)
      }
    }
    return(p)
    
  }
  
  pred0 <- process_predictions(fit_pred, id)
  l1 <- prepare_predictions(pred = pred0, act = fd, test_game = setdiff(fd$Game, fd_train$Game), var = "FTR")
  l2 <- prepare_predictions(pred = pred0, act = fd, test_game = setdiff(fd$Game, fd_train$Game), var = "FTHG")
  m <- compute_metrics(pred = pred0, act = fd, test_game = setdiff(fd$Game, fd_train$Game), var = "FTR")
  
  plot_lift(l1) + theme(legend.position = "top")
  plot_calibration(l1, CI = NULL)
  plot_calibration(l1, CI = 0.95, pool = TRUE)
  # plot_lift(l2, best_bet = TRUE)
  # plot_calibration(l2, CI = NULL)
  
  # TO DO
  # option to compute FTG in plot_lift and plot_calibration
  
  # problem with loess in plot_calibration for l2 (probably because of few outcomes)
  
  # strange that for test games, we can be sure in the prediction that the team will score N goals (e.g. 8, and be right!)
  # not problem with game id apparently
  # and teams have played enough not to overfit
  # OK, apparently it's because the saved results doesn't correspond to "test_games": would need to save or change the script
  
  
}

