# Exploration ---------------------------------------------------------

heatmap_results <- function(df) {
  # Plot results
  #
  # Args:
  # df: Dataframe with HomeTeam, AwayTeam and FTR columns
  #
  # Returns:
  # Ggplot2
  
  library(ggplot2)
  palette <- c("#fc8d59", "#ffffbf", "#91bfdb")
  
  teams <- with(df, sort(unique(c(as.character(HomeTeam), as.character(AwayTeam)))))
  df$HomeTeam <- factor(df$HomeTeam, levels = teams)
  df$AwayTeam <- factor(df$AwayTeam, levels = teams)
  df$FTR <- factor(df$FTR, levels = c("A", "D", "H"), ordered = TRUE)
  
  
  ggplot(data = df, aes(x = AwayTeam, y = HomeTeam, fill = FTR)) +
    geom_raster() +
    scale_fill_manual(values = palette) +
    theme_classic(base_size = 15) +
    theme(axis.text.x = element_text(angle = 90))
}

football_stats <- function(df) {
  # Extract stats from dataframe of games
  #
  # Args:
  # df: Dataframe of games
  #
  # Returns:
  # Datafrane of football statistics
  
  teams <- with(df, sort(unique(c(as.character(HomeTeam), as.character(AwayTeam)))))
  
  # Win
  win_home <- sapply(teams,
                     function(teamName) {
                       with(subset(df, HomeTeam == teamName), sum(FTR ==  "H"))
                     })
  win_away <- sapply(teams,
                     function(teamName) {
                       with(subset(df, AwayTeam == teamName), sum(FTR ==  "A"))
                     })
  # Draw
  draw_home <- sapply(teams,
                      function(teamName) {
                        with(subset(df, HomeTeam == teamName), sum(FTR ==  "D"))
                      })
  draw_away <- sapply(teams,
                      function(teamName) {
                        with(subset(df, AwayTeam == teamName), sum(FTR ==  "D"))
                      })
  # Lose
  lose_home <- sapply(teams,
                      function(teamName) {
                        with(subset(df, HomeTeam == teamName), sum(FTR ==  "A"))
                      })
  lose_away <- sapply(teams,
                      function(teamName) {
                        with(subset(df, AwayTeam == teamName), sum(FTR ==  "H"))
                      })
  # Goal
  goal_tot_home <- sapply(teams,
                      function(teamName) {
                        with(subset(df, HomeTeam == teamName), sum(FTHG))
                      })
  goal_tot_away <- sapply(teams,
                      function(teamName) {
                        with(subset(df, AwayTeam == teamName), sum(FTAG))
                      })
  # Goal difference (goal scored - goal conceded)
  goal_diff_home <- sapply(teams,
                           function(teamName) {
                             with(subset(df, HomeTeam == teamName), sum(FTHG - FTAG))
                           })
  goal_diff_away <- sapply(teams,
                           function(teamName) {
                             with(subset(df, AwayTeam == teamName), sum(FTAG - FTHG))
                           })
  
  # Aggregates
  win <- win_home + win_away
  draw <- draw_home + draw_away
  lose <- lose_home + lose_away
  goal_tot <- goal_tot_home + goal_tot_away
  goal_diff <- goal_diff_home + goal_diff_away
  point <- 3 * win + draw
  
  out <- data.frame(Team = teams,
                    win_home, win_away, win,
                    draw_home, draw_away, draw,
                    lose_home, lose_away, lose,
                    goal_tot_home, goal_tot_away, goal_tot,
                    goal_diff, point)
  out <- merge(out,
               data.frame(Team = teams[order(point, goal_diff, decreasing = TRUE)],
                          rank = 1:length(teams)), # rank
               by = "Team")
  return(out)
}

# Process data ------------------------------------------------------------

game_id <- function(teams) {
  # Associate each game with a unique identifier according to the Stan model
  #
  # Args:
  # teams: Vector of teams names
  #
  # Returns:
  # Dataframe
  
  n_teams <- length(teams)
  n_games <- n_teams * (n_teams - 1)
  out <- data.frame(Game = 1:n_games, HomeTeam = NA, AwayTeam = NA)
  i <- 1
  for (ht in 1:n_teams) {
    for (at in 1:n_teams) {
      if (ht != at) {
        out[i, c("HomeTeam", "AwayTeam")] <- c(teams[ht], teams[at])
        i <- i + 1
      }
    }
  }
  return(out)
}

# Processing Stan output --------------------------------------

extract_parameters <- function(fit, param, param_ind, param_obs, teams, games, data_stan) {
  # Extract parameters' summary
  #
  # Args:
  # fit: stanfit object
  # param: parameters to extract
  # param_ind: individual parameters in param
  # param_obs
  # teams: vector of team ID
  # games: vector of game ID
  # data_stan: data input to the stan function
  #
  # Returns: dataframe containing posterior summary statistics of the parameters 
  
  par <- HuraultMisc::summary_statistics(fit, param)
  par$Team <- NA
  par$Game <- NA

  ## Team dependent parameters
  for (i in intersect(param_ind, param)) {
    idx <- which(par$Variable == i)
    par$Team[idx] <- teams[par$Index[idx]]
  }
  
  ## Game dependent parameters
  for (i in intersect(param_obs, param)) {
    idx <- which(par$Variable == i)
    par$Game[idx] <- games[par$Index[idx]]
  }

  par$Index <- NULL
  return(par)
}

compute_rank <- function(fit, sfx = "rep") {
  # Compute ranking for each replication/test
  #
  # Args:
  # fit: stanfit object
  # sfx: suffix indicating whether to extract replications or test (without underscore)
  #
  # Return:
  # Matrix of posterior samples of the rank (similar to rstan::extract output)
  
  pt <- rstan::extract(fit, pars = paste("point", sfx, sep = "_"))[[1]]
  gd <- rstan::extract(fit, pars = paste("goal_diff", sfx, sep = "_"))[[1]]
  n_teams <- ncol(pt)
  t(sapply(1:nrow(pt),
           function(i) {
             ind <- order(pt[i, ], gd[i, ], decreasing = TRUE)
             rk <- rep(NA, n_teams)
             rk[ind] <- 1:n_teams
             return(rk)
           }))
}

# Fake data check ----------------------------------------

check_estimates <- function(par, true_param, param_pop, param_ind) {
  # Plot estimates versus true values
  #
  # Args:
  # par: Dataframe of parameter estimates
  # true_param: Dataframe of true parameter values
  # param_pop: Vector of names of population parameters
  # param_ind: Vector of names of team parameters
  #
  # Return:
  # List of Ggplot: one plot for population parameter and other plot for each team parameter
  
  library(ggplot2)
  
  tmp <- merge(subset(par, Variable %in% c(param_pop, param_ind)),
               true_param,
               by = c("Variable", "Team"))
  prop90 <- with(tmp, mean(True > `5%` & Mean < `95%`)) # Proportion of true values in 90% CI
  
  # Population parameters
  p1 <- ggplot(data = subset(tmp, Variable %in% param_pop),
               aes(x = Variable)) +
    geom_pointrange(aes(y = Mean, ymin = `5%`, ymax = `95%`)) +
    geom_point(aes(y = True), col = "#E69F00", size = 2) +
    coord_flip() +
    labs(x = "", y = "Estimate") +
    theme_bw(base_size = 20)
  
  # Team parameters
  pl <- lapply(param_ind,
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
  
  return(c(list(p1), pl, prop90 = prop90))
}

# Analyse posterior ---------------------------------------------

plot_abilities <- function(par) {
  # Plot attack and defence estimates (ordered by best attack)
  #
  # Args:
  # par: parameters dataframe
  #
  # Returns:
  # Ggplot
  
  library(ggplot2)
  
  atc <- subset(par, Variable == "attack")
  dfc <- subset(par, Variable == "defence")
  
  # Order teams by best attack
  ord <- atc$Team[order(atc$Mean)]
  atc$Team <- factor(atc$Team, levels = ord)
  dfc$Team <- factor(dfc$Team, levels = ord)
  
  ggplot(data = rbind(atc, dfc),
         aes(x = Team, y = Mean, ymin = `5%`, ymax = `95%`)) +
    geom_pointrange() +
    facet_grid(cols = vars(Variable)) +
    coord_flip() +
    labs(x = "", y = "") +
    theme_bw(base_size = 15)
}

PPC_football_stats <- function(fit, stat_name, fstats, teams, order = FALSE) {
  # Plot posterior predictive checks of some football statistics (number of something)
  #
  # Args:
  # fit: stanfit object
  # stat_name: name of the statistics to show (with suffix _rep or _test)
  # fstats: Dataframe of observed football statistics
  # teams: vector of team names (in the same order as in the model)
  # order: whether to order team by the observed statistics
  #
  # Returns:
  # Ggplot
  
  library(ggplot2)
  
  sfx <- tail(strsplit(stat_name, "_")[[1]], 1)
  fstat_name <- gsub(paste("_", sfx, sep = ""), "", stat_name)

  if (fstat_name != "rank") {
    tmp <- rstan::extract(fit, pars = stat_name)[[1]]
  } else {
    tmp <- compute_rank(fit, sfx)
  }
  
  n_teams <- length(teams)
  n_min <- round(min(0, min(tmp)))
  n_max <- round(max(0, max(tmp) * 1.1))
  # Compute probability table from posterior samples
  out <- do.call(rbind,
                 lapply(1:n_teams,
                        function(i) {
                          p <- table(factor(tmp[, i], levels = 0:n_max)) / nrow(tmp)
                          data.frame(Team = teams[i],
                                     N = as.numeric(names(p)),
                                     Probability = as.numeric(p),
                                     Statistic = "p",
                                     Actual = FALSE)
                        }))
  
  # Fill actual column with observed value of statistics
  for (i in 1:n_teams) {
    out$Actual[out$Team == teams[i] & out$N == subset(fstats, Team == teams[i])[[fstat_name]]] <- TRUE
  }
  
  # Order teams by observed football statistics
  if (order) {
    out$Team <- factor(out$Team, levels = teams[order(fstats[[fstat_name]])])
  }
  
  ggplot(data = out, aes(x = N, y = Probability, fill = Actual)) +
    scale_fill_manual(values = c("#000000", "#E69F00")) +
    geom_bar(stat = "identity") +
    facet_grid(rows = vars(Team)) +
    labs(x = fstat_name) +
    theme_bw(base_size = 15) +
    theme(legend.position = "none")
}

stackhist_rank <- function(rank_rep, teams) {
  # Plot rank as a stacked histogram
  #
  # Args:
  # rank_rep: Posterior samples of rank (output from compute_rank)
  # teams: vector of team names
  #
  # Returns:
  # Ggplot
  
  library(ggplot2)
  getPalette <- colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Set1"))) # to extend colour palette
  
  rk <- do.call(rbind,
                lapply(1:length(teams),
                       function(i) {
                         tmp <- table(factor(rank_rep[, i], levels = 1:length(teams))) / nrow(rank_rep)
                         data.frame(Team = teams[i], Rank = names(tmp), Probability = as.numeric(tmp))
                       }))
  rk <- HuraultMisc::factor_to_numeric(rk, "Rank")
  # Order by expected rank
  exp_rank <- apply(rank_rep, 2, mean)
  rk$Team <- factor(rk$Team, levels = teams[order(exp_rank, decreasing = TRUE)])
  
  ggplot(data = rk, aes(x = Rank, y = Probability, fill = Team)) +
    geom_histogram(stat = "identity") +
    scale_x_continuous(breaks = 1:length(teams), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = getPalette(length(teams))) +
    labs(y = "Cumulative probability") +
    theme_classic(base_size = 20)
}

# Validation --------------------------------------------------------------

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
             Mean = sapply(list(RPS, CumLogLoss, BrierScore, LogLoss), mean),
             SE = sapply(list(RPS, CumLogLoss, BrierScore, LogLoss), function(x) {sd(x) / sqrt(length(x))}),
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
