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

# Processing Stan output --------------------------------------

extract_parameters <- function(fit, param, param_ind, param_obs, teams, data_stan) {
  # Extract parameters' summary
  #
  # Args:
  # fit: stanfit object
  # param: parameters to extract
  # param_ind: individual parameters in param
  # param_obs
  # teams: vector of team ID
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
    par$Game[idx] <- par$Index[idx]
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

plot_prior_posterior <- function(par, par0, param) {
  # Plot posterior estimates alongside prior estimates
  #
  # Args:
  # par: Dataframe of posterior parameter estimates
  # par0: Dataframe of prior parameter estimates
  # param: Vector of parameter names to plot
  #
  # Returns:
  # Ggplot
  
  library(ggplot2)
  
  par <- subset(par, Variable %in% param)
  par$Distribution <- "Posterior"
  par0 <- subset(par0, Variable %in% param)
  par0$Distribution <- "Prior"
  tmp <- rbind(par, par0)
  tmp$Distribution <- factor(tmp$Distribution, levels = c("Prior", "Posterior")) # to show posterior on top
  
  ggplot(data = tmp, aes(x = Variable, y = Mean, ymin = `5%`, ymax = `95%`, colour = Distribution)) +
    geom_pointrange(position = position_dodge2(width = .3), size = 1.2) +
    scale_colour_manual(values = c("#E69F00", "#000000")) +
    coord_flip() +
    labs(colour = "", x = "", y = "Estimate") +
    theme_bw(base_size = 20) +
    theme(legend.position = "top")
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
  getPalette = colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Set1"))) # to extend colour palette
  
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
