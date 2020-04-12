# Notes -------------------------------------------------------------------

#

# Initialisation ----------------------------------------------------------

rm(list = ls())

seed <- 1559354162
set.seed(seed) # Reproducibility

library(HuraultMisc)
library(ggplot2)
library(cowplot)
library(rstan)
rstan_options(auto_write = TRUE) # Save compiled model
options(mc.cores = parallel::detectCores()) # Parallel computing
source("functions.R")

data_file <- "Data/PremierLeague1819.csv"

stan_code <- "Model/DC_model.stan"

run <- FALSE
res_file <- "Results/val_mdl1.rds"

# MCMC preparation --------------------------------------------------------

n_cluster <- 10

n_chains <- 4
n_it <- 2000

# Parameters of interest
param_pop <- c("b", "home_advantage", "sigma_ability")
param_rep <- c(
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


param_ind <- c("attack", "defence", param_test)
param_obs <- c()
param <- c(param_pop, param_ind, param_obs, "home_goals_test", "away_goals_test")

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

compiled_model <- rstan::stan_model(stan_code)

if (run) {
  library(foreach)
  library(doParallel)
  
  duration <- Sys.time()
  cl <- makeCluster(n_cluster)
  registerDoParallel(cl)
  writeLines(c(""), "log.txt")
  
  out <- foreach(w = 1:(length(weeks) - 1)) %dopar% {
    
    source("functions.R")
    library(rstan)
    rstan_options(auto_write = TRUE) # Save compiled model
    options(mc.cores = parallel::detectCores()) # Parallel computing
    
    sink("log.txt", append = TRUE)
    cat(paste("Starting training at week ", w, " \n", sep = ""))
    
    sub_df <- df[df$WeekNumber <= weeks[w], ]
    test_game <- df[df[["WeekNumber"]] > median(weeks), "Game"]
    
    data_stan <- format_stan_data(sub_df)
    
    fit <- sampling(compiled_model, data = data_stan, pars = param,
                    iter = n_it, chains = n_chains, seed = seed)
    
    # Parameters
    par <- extract_parameters(fit, param = c(param_pop, param_ind), param_ind, param_obs, teams, sub_df[["Game"]], data_stan)
    par$WeekNumber <- weeks[w]
    par$ProportionGamePlayed <- nrow(sub_df) / nrow(df)
    
    # Rank
    rk <- compute_rank(fit, "test")
    rk <- do.call(rbind,
                  lapply(1:length(teams),
                         function(i) {
                           tmp <- table(factor(rk[, i], levels = 1:length(teams))) / nrow(rk)
                           data.frame(Team = teams[i], Rank = names(tmp), Probability = as.numeric(tmp))
                         }))
    rk <- HuraultMisc::factor_to_numeric(rk, "Rank")
    rk$WeekNumber <- weeks[w]
    rk$ProportionGamePlayed <- nrow(sub_df) / nrow(df)
    
    # Metrics
    pred <- process_predictions(fit, id)
    m <- compute_metrics(pred = pred, act = df, test_game = test_game, var = "FTR")
    m$WeekNumber <- weeks[w]
    m$ProportionGamePlayed <- nrow(sub_df) / nrow(df)
    
    list(Performance = m, Parameters = par, Rank = rk)
  }
  stopCluster(cl)
  (duration = Sys.time() - duration)
  
  saveRDS(out, file = res_file)
} else {
  out <- readRDS(res_file)
}

m <- do.call(rbind, lapply(out, function(x) {x$Performance}))
par <- do.call(rbind, lapply(out, function(x) {x$Parameters}))
rk <- do.call(rbind, lapply(out, function(x) {x$Rank}))

# Analysis ----------------------------------------------------------------

if (FALSE) {
  
  # Evolution of RPS (FTR) as a function of WeekNumber or ProportionGamePlayed
  pl1 <- lapply(unique(m$Metric),
                function(x) {
                  ggplot(data = subset(m, Metric == x),
                         aes(x = ProportionGamePlayed, y = Mean, ymin = Mean - SE, ymax = Mean + SE)) +
                    geom_pointrange() +
                    scale_y_continuous(limits = c(0, NA)) +
                    labs(y = x) +
                    theme_bw(base_size = 15) # + theme(axis.text.x = element_text(angle = 90))
                })
  plot_grid(plotlist = pl1, nrow = 2)
  
  # Evolution of predictions of rank
  rk <- rbind(data.frame(expand.grid(Team = teams, Rank = 1:length(teams)),
                         Probability = 1 / length(teams), WeekNumber = strftime(min(df[["Date"]]) - 7, format = "%Y-%V"), ProportionGamePlayed = 0), # add first week
              rk)
  tmp <- data.frame(expand.grid(Team = teams, Rank = 1:length(teams)),
                    Probability = 0, WeekNumber = weeks[length(weeks)], ProportionGamePlayed = 1)
  for (i in 1:nrow(fstats)) {
    id <- which((tmp[["Team"]] == fstats[i, "Team"]) & (tmp[["Rank"]] == fstats[i, "rank"]))
    tmp[id, "Probability"] <- 1
  }
  rk <- rbind(rk, tmp) # add last week
  pl2 <- lapply(teams,
         function(x) {
           ggplot(data = subset(rk, Team == x),
                  aes(x = factor(ProportionGamePlayed), y = Rank, fill = Probability)) + # use factor because hard to make tiles of varying width...
             geom_tile() +
             scale_fill_viridis_c() +
             scale_y_continuous(expand = c(0, 0), breaks = 1:length(teams)) +
             scale_x_discrete(expand = c(0, 0), breaks = c(0, 1)) +
             labs(title = x, x = "Proportion of game played*") + # * not exactly but close
             theme_classic(base_size = 15)
         })
  plot_grid(get_legend(pl2[[1]] + theme(legend.position = "top")),
            plot_grid(plotlist = lapply(pl2, function(x) {x + theme(legend.position = "none")}),
                      nrow = 5),
            nrow = 2, rel_heights = c(.05, .95))

  # Evolution of belief in population parameters
  pl3 <- lapply(param_pop,
                function(x) {
                  ggplot(data = subset(par, Variable == x),
                         aes(x = ProportionGamePlayed, y = Mean, ymin = `5%`, ymax = `95%`)) +
                    geom_pointrange() +
                    labs(y = x) +
                    theme_bw(base_size = 15)
                })
  plot_grid(plotlist = pl3, nrow = 2)
  
  # Evolution of belief in abilities
  tmp <- subset(par, Variable %in% c("attack", "defence"))
  pl4 <- lapply(teams, function(x) {
    cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    ggplot(data = subset(tmp, Team == x), 
           aes(x = ProportionGamePlayed, y = Mean, ymin = `5%`, ymax = `95%`, colour = Variable, fill = Variable)) +
      geom_line() +
      geom_ribbon(alpha = 0.5) +
      scale_colour_manual(values = cbbPalette) +
      scale_fill_manual(values = cbbPalette) +
      labs(title = x, y = "Ability", colour = "", fill = "") +
      coord_cartesian(ylim = c(-1, 1)) +
      theme_bw(base_size = 15)
  })
  plot_grid(get_legend(pl4[[1]] + theme(legend.position = "top")),
            plot_grid(plotlist = lapply(pl4, function(p) {p + theme(legend.position = "none")}),
                      nrow = 5),
            nrow = 2, rel_heights = c(.05, .95))
  
  # Refit model at the mid-season
  sub_df <- df[df[["WeekNumber"]] <= median(weeks), ]
  test_game <- df[df[["WeekNumber"]] > median(weeks), "Game"]
  data_stan <- format_stan_data(sub_df)
  fit <- sampling(compiled_model, data = data_stan, pars = param,
                  iter = n_it, chains = n_chains, seed = seed)
  # Statistics predictions (in orange the actual statistic at the end of the season)
  PPC_football_stats(fit, "win_test", fstats, teams)
  PPC_football_stats(fit, "lose_test", fstats, teams)
  PPC_football_stats(fit, "point_test", fstats, teams)
  stackhist_rank(compute_rank(fit, "test"), teams)
  # Calibration and lift for FTR
  pred <- process_predictions(fit, id)
  l1 <- prepare_predictions(pred = pred, act = df, test_game = test_game, var = "FTR")
  plot_lift(l1) + theme(legend.position = "top")
  plot_calibration(l1, CI = NULL)
  plot_calibration(l1, CI = 0.95, pool = TRUE)
  # Calibration and lift for goals (FTHG, FTAG)
  l2 <- prepare_predictions(pred = pred, act = df, test_game = test_game, var = "FTHG")
  plot_lift(l2, best_bet = TRUE)
  # plot_calibration(l2, CI = NULL) # problem with loess in plot_calibration for l2 (probably because of few outcomes)
  
}
