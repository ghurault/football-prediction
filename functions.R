# Heatmap results ---------------------------------------------------------

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

# Extract parameters from Stan model --------------------------------------

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

# Football statistics -----------------------------------------------------

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
  N_win_home <- sapply(teams,
                       function(teamName) {
                         with(subset(df, HomeTeam == teamName), sum(FTR ==  "H"))
                       })
  N_win_away <- sapply(teams,
                       function(teamName) {
                         with(subset(df, AwayTeam == teamName), sum(FTR ==  "A"))
                       })
  # Draw
  N_draw_home <- sapply(teams,
                        function(teamName) {
                          with(subset(df, HomeTeam == teamName), sum(FTR ==  "D"))
                        })
  N_draw_away <- sapply(teams,
                        function(teamName) {
                          with(subset(df, AwayTeam == teamName), sum(FTR ==  "D"))
                        })
  # Lose
  N_lose_home <- sapply(teams,
                        function(teamName) {
                          with(subset(df, HomeTeam == teamName), sum(FTR ==  "A"))
                        })
  N_lose_away <- sapply(teams,
                        function(teamName) {
                          with(subset(df, AwayTeam == teamName), sum(FTR ==  "H"))
                        })
  # Goal
  N_goal_home <- sapply(teams,
                        function(teamName) {
                          with(subset(df, HomeTeam == teamName), sum(FTHG))
                        })
  N_goal_away <- sapply(teams,
                        function(teamName) {
                          with(subset(df, AwayTeam == teamName), sum(FTAG))
                        })
  
  # Aggregates
  N_win <- N_win_home + N_win_away
  N_draw <- N_draw_home + N_draw_away
  N_lose <- N_lose_home + N_lose_away
  N_goal <- N_goal_home + N_goal_away
  N_point <- 3 * N_win + N_draw
  
  out <- data.frame(Team = teams,
                    N_win_home, N_win_away, N_win,
                    N_draw_home, N_draw_away, N_draw,
                    N_lose_home, N_lose_away, N_lose,
                    N_goal_home, N_goal_away, N_goal,
                    N_point)
  out <- merge(out,
               data.frame(Team = teams[order(N_point, N_goal, decreasing = TRUE)],
                          rank = 1:length(teams)), # rank
               by = "Team")
  return(out)
}
