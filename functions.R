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
