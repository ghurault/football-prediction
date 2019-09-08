// Poisson model with constant attack/defence abilities and constant home advantage

data {
    int<lower = 0> N_teams; // Number of teams
    int<lower = 0> N_games; // Number of games
    
    int<lower = 0> home_goals[N_games]; // Number of goals scored by home team
    int<lower = 0> away_goals[N_games]; // Number of goals scored by away team
    int<lower = 1, upper = N_teams> home_id[N_games]; // ID of home team
    int<lower = 1, upper = N_teams> away_id[N_games]; // ID of away team
    
    int<lower = 0, upper = 1> run; // Switch for inference
}

parameters {
    real b; // Intercept
    real home_advantage; // Home advantage
    vector[N_teams] attack; // Latent attack ability
    vector[N_teams] defence; // Latent defence ability
    real<lower = 0> sigma_ability; // Population standard deviation for latent abilities
}

transformed parameters {
  vector[N_games] home_pred = b +
        home_advantage +
        attack[home_id] -
        defence[away_id]; // Log rate of goals for home team

  vector[N_games] away_pred = b +
        attack[away_id] -
        defence[home_id]; // Log rate of goals for away team
}

model {
  // Priors
  b ~ normal(0, 1);
  home_advantage ~ normal(0, 1);
  attack ~ normal(0, sigma_ability);
  defence ~ normal(0, sigma_ability);
  sigma_ability ~ normal(0, 1);
  
  // Likelihood
  if (run == 1) {
    home_goals ~ poisson_log(home_pred);
    away_goals ~ poisson_log(away_pred);
  }
    
}

generated quantities {
  int home_goals_rep[N_games] = poisson_log_rng(home_pred);
  int away_goals_rep[N_games] =  poisson_log_rng(away_pred);
}
