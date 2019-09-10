// Poisson model with constant attack/defence abilities and constant home advantage

functions {
  int[] get_rank(int[] indices) {
    int n = size(indices);
    int rk[n];
    for (i in 1:n) {
      rk[indices[i]] = i;
    }
    return rk;
  }
}

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
  // Goals
  int home_goals_rep[N_games] = poisson_log_rng(home_pred);
  int away_goals_rep[N_games] =  poisson_log_rng(away_pred);
  // Number of win/draw/lose
  vector[N_teams] win_home_rep = rep_vector(0, N_teams);
  vector[N_teams] win_away_rep = rep_vector(0, N_teams);
  vector[N_teams] win_rep;
  vector[N_teams] draw_home_rep = rep_vector(0, N_teams);
  vector[N_teams] draw_away_rep = rep_vector(0, N_teams);
  vector[N_teams] draw_rep;
  vector[N_teams] lose_home_rep = rep_vector(0, N_teams);
  vector[N_teams] lose_away_rep = rep_vector(0, N_teams);
  vector[N_teams] lose_rep;
  // Number of goals
  vector[N_teams] goal_home_rep = rep_vector(0, N_teams);
  vector[N_teams] goal_away_rep = rep_vector(0, N_teams);
  vector[N_teams] goal_rep;
  // Goal difference (scored - conceded)
  vector[N_teams] goal_diff_home_rep = rep_vector(0, N_teams);
  vector[N_teams] goal_diff_away_rep = rep_vector(0, N_teams);
  vector[N_teams] goal_diff_rep;
  // Number of points
  vector[N_teams] point_rep;
  
  for (i in 1:N_games) {
    goal_home_rep[home_id[i]] += home_goals_rep[i];
    goal_away_rep[away_id[i]] += away_goals_rep[i];
    goal_diff_home_rep[home_id[i]] += home_goals_rep[i] - away_goals_rep[i];
    goal_diff_away_rep[home_id[i]] += away_goals_rep[i] - home_goals_rep[i];
    if (home_goals_rep[i] > away_goals_rep[i]) {
      win_home_rep[home_id[i]] += 1;
      lose_away_rep[away_id[i]] += 1;
    } else if (home_goals_rep[i] == away_goals_rep[i]) {
      draw_home_rep[home_id[i]] += 1;
      draw_away_rep[away_id[i]] += 1;
    } else {
      lose_home_rep[home_id[i]] += 1;
      win_away_rep[away_id[i]] += 1;
    }
  }
  win_rep = win_home_rep + win_away_rep;
  draw_rep = draw_home_rep + draw_away_rep;
  lose_rep = lose_home_rep + lose_away_rep;
  goal_rep = goal_home_rep + goal_away_rep;
  goal_diff_rep = goal_diff_home_rep + goal_diff_away_rep;
  point_rep = 3 * win_rep + draw_rep;
}
