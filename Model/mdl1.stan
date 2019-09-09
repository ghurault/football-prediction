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
  // Goals
  int home_goals_rep[N_games] = poisson_log_rng(home_pred);
  int away_goals_rep[N_games] =  poisson_log_rng(away_pred);
  // Number of win/draw/lose
  int N_win_home_rep[N_teams] = rep_vector(0, N_teams);
  int N_win_away_rep[N_teams] = rep_vector(0, N_teams);
  int N_win_rep[N_teams];
  int N_draw_home_rep[N_teams] = rep_vector(0, N_teams);
  int N_draw_away_rep[N_teams] = rep_vector(0, N_teams);
  int N_draw_rep[N_teams];
  int N_lose_home_rep[N_teams] = rep_vector(0, N_teams);
  int N_lose_away_rep[N_teams] = rep_vector(0, N_teams);
  int N_lose_rep[N_teams];
  // Number of goals
  int N_goal_home_rep[N_teams] = rep_vector(0, N_teams);
  int N_goal_away_rep[N_teams] = rep_vector(0, N_teams);
  int N_goal_rep[N_teams];
  // Number of points
  int N_point_rep[N_teams];
  // Rank
  int rank_rep[N_teams];
  
  for (i in 1:N_games) {
    N_goal_home_rep[home_id[i]] += home_goals_rep[i];
    N_goal_away_rep[away_id[i]] += away_goals_rep[i];
    if (home_goals_rep[i] > away_goals_rep[i]) {
      N_win_home_rep[home_id[i]] += 1;
      N_lose_away_rep[away_id[i]] += 1;
    } else if (home_goals_rep[i] == away_goals_rep[i]) {
      N_draw_home_rep[home_id[i]] += 1;
      N_draw_away_rep[away_id[i]] += 1;
    } else {
      N_lose_home_rep[home_id[i]] += 1;
      N_win_away_rep[away_id[i]] += 1;
    }
  }
  N_win_rep = N_win_home_rep + N_win_away_rep;
  N_draw_rep = N_draw_home_rep + N_draw_away_rep;
  N_lose_rep = N_lose_home_rep + N_lose_away_rep;
  N_goal_rep = N_goal_home_rep + N_goal_away_rep;
  N_point_rep = 3 * N_win_rep + N_draw_rep;
  rank_rep = sort_indices_desc(N_point_rep);
}
