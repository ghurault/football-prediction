// Poisson model with constant attack/defence abilities and constant home advantage
// "_train" suffix to denote training data
// "_rep" suffix to denote replications (on train) data
// "_test" suffix to denote testing data (and predicted goals)
// "_pred" suffix to denote combined observed (train) and predicted (test) data

data {
  int<lower = 0> N_teams; // Number of teams
  // Training
  int<lower = 0> N_games_train; // Number of games
  int<lower = 0> home_goals_train[N_games_train]; // Number of goals scored by home team
  int<lower = 0> away_goals_train[N_games_train]; // Number of goals scored by away team
  int<lower = 1, upper = N_teams> home_id_train[N_games_train]; // ID of home team
  int<lower = 1, upper = N_teams> away_id_train[N_games_train]; // ID of away team
  int<lower = 0, upper = 1> run; // Switch for inference
  // Testing
  int<lower = 0, upper = N_teams * (N_teams - 1) - N_games_train> N_games_test; // Number of games to predict
  int<lower = 1, upper = N_teams> home_id_test[N_games_test]; // ID of home team
  int<lower = 1, upper = N_teams> away_id_test[N_games_test]; // ID of away team
}

transformed data {
  int N = N_teams * (N_teams - 1); // Total number of games
}

parameters {
  real b; // Intercept
  real home_advantage; // Home advantage
  vector[N_teams] attack; // Latent attack ability
  vector[N_teams] defence; // Latent defence ability
  real<lower = 0> sigma_ability; // Population standard deviation for latent abilities
}

transformed parameters {
  vector[N_games_train] home_pred = b +
  home_advantage +
  attack[home_id_train] -
  defence[away_id_train]; // Log rate of goals for home team
  
  vector[N_games_train] away_pred = b +
  attack[away_id_train] -
  defence[home_id_train]; // Log rate of goals for away team
}

model {
  // Priors
  b ~ normal(0, 0.5); // exp(b) is average number of goals in situation where attack and defence cancels out, prior between e^-1 and e^1
  home_advantage ~ normal(0.5, 0.25); // Home advantage assume to be positive but not excessive
  attack ~ normal(0, sigma_ability);
  defence ~ normal(0, sigma_ability);
  sigma_ability ~ normal(0, log(10) / 2.3 / sqrt(2)); // If attack independent from defence, (attack - defence follow) N(0, sqrt(2) * sigma_ability). A situation at the upper tail of distribution, the team would score exp(2 * sqrt(2) * sigma_ability) more goals, for instance 10 times
  
  // Likelihood
  if (run == 1) {
    home_goals_train ~ poisson_log(home_pred);
    away_goals_train ~ poisson_log(away_pred);
  }
  
}

generated quantities {
  // REPLICATIONS
  // Goals
  int home_goals_rep[N_games_train] = poisson_log_rng(home_pred);
  int away_goals_rep[N_games_train] =  poisson_log_rng(away_pred);
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
  // TEST
  int home_goals_test[N_games_test];
  int away_goals_test[N_games_test];
  // PREDICTIONS
  int home_goals_pred[N]; // First N_games element correspond to played games, then correspond to home_test_id
  int away_goals_pred[N];
  // Number of wins/lose/draws
  vector[N_teams] win_home_pred = rep_vector(0, N_teams);
  vector[N_teams] win_away_pred = rep_vector(0, N_teams);
  vector[N_teams] win_pred;
  vector[N_teams] draw_home_pred = rep_vector(0, N_teams);
  vector[N_teams] draw_away_pred = rep_vector(0, N_teams);
  vector[N_teams] draw_pred;
  vector[N_teams] lose_home_pred = rep_vector(0, N_teams);
  vector[N_teams] lose_away_pred = rep_vector(0, N_teams);
  vector[N_teams] lose_pred;
  // Number of goals
  vector[N_teams] goal_home_pred = rep_vector(0, N_teams);
  vector[N_teams] goal_away_pred = rep_vector(0, N_teams);
  vector[N_teams] goal_pred;
  // Goal difference (scored - conceded)
  vector[N_teams] goal_diff_home_pred = rep_vector(0, N_teams);
  vector[N_teams] goal_diff_away_pred = rep_vector(0, N_teams);
  vector[N_teams] goal_diff_pred;
  // Number of points
  vector[N_teams] point_pred;
  
  // REPLICATIONS
  for (i in 1:N_games_train) {
    goal_home_rep[home_id_train[i]] += home_goals_rep[i];
    goal_away_rep[away_id_train[i]] += away_goals_rep[i];
    goal_diff_home_rep[home_id_train[i]] += home_goals_rep[i] - away_goals_rep[i];
    goal_diff_away_rep[home_id_train[i]] += away_goals_rep[i] - home_goals_rep[i];
    if (home_goals_rep[i] > away_goals_rep[i]) {
      win_home_rep[home_id_train[i]] += 1;
      lose_away_rep[away_id_train[i]] += 1;
    } else if (home_goals_rep[i] == away_goals_rep[i]) {
      draw_home_rep[home_id_train[i]] += 1;
      draw_away_rep[away_id_train[i]] += 1;
    } else {
      lose_home_rep[home_id_train[i]] += 1;
      win_away_rep[away_id_train[i]] += 1;
    }
  }
  win_rep = win_home_rep + win_away_rep;
  draw_rep = draw_home_rep + draw_away_rep;
  lose_rep = lose_home_rep + lose_away_rep;
  goal_rep = goal_home_rep + goal_away_rep;
  goal_diff_rep = goal_diff_home_rep + goal_diff_away_rep;
  point_rep = 3 * win_rep + draw_rep;
  
  // TEST
  {
    int idx = 1; // Indexing test arrays
    int ht;
    int at;
    real ht_pred;
    real at_pred;
    for (i in 1:N) {
      if (i <= N_games_train) {
        // Fill with already played games
        ht = home_id_train[i];
        at = away_id_train[i];
        home_goals_test[i] = home_goals_train[i];
        away_goals_test[i] = away_goals_train[i];
      } else {
        // Complete with predictions
        ht = home_id_test[i - N_games_train];
        at = away_id_test[i - N_games_train];
        ht_pred = b + home_advantage + attack[ht] - defence[at];
        at_pred = b + attack[at] - defence[ht];
        home_goals_test[idx] = poisson_log_rng(ht_pred);
        away_goals_test[idx] = poisson_log_rng(at_pred);
      }
      // Compute stats
      goal_home_pred[ht] += home_goals_test[idx];
      goal_away_pred[at] += away_goals_test[idx];
      goal_diff_home_pred[ht] += home_goals_test[idx] - away_goals_test[idx];
      goal_diff_away_pred[at] += away_goals_test[idx] - home_goals_test[idx];
      if (home_goals_pred[idx] > away_goals_test[idx]) {
        win_home_pred[ht] += 1;
        lose_away_pred[at] += 1;
      } else if (home_goals_test[idx] == away_goals_test[idx]) {
        draw_home_pred[ht] += 1;
        draw_away_pred[at] += 1;
      } else {
        win_away_pred[at] += 1;
        lose_home_pred[ht] += 1;
      }
      idx += 1;
    }
    win_pred = win_home_pred + win_away_pred;
    draw_pred = draw_home_pred + draw_away_pred;
    lose_pred = lose_home_pred + lose_away_pred;
    goal_pred = goal_home_pred + goal_away_pred;
    goal_diff_pred = goal_diff_home_pred + goal_diff_away_pred;
    point_pred = 3 * win_pred + draw_pred;
  }
  
}
