// Poisson model with constant attack/defence abilities and constant home advantage

functions {
  int get_test_id(int ht, int at, int nt) {
    // Get row id for test variables ordered by home team then away team
    //
    // Args:
    // ht: home team id
    // at: away team id
    // nt: number of teams
    //
    // Returns:
    // Corresponding id in test
    
    int out = (ht - 1) * (nt - 1) + at;
    if (at > ht) {
      out = out - 1;
    }
    return out;
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

transformed data {
  int N = N_teams * (N_teams - 1); // Total number of games
  int is_played[N] = rep_array(0, N); // Array indicating whether a game has been played
  
  for (i in 1:N_games) {
    is_played[get_test_id(home_id[i], away_id[i], N_teams)] = 1;
  }
}

parameters {
  real b; // Intercept
  real home_advantage; // Home advantage
  vector[N_teams] attack; // Latent attack ability
  vector[N_teams] defence; // Latent defence ability
  real<lower = 0> sigma_ability; // Population standard deviation for latent abilities
}

transformed parameters {
  // Define abilities for all possible games, convenient because everything is defined only once
  matrix[N_teams, N_teams] home_linpred;
  matrix[N_teams, N_teams] away_linpred;
  vector[N_games] home_linpred1;
  vector[N_games] away_linpred1;
  
  for (j in 1:N_teams) {
    for (i in 1:N_teams) {
      home_linpred[i, j] = b + home_advantage + attack[i] - defence[j];
      away_linpred[i, j] = b + attack[j] - defence[i];
    }
  }
  for (i in 1:N_games) {
    home_linpred1[i] = home_linpred[home_id[i], away_id[i]];
    away_linpred1[i] = away_linpred[home_id[i], away_id[i]];
  }
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
    home_goals ~ poisson_log(home_linpred1);
    away_goals ~ poisson_log(away_linpred1);
  }
}

generated quantities {
  // REPLICATIONS
  // Goals
  int home_goals_rep[N_games] = poisson_log_rng(home_linpred1);
  int away_goals_rep[N_games] =  poisson_log_rng(away_linpred1);
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
  // Total number of goals
  vector[N_teams] goal_tot_home_rep = rep_vector(0, N_teams);
  vector[N_teams] goal_tot_away_rep = rep_vector(0, N_teams);
  vector[N_teams] goal_tot_rep;
  // Goal difference (scored - conceded)
  vector[N_teams] goal_diff_home_rep = rep_vector(0, N_teams);
  vector[N_teams] goal_diff_away_rep = rep_vector(0, N_teams);
  vector[N_teams] goal_diff_rep;
  // Number of points
  vector[N_teams] point_rep;
  
  // TEST
  int home_goals_test[N];
  int away_goals_test[N];
  // Number of wins/lose/draws
  vector[N_teams] win_home_test = rep_vector(0, N_teams);
  vector[N_teams] win_away_test = rep_vector(0, N_teams);
  vector[N_teams] win_test;
  vector[N_teams] draw_home_test = rep_vector(0, N_teams);
  vector[N_teams] draw_away_test = rep_vector(0, N_teams);
  vector[N_teams] draw_test;
  vector[N_teams] lose_home_test = rep_vector(0, N_teams);
  vector[N_teams] lose_away_test = rep_vector(0, N_teams);
  vector[N_teams] lose_test;
  // Number of goals
  vector[N_teams] goal_tot_home_test = rep_vector(0, N_teams);
  vector[N_teams] goal_tot_away_test = rep_vector(0, N_teams);
  vector[N_teams] goal_tot_test;
  // Goal difference (scored - conceded)
  vector[N_teams] goal_diff_home_test = rep_vector(0, N_teams);
  vector[N_teams] goal_diff_away_test = rep_vector(0, N_teams);
  vector[N_teams] goal_diff_test;
  // Number of points
  vector[N_teams] point_test;
  
  // REPLICATIONS
  for (i in 1:N_games) {
    goal_tot_home_rep[home_id[i]] += home_goals_rep[i];
    goal_tot_away_rep[away_id[i]] += away_goals_rep[i];
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
  goal_tot_rep = goal_tot_home_rep + goal_tot_away_rep;
  goal_diff_rep = goal_diff_home_rep + goal_diff_away_rep;
  point_rep = 3 * win_rep + draw_rep;
  
  // TEST
  {
    int id; // Test id
    for (g in 1:N_games) {
      // Fill test goals with played games
      id = get_test_id(home_id[g], away_id[g], N_teams);
      home_goals_test[id] = home_goals[g];
      away_goals_test[id] = away_goals[g];
    }
    id = 1;
    for (ht in 1:N_teams) {
      for (at in 1:N_teams) {
        if (ht != at) {
          if (is_played[id] == 0) {
            // Complete predictions
            home_goals_test[id] = poisson_log_rng(home_linpred[ht, at]);
            away_goals_test[id] = poisson_log_rng(away_linpred[ht, at]);
          }
          // Compute stats
          goal_tot_home_test[ht] += home_goals_test[id];
          goal_tot_away_test[at] += away_goals_test[id];
          goal_diff_home_test[ht] += home_goals_test[id] - away_goals_test[id];
          goal_diff_away_test[at] += away_goals_test[id] - home_goals_test[id];
          if (home_goals_test[id] > away_goals_test[id]) {
            win_home_test[ht] += 1;
            lose_away_test[at] += 1;
          } else if (home_goals_test[id] == away_goals_test[id]) {
            draw_home_test[ht] += 1;
            draw_away_test[at] += 1;
          } else {
            win_away_test[at] += 1;
            lose_home_test[ht] += 1;
          }
          id += 1;
        }
      }
    }
    win_test = win_home_test + win_away_test;
    draw_test = draw_home_test + draw_away_test;
    lose_test = lose_home_test + lose_away_test;
    goal_tot_test = goal_tot_home_test + goal_tot_away_test;
    goal_diff_test = goal_diff_home_test + goal_diff_away_test;
    point_test = 3 * win_test + draw_test;
  }
}
