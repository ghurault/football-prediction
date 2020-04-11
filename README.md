# Football prediction model

<!-- badges: start -->
[![Lifecycle: dormant](https://img.shields.io/badge/lifecycle-dormant-blue.svg)](https://www.tidyverse.org/lifecycle/#dormant)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Models for predicting the outcomes of football games in Stan, using publicly available [football data](http://football-data.co.uk/).

**[Notebook in progress](https://ghurault.github.io/football-prediction/overview.nb.html)**

## Model

Here, I implemented a simple Dixon-Coles model where we assume that the number of goals scored by each team follow independent Poisson distributions.

For each game, if we index the home team by `h` and the away team by `a`, then the rates of the Poisson distribution are given by:

<img src="https://latex.codecogs.com/gif.latex?$$\log(\lambda_h)&space;=&space;b&space;&plus;&space;\mathit{advtg}&space;&plus;&space;\mathit{attack_h}&space;-&space;\mathit{defence_a}&space;$$" title="$$\log(\lambda_h) = b + \mathit{advtg} + \mathit{attack_h} - \mathit{defence_a} $$" />

<img src="https://latex.codecogs.com/gif.latex?$$\log(\lambda_a)&space;=&space;b&space;&plus;&space;\mathit{attack_a}&space;-&space;\mathit{defence_h}&space;$$" title="$$\log(\lambda_a) = b + \mathit{attack_a} - \mathit{defence_h} $$" />

Where `b` is the intercept (logarithm of the average goals rate assuming the attack and defence abilities of the teams cancels out), `advtg` is the home advantage, and `attack` and `defence` are the vectors containing the latent attack and defence abilities for each team.

The model is available in [`Model/mdl1.stan`](Model/mdl1.stan), where priors choices are motivated in the comments.

While the model is simple (but could be easily extended given time), I see my main contribution is on the quantities that we can generate, notably important statistics that the model can predict (cf. suffix `_test`) or that can be used for posterior predictive checking (cf. suffix `rep`):

- the number of games won
- the number of games lost
- the number of games ending in a draw
- the total number of goals scored
- the goal difference
- the number of points
- the ranking

## Workflow

Utility functions are available in [`functions.R`](functions.R).

The script [`prior_pred_check.R`](prior_pred_check.R) contains:

- Prior predictive check, to check whether the choice of priors leads to reasonable simulations in terms of the number/rate of goals scored, the probability of winning x games in the season, the position in the ranking, etc.
- Fake data check, to check whether we can estimate the model from data simulated from the proposed generative process (i.e. to check whether the inference process works).
I notably checked whether the estimated parameters are "close" to the true parameters, and the coverage probability of the hierarchical parameters.
Simulation-Based Calibration could then be implemented to extend this.
- The pipeline to generate and evaluate predictions (more details below).

The model is fitted to real data in [`fitting.R`](fitting.R) and posterior predictive check is performed.

The predictive performance of the model is computed in [`validation.R`](validation.R) using forward chaining: the model is trained with the data from the first week and tested on the remaining games, then trained on the data from the first two weeks and tested on the remaining games, etc.

We assessed performance using:

- the Ranked Probability Score (RPS) and "cumulative" log-loss.
Both are scoring rules, i.e. metrics to assess the accuracy of a probabilistic forecast, and designed for ordinal data: e.g. Lose < Draw < Win.
- the Brier Score (BS) and log-loss.
These are the categorical version of the previous scoring rules for categorical outcomes, but the ordinal metrics are to be preferred.
- calibration curves
- lift curves

## Brier summary of results and future directions

The model is successfully fit to the data and can gives valuable insights into the teams abilities.

When testing the predictive performance of the model, interestingly we find that the predictive performance becomes much better after the mid-season.

What I find particularly interesting is that we could predict how the predicted rank at the end of the season can change as more games are being played.

However, when trained on the full dataset, the parameters, notably the abilities, are still quite uncertain.
It does not seem to hurt much the predictions for the outcome of a game (lose, draw, win) as what mostly matters for this is how the abilities of one team compares to the abilities of the other team.
Still, the model does not accurately predict the number of goals scored.

In conclusion, this was a fun side project for me, a good way to work with "clean" data and learn about the Bayesian workflow.
However, at the moment of writing, I don't have the time or domain expertise to design a good enough model to be used for betting (and if the goal was betting, designing models for individual sports, or thing like darts or horse racing would probably be safer as the outcomes are less uncertain).
I nonetheless lay down a few ideas for improving the model in the [`Projects` tab of this repository](https://github.com/ghurault/football-prediction/projects/1).
