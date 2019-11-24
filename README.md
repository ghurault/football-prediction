# Football prediction model

<!-- badges: start -->
[![Lifecycle: dormant](https://img.shields.io/badge/lifecycle-dormant-blue.svg)](https://www.tidyverse.org/lifecycle/#dormant)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Models for predicting the outcomes of football games in Stan, using publicly available [football data](http://football-data.co.uk/).

## Model

Here, I implemented a simple Dixon-Coles model where we assume that the number of goals scored by each team follow independent Poisson distributions.

For each game, if we index the home team by `h` and the away team by `a`, then the rates of the Poisson distribution are given by:

<img src="https://latex.codecogs.com/gif.latex?$$\log(\lambda_h)&space;=&space;b&space;&plus;&space;\mathit{advtg}&space;&plus;&space;\mathit{attack_h}&space;-&space;\mathit{defence_a}&space;$$" title="$$\log(\lambda_h) = b + \mathit{advtg} + \mathit{attack_h} - \mathit{defence_a} $$" />

<img src="https://latex.codecogs.com/gif.latex?$$\log(\lambda_a)&space;=&space;b&space;&plus;&space;\mathit{attack_a}&space;-&space;\mathit{defence_h}&space;$$" title="$$\log(\lambda_a) = b + \mathit{attack_a} - \mathit{defence_h} $$" />

Where `b` is the intercept (logarithm of the average goals rate assuming the attack and defence abilities of the teams cancels out), `advtg` is the home advantage, and `attack` and `defence` are the vectors containing the latent attack and defence abilities for each team.

The model is available in `Models/mdl1.stan`.

## Workflow

For the moment, I have mainly focused on preparing the workflow for evaluating the models.
Notably, I implemented functions to look at the posterior, prior or predictive (including games already played and games to play) distribution for:

- the number of games won
- the number of games lost
- the number of games ending in a draw
- the total number of goals scored
- the goal difference
- the number of points
- the ranking

I have also written functions to evaluate the quality of the predictions (whether the predictions are for the outcome of a game or the number of goals scored during the game) by looking at:

- the Ranked Probability Score (RPS) and "cumulative" log-loss (cf. ordinal outcomes)
- the Brier Score (BS) and log-loss (cf. categorical outcomes)
- calibration curves
- lift curves

## Repository roadmap

Utility functions are available in `functions.R`

At the moment, the script `prior_pred_check.R` is the most complete one.
It implements prior predictive check, fake data check as well as the pipeline to evaluate predictions.
The script notably tests all the functions on simulated data.

The model is fitted to real data in `fitting.R` and posterior predictive check is performed.

A validation script implementing forward chaining to evaluate the predictive performance of the model will be available soon.

In addition, I lay down a few ideas for model improvement in the `Projects` tab of this repository.

