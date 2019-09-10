# Football prediction model

Models for predicting the outcomes of football games.

Publicly available [football data](http://football-data.co.uk/).

## TO DO

### Workflow

- [x] Prior predictive check
- [x] Fake data check
- [ ] Functions for model checking
- [x] Statistics for posterior predictive check
- [ ] Functions for posterior predictive check
- [ ] Include "test" in generated quantities: have a vector of all possibles games, feed played games and predict unplayed games so that we can update football statistics as more data comes in.
- [ ] Evaluate model in forward chaining
- [ ] Evaluate model with LogLoss (not cumulative, don't care about that for betting), calibration curves, lift curves
- 

### Modelling

- [x] Poisson regression with latent attack/defence abilities
- [ ] Model half-time goals (i.e. double the data)
- [ ] Different sigmas for attack and defence?
- [ ] If probability of draw underconfident (or underestimate 0 goals), could use zero-inflated model or activation function for (attack - defence) (pool toward 0, useful if probabily of draw underconfident)
- [ ] Exponential smoothing or Random walk to allow abilities to change
- [ ] Parametrisation with total ability and proportion of attack/defence (convenient if model feedback after half time)?
- [ ] Variability in home advantage: home team dependent? Can potentially interact with away team (some team might less susceptible to be affected by "away" disadvantage), something like `HomeAdvantage * AwaySusceptibility`.
- [ ] Prior abilities as data to leverage knowledge from previous seasons (not ideal but might be sufficient)?
- 

Example activation function:
```
inv_logit <- function(x) {1 / (1 + exp(-x))}
slp <- 1 # slope at midpoint
inf <- 5 # inflexion point
a <- 4 * slp # slope
b <- -4 * slp * inf - 2 # intercept

x <- seq(-10, 10, .01)
plot(x, (inv_logit(a * x + b) + inv_logit(-a * x + b)) * x, type = "l")
abline(c(0, 1), col = "red")
abline(c(0, 0), col = "red")
```

## Notes

- Difference of two Poisson distributed random variables follow a Skellam distribution
- 

