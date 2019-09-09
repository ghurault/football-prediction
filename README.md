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
- [ ] Evaluate model with (cumulative?) LogLoss, calibration curves, lift curves
- 

### Modelling

- [x] Poisson regression with latent attack/defence abilities
- [ ] Model half-time goals (i.e. double the data)
- [ ] Different sigmas for attack and defence?
- [ ] Parametrisation with total ability and proportion of attack/defence (convenient if model feedback after half time)?
- [ ] Exponential smoothing or Random walk to allow abilities to change
- [ ] Variability in home advantage: home team dependent? Can potentially interact with away team (some team might less susceptible to be affected by "away" disadvantage), something like `HomeAdvantage * AwaySusceptibility`.
- 

## Notes

- Difference of two Poisson distributed random variables follow a Skellam distribution
- 

