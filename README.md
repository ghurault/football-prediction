# Football prediction model

<!-- badges: start -->
[![Lifecycle: dormant](https://img.shields.io/badge/lifecycle-dormant-blue.svg)](https://www.tidyverse.org/lifecycle/#dormant)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

This repository contains the code of a personal project where I am implementing a simple "Dixon-Coles" model to predict the outcome of football games in Stan, using publicly available [football data](http://football-data.co.uk/).

As a starting point, I would suggest looking at the notebook [`overview.Rmd`](overview.Rmd) summarising what I have done during this project, which one could access by clicking here: **[Getting started](https://ghurault.github.io/football-prediction/overview.nb.html)**.

A more detailed analysis, is conducted in the other files in this repository:

- Utility functions are available in [`functions.R`](functions.R).
- The script [`prior_pred_check.R`](prior_pred_check.R) includes the implementation of the prior predictive checks, fake data checks and the pipeline to generate and evaluate predictions.
- The model is fitted to real data in [`fitting.R`](fitting.R) and posterior predictive checks are performed.
- The predictive performance of the model is assessed in [`validation.R`](validation.R).
