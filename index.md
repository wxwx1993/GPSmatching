# CausalGPS  <a href="https://fasrc.github.io/GPSmatching/"><img src="man/figures/png/causalgps_logo_01.png" align="right" height="115" /></a>

<!-- badges: start -->
[![R build status](https://github.com/fasrc/CausalGPS/workflows/R-CMD-check/badge.svg)](https://github.com/fasrc/CausalGPS/actions)
<!-- badges: end -->


## Summary

An R package for implementing matching, and weighting on generalized propensity scores with continuous exposures. We developed an innovative approach for estimating causal effects using observational data in settings with continuous exposures, and introduce a new framework for GPS caliper matching that jointly matches on both the estimated GPS and exposure levels to fully adjust for confounding bias.

## Features

### Estimating GPS

The generalized propensity scores are computed using various parametric and/or non-parametric models. The generalized propensity scores are used for the following causal inference approaches. 

### Generating Pseudo Population

Pseudo population dataset is computed based on user-defined causal inference approaches (e.g., matching or weighting). A covariate balance test is performed on the pseudo population dataset. Users can specify covariate balance criteria and activate an adaptive approach and number of attempts to search for a target pseudo population dataset that meets the covariate balance criteria.

###  Outcome Models

Several outcome models can be achieved using the generated pseudo population dataset. Users can specify non-/semi-parametric models to obtain exposure-response curves and parametric models to obtain regression coefficients of interest.
