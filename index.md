# CausalGPS  <a href="https://fasrc.github.io/CausalGPS/"><img src="man/figures/png/causalgps_logo_01.png" align="right" width="120" /></a>

<!-- badges: start -->
[![R build status](https://github.com/fasrc/CausalGPS/workflows/R-CMD-check/badge.svg)](https://github.com/fasrc/CausalGPS/actions)
[![](https://www.r-pkg.org/badges/version-last-release/CausalGPS)](https://cran.r-project.org/package=CausalGPS)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/CausalGPS)](http://www.r-pkg.org/pkg/causalgps)
[![codecov](https://codecov.io/gh/fasrc/CausalGPS/branch/develop/graph/badge.svg?token=97PCUXRGXH)](https://app.codecov.io/gh/fasrc/CausalGPS/)
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/NSAPH/R_project_binder/master?urlpath=rstudio)
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

#### Acknowledgments

Funding was provided by the Health Effects Institute grant 4953-RFA14-3/16-4, Environmental Protection Agency grant 83587201-0, National Institute of Health grants R01 ES026217, R01 MD012769, R01 ES028033, 1R01 ES030616, 1R01 AG066793-01R01, 1R01 ES029950, R01 ES028033-S1, Alfred P. Sloan Foundation grant G-2020-13946.
