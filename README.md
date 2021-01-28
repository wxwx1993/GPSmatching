# GPSmatching


| Resource    |  Github Actions      |
| ----------  | -------------------- |
| Platforms   | Windows, macOS, Linux|
| R CMD check | [![R build status](https://github.com/Naeemkh/GPSmatching/workflows/R-CMD-check/badge.svg)](https://github.com/Naeemkh/GPSmatching/actions) |
| Test Coverage | [![codecov](https://codecov.io/gh/fasrc/GPSmatching/branch/master/graph/badge.svg)](https://codecov.io/gh/Naeemkh/GPSmatching) |



Matching on generalized propensity scores with continuous exposures

## Summary

An R package for implementing matching on generalized propensity scores with continuous exposures. We developed an innovative approach for estimating causal effects using observational data in settings with continuous exposures, and introduce a new framework for GPS caliper matching that jointly matches on both the estimated GPS and exposure levels to fully adjust for confounding bias.

## Installation
```r
library("devtools")
install_github("wxwx1993/GPSmatching")
library("GPSmatching")
```

## Example
Let Y denote a vector of observed outcome; w denote a vector of observed continuous exposure; c denote a data frame or matrix of observed baseline covariates.
matching_fun is a specifed matching function (Default is "matching_l1" (Manhattan distance matching)).
scale is a specified scale parameter to control the relative weight that is attributed to the distance measures of the exposure versus the GPS estimates (Default is 0.5).
delta_n is a specified caliper parameter on the exposure (Default is 1).
sl.lib is a set of machine learning methods used for estimating GPS (Default is ("SL.xgboost","SL.earth","SL.gam","SL.ranger")).
```r
matched_set = create_matching(Y,
                              w,
                              c,
                              matching_fun = matching_l1,
                              sl.lib = c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),
                              scale = 0.5,
                              delta_n=1)
erf = matching_smooth(matched_Y = matched_set$Y,
                      matched_w = matched_set$w,
                      bw.seq = seq(0.2,2,0.2),
                      w.vals = seq(min(w),max(w),length.out = 100))
```
        
## Code
create_matching is functions for creating matched set using GPS matching approaches.

absolute_corr_fun is functions for checking covariate balance based on absolute correlations for given data sets.

matching_smooth is functions for estimating smoothed exposure-response function (ERF).

## Contribution

For more information about contribution, please read [Contribution Page](inst/misc/developer_manual.md).

## References

1. Wu, X., Mealli, F., Kioumourtzoglou, M.A., Dominici, F. and Braun, D., 2018. Matching on generalized propensity scores with continuous exposures. arXiv preprint arXiv:1812.06575. (https://arxiv.org/abs/1812.06575)
