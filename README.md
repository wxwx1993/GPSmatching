# GPSmatching
## The R Package for the manuscript "Matching on generalized propensity scores with continuous exposures".
An R package for implementing matching on generalized propensity scores with continuous exposures. We developed an innovative approach for estimating causal effects using observational data in settings with continuous exposures, and introduce a new framework for GPS caliper matching that jointly matches on both the estimated GPS and exposure levels to fully adjust for confounding bias.

## Example
Denote Y as a vector of observed outcome; w a vector of observed continuous exposure; c a data frame or matrix of observed baseline covariates.
matching_fun is a specifed matching function (Default is "matching_l1" (Manhattan distance matching)).
scale is a specified scale parameter to control the relative weight that is attributed to the distance measures of the exposure versus the GPS estimates (Default is 0.5).
delta_n is a specified caliper parameter on the exposure (Default is 1).
sl.lib is a set of machine learning methods used for estimating GPS (Default is ("SL.xgboost","SL.earth","SL.gam","SL.ranger")).
```r
matached_set = create_matching(Y,
                               w,
                               c,
                               matching_fun=matching_l1,
                               sl.lib = c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),
                               scale=0.5,
                               delta_n=1)
erf = matching_smooth<-function(matached_set$Y,
                                matached_set$w,
                                bw.seq=seq(0.2,2,0.2),
                                w.vals)
```
        
## Code
create_matching is functions for creating matched set using GPS matching approaches.

absolute_corr_fun is functions for checking covariate balance based on absolute correlations for given data sets.

matching_smooth is functions for estimating smoothed exposure-response function (ERF).

References: 
1. Wu, X., Mealli, F., Kioumourtzoglou, M.A., Dominici, F. and Braun, D., 2018. Matching on generalized propensity scores with continuous exposures. arXiv preprint arXiv:1812.06575. (https://arxiv.org/abs/1812.06575)

