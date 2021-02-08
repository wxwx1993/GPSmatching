# GPSmatching


| Resource    |  Github Actions      |
| ----------  | -------------------- |
| Platforms   | Windows, macOS, Linux|
| R CMD check | [![R build status](https://github.com/Naeemkh/GPSmatching/workflows/R-CMD-check/badge.svg)](https://github.com/Naeemkh/GPSmatching/actions) |




Matching on generalized propensity scores with continuous exposures

## Summary

An R package for implementing matching on generalized propensity scores with continuous exposures. We developed an innovative approach for estimating causal effects using observational data in settings with continuous exposures, and introduce a new framework for GPS caliper matching that jointly matches on both the estimated GPS and exposure levels to fully adjust for confounding bias.

## Installation
```r
library("devtools")
install_github("fasrc/GPSmatching")
library("GPSmatching")
```

## Usage

Input parameters:

**Y**: a vector of observed outcome  
**w**: a vector of observed continues exposure  
**c**: data frame or matrix of observed baseline covariates  
**matching.fun**: specified matching function  
**scale**: specified scale parameter to control the relative weight that is attributed **to the distance measures of the exposure versus the GPS estimates  
**delta.n**: specified caliper parameter on the exposure  
**sl.lib**: a set of machine learning methods used for estimating GPS  
**ci.appr**: causal inference approach  
**covar.bl.method**: specified covariate balance method  
**covar.bl.trs**: specified covariate balance threshold  
**max.attempt**: maximum number of attempt to satisfy covariate balance  

- Generating Pseudo Population

```r
pseuodo.pop <- GenPseudoPop(Y,
                            w,
                            c,
                            ci.appr = "matching",
                            pred.model = "sl",
                            sl.lib = c("SL.xgboost","SL.earth","SL.gam",
                                       "SL.ranger"),
                            covar.bl.method = "absolute",
                            covar.bl.trs = 0.1,
                            max.attempt = 1,
                            matching.fun = "MatchingL1",
                            delta.n = 1,
                            scale = 0.5)

```
`MatchingL1` is Manhattan distance matching approach. `sl` uses SuperLearner package to train the prediction model.

- Estimating GPS

```r
data.with.gps <- EstimateGPS(Y,
                             w,
                             c,
                             pred.model = "sl",
                             internal.use = FALSE,
                             sl.lib = c("SL.xgboost","SL.earth","SL.gam",
                                       "SL.ranger")
                             )

```

If `internal.use` is set to be TRUE, the program will return additional vectors to be used by selected causal inference approach to generate pseudo population. See `?EstimateGPS` for more details.

- Estimating Exposure Rate Function

```r
erf <- EstimateERF(Y,
                   w,
                   bw.seq,
                   w.vals)
```

- Generating Synthetic Data

```r
syn.data <- GenSynData(sample.size=1000,
                       seed = 403,
                       outcome.sd = 10,
                       gps.spec = 1,
                       cova.spec = 1)

```

## Contribution

For more information about reporting bugs and contribution, please read [Contribution Page](inst/misc/developer_manual.md).

## References

1. Wu, X., Mealli, F., Kioumourtzoglou, M.A., Dominici, F. and Braun, D., 2018. Matching on generalized propensity scores with continuous exposures. arXiv preprint arXiv:1812.06575. (https://arxiv.org/abs/1812.06575)
