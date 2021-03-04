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

`Y`: a vector of observed outcome  
`w`: a vector of observed continues exposure  
`c`: data frame or matrix of observed baseline covariates  
`matching_fun`: specified matching function  
`scale`: specified scale parameter to control the relative weight that is attributed to the distance measures of the exposure versus the GPS estimates  
`delta_n`: specified caliper parameter on the exposure  
`sl_lib`: a set of machine learning methods used for estimating GPS  
`ci_appr`: causal inference approach   
`running_appr`: running approach (base, parallel)  
`covar_bl_method`: specified covariate balance method  
`covar_bl_trs`: specified covariate balance threshold  
`max_attempt`: maximum number of attempt to satisfy covariate balance  

- Generating Pseudo Population

```r
pseudo_pop <- gen_pseudo_pop(Y,
                             w,
                             c,
                             ci_appr = "matching",
                             running_appr = "base",
                             pred_model = "sl",
                             sl_lib = c("m_xgboost","SL.earth","SL.gam",
                                        "SL.ranger"),
                             params = list(xgb_nrounds=c(10,20,30),
                                           xgb_eta=c(0.1,0.2,0.3)),
                             nthread = 1,
                             covar_bl_method = "absolute",
                             covar_bl_trs = 0.1,
                             max_attempt = 1,
                             matching_fun = "MatchingL1",
                             delta_n = 1,
                             scale = 0.5)

```
`MatchingL1` is Manhattan distance matching approach. `sl` uses SuperLearner package to train the prediction model. `params` is a list of hyperparameters. All hyperparameters go into the params list.  The prefixes are used to distinguished parameters for different libraries. The following table shows the external package names, their equivalent name that should be used in `sl_lib`, the prefixes that should be used for their hyperparameters in the `params` list, and available hyperparameters. 

| Package name | `sl_lib` name | prefix| available hyperparameters |
|:------------:|:-------------:|:-----:|:-------------------------:|
| [XGBoost](https://xgboost.readthedocs.io/en/latest/index.html)| `m_xgboost` | `xgb_`|  nrounds, eta, max_depth, min_child_weight |

`nthread` is the number of available threads (cores). XGBoost needs OpenMP installed on the system to parallize the processing.

- Estimating GPS

```r
data_with_gps <- estimate_gps(Y,
                              w,
                              c,
                              pred_model = "sl",
                              running_appr = "base",
                              internal_use = FALSE,
                              params = list(xgb_max_depth = c(3,4,5),
                                            xgb_rounds = c(10,20,30,40)),
                              nthread = 1,                                
                              sl_lib = c("m_xgboost")
                              )

```

If `internal_use` is set to be TRUE, the program will return additional vectors to be used by the selected causal inference approach to generate a pseudo population. See `?estimate_gps` for more details. 

- Estimating Exposure Rate Function

```r
erf <- estimate_erf(Y,
                    w,
                    bw_seq,
                    w_vals)
```

- Generating Synthetic Data

```r
syn_data <- GenSynData(sample_size=1000,
                       seed = 403,
                       outcome_sd = 10,
                       gps_spec = 1,
                       cova_spec = 1)

```

## Contribution

For more information about reporting bugs and contribution, please read [Contribution Page](inst/misc/developer_manual.md).

## References

1. Wu, X., Mealli, F., Kioumourtzoglou, M.A., Dominici, F. and Braun, D., 2018. Matching on generalized propensity scores with continuous exposures. arXiv preprint arXiv:1812.06575. (https://arxiv.org/abs/1812.06575)
