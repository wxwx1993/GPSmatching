# CausalGPS


| Resource    |  Github Actions      |  Code Coverage  |
| ----------  | -------------------- |-----------------|
| Platforms   | Windows, macOS, Linux|  codecov        |
| R CMD check | [![R build status](https://github.com/FASRC/CausalGPS/workflows/R-CMD-check/badge.svg)](https://github.com/fasrc/CausalGPS/actions) | [![codecov](https://codecov.io/gh/fasrc/CausalGPS/branch/develop/graph/badge.svg?token=97PCUXRGXH)](https://app.codecov.io/gh/fasrc/CausalGPS/) |

Matching on generalized propensity scores with continuous exposures

## Summary

An R package for implementing matching on generalized propensity scores with continuous exposures. We developed an innovative approach for estimating causal effects using observational data in settings with continuous exposures, and introduce a new framework for GPS caliper matching that jointly matches on both the estimated GPS and exposure levels to fully adjust for confounding bias.

## Installation

```r
library("devtools")
install_github("fasrc/CausalGPS")
library("CausalGPS")
```

## Usage

Input parameters:

`Y`: a vector of observed outcome  
`w`: a vector of observed continues exposure  
`c`: data frame or matrix of observed baseline covariates  
`matching_fun`: specified matching function  
`pred_model`: prediction model 
`gps_model`: model type for estimating GPS (parametric, non-parametric)
`bin_seq`: sequence of treatment (w) to generate pseudo population 
`scale`: specified scale parameter to control the relative weight that is attributed to the distance measures of the exposure versus the GPS estimates  
`delta_n`: specified caliper parameter on the exposure  
`sl_lib`: a set of machine learning methods used for estimating GPS  
`ci_appr`: causal inference approach   
`covar_bl_method`: specified covariate balance method  
`covar_bl_trs`: specified covariate balance threshold  
`max_attempt`: maximum number of attempt to satisfy covariate balance 
`use_cov_transform`: If `TRUE`, uses internal transformers to achieve covariate balance.
`transformers`: List of transformers (default: list("pow2","pow3")). Users can define a unary function and pass as transformer to the list.
`trim_quantiles`: a vector of two indicating upper and lower trimming quantiles (default: c(0.01, 0.99)). 

- Generating Pseudo Population

```r
pseudo_pop <- generate_pseudo_pop(Y,
                                  w,
                                  c,
                                  ci_appr = "matching",
                                  pred_model = "sl",
                                  gps_model = "parametric",
                                  use_cov_transform = TRUE,
                                  transformers = list("pow2", "pow3"),
                                  sl_lib = c("m_xgboost","SL.earth","SL.gam",
                                             "SL.ranger"),
                                  params = list(xgb_nrounds=c(10,20,30),
                                                xgb_eta=c(0.1,0.2,0.3)),
                                  nthread = 1,
                                  covar_bl_method = "absolute",
                                  covar_bl_trs = 0.1,
                                  trim_quantiles = c(0.01,0.99),
                                  max_attempt = 1,
                                  matching_fun = "matching_l1",
                                  delta_n = 1,
                                  scale = 0.5)

```
`matching_l1` is Manhattan distance matching approach. For prediciton model we use [SuperLearner](https://github.com/ecpolley/SuperLearner) package. User need to pass `sl` as `pred_model` to use SuperLearner package. SuperLearner supports different machine learning methods and packages. `params` is a list of hyperparameters that users can pass to the third party libraries in the SuperLearner package. All hyperparameters go into the params list.  The prefixes are used to distinguished parameters for different libraries. The following table shows the external package names, their equivalent name that should be used in `sl_lib`, the prefixes that should be used for their hyperparameters in the `params` list, and available hyperparameters. 

| Package name | `sl_lib` name | prefix| available hyperparameters |
|:------------:|:-------------:|:-----:|:-------------------------:|
| [XGBoost](https://xgboost.readthedocs.io/en/latest/index.html)| `m_xgboost` | `xgb_`|  nrounds, eta, max_depth, min_child_weight |
| [ranger](https://cran.r-project.org/package=ranger) |`m_ranger`| `rgr_` | num.trees, write.forest, replace, verbose, family |

`nthread` is the number of available threads (cores). XGBoost needs OpenMP installed on the system to parallize the processing. `use_covariate_transform` activates transforming covariates in order to achieve covariate balance. Users can pass custom function name in a list to be included in the processing. At each iteration, which is set by the users using `max_attempt`, the column that provides the worst covariate balance will be transformed.  

- Estimating GPS

```r
data_with_gps <- estimate_gps(Y,
                              w,
                              c,
                              pred_model = "sl",
                              gps_model = "parametric",
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
syn_data <- generate_syn_data(sample_size=1000,
                              seed = 403,
                              outcome_sd = 10,
                              gps_spec = 1,
                              cova_spec = 1)

```

## Contribution

For more information about reporting bugs and contribution, please read the contribution page from the package web page. 


## References

1. Wu, X., Mealli, F., Kioumourtzoglou, M.A., Dominici, F. and Braun, D., 2018. Matching on generalized propensity scores with continuous exposures. arXiv preprint arXiv:1812.06575. (https://arxiv.org/abs/1812.06575)
