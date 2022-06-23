# CausalGPS


| Resource    |  Github Actions      |  Code Coverage  |
| ----------  | -------------------- |-----------------|
| Platforms   | Windows, macOS, Linux|  codecov        |
| R CMD check | [![R build status](https://github.com/FASRC/CausalGPS/workflows/R-CMD-check/badge.svg)](https://github.com/fasrc/CausalGPS/actions) | [![codecov](https://codecov.io/gh/fasrc/CausalGPS/branch/develop/graph/badge.svg?token=97PCUXRGXH)](https://app.codecov.io/gh/fasrc/CausalGPS/) |

Matching on generalized propensity scores with continuous exposures

## Summary

An R package for implementing matching on generalized propensity scores with continuous exposures. We developed an innovative approach for estimating causal effects using observational data in settings with continuous exposures, and introduce a new framework for GPS caliper matching that jointly matches on both the estimated GPS and exposure levels to fully adjust for confounding bias.

## Installation

- Installing from source

```r
library("devtools")
install_github("fasrc/CausalGPS")
library("CausalGPS")
```

- Installing from CRAN 

```r
install.packages("CausalGPS")
```

- Setting up docker environment

Developing Docker image can be downloaded from Docker Hub. See more details in docker_singularity.

## Usage

Input parameters:

**`Y`** A vector of observed outcome variable.     
 **`w`** A vector of observed continuous exposure variable.   
 **`c`** A data.frame or matrix of observed covariates variable.   
 **`ci_appr`** The causal inference approach. Possible values are:   
   - "matching": Matching by GPS   
   - "weighting": Weighting by GPS   
 **`pred_model`** a prediction model (use "sl" for SuperLearner)   
 **`gps_model`** Model type which is used for estimating GPS value, including
 parametric (default) and non-parametric.   
 **`use_cov_transform`** If TRUE, the function uses transformer to meet the
  covariate balance.   
 **`transformers`** A list of transformers. Each transformer should be a
 unary function. You can pass name of customized function in the quotes.   
 Available transformers:   
   - pow2: to the power of 2   
   - pow3: to the power of 3   
 **`bin_seq`** Sequence of w (treatment) to generate pseudo population. If
 NULL is passed the default value will be used, which is
 `seq(min(w)+delta_n/2,max(w), by=delta_n)`.   
 **`trim_quantiles`** A numerical vector of two. Represents the trim quantile
 level. Both numbers should be in the range of [0,1] and in increasing order
 (default: c(0.01,0.99)).   
 **`optimized_compile`** If TRUE, uses counts to keep track of number of
 replicated pseudo population.   
 **`params`** Includes list of params that is used internally. Unrelated
  parameters will be ignored.   
 **`nthread`** An integer value that represents the number of threads to be
 used by internal packages.   
 **`...`**  Additional arguments passed to different models.

### Additional parameters   
#### Causal Inference Approach (`ci.appr`)   
 
 - if ci.appr = 'matching':   
   - *matching_fun*: Matching function. Available options:   
     - matching_l1: Manhattan distance matching   
   - *delta_n*: caliper parameter.   
   - *scale*: a specified scale parameter to control the relative weight that
  is attributed to the distance measures of the exposure versus the GPS.   
   - *covar_bl_method*: covariate balance method. Available options:   
      - 'absolute'   
   - *covar_bl_trs*: covariate balance threshold   
   - *covar_bl_trs_type*: covariate balance type (mean, median, maximal)   
   - *max_attempt*: maximum number of attempt to satisfy covariate balance.   
   - See [create_matching()] for more details about the parameters and default
   values.   
 - if ci.appr = 'weighting':   
   - *covar_bl_method*: Covariate balance method.   
   - *covar_bl_trs*: Covariate balance threshold   
   - *max_attempt*: Maximum number of attempt to satisfy covariate balance.
   
#### Prediction models (pred_model)   
 - if pred_model = 'sl':   
   - *sl_lib*: A vector of prediction algorithms.   

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
                                  sl_lib = c("m_xgboost"),
                                  params = list(xgb_nrounds = 50,
                                                xgb_max_depth = 6,
                                                xgb_eta = 0.3,
                                                xgb_min_child_weight = 1),
                                  nthread = 1,
                                  covar_bl_method = "absolute",
                                  covar_bl_trs = 0.1,
                                  trim_quantiles = c(0.01,0.99),
                                  optimized_compile = TRUE,
                                  max_attempt = 1,
                                  matching_fun = "matching_l1",
                                  delta_n = 1,
                                  scale = 1)

```
`matching_l1` is Manhattan distance matching approach. For prediction model we use [SuperLearner](https://github.com/ecpolley/SuperLearner) package. User need to pass `sl` as `pred_model` to use SuperLearner package. SuperLearner supports different machine learning methods and packages. `params` is a list of hyperparameters that users can pass to the third party libraries in the SuperLearner package. All hyperparameters go into the params list.  The prefixes are used to distinguished parameters for different libraries. The following table shows the external package names, their equivalent name that should be used in `sl_lib`, the prefixes that should be used for their hyperparameters in the `params` list, and available hyperparameters. 

| Package name | `sl_lib` name | prefix| available hyperparameters |
|:------------:|:-------------:|:-----:|:-------------------------:|
| [XGBoost](https://xgboost.readthedocs.io/en/latest/index.html)| `m_xgboost` | `xgb_`|  nrounds, eta, max_depth, min_child_weight |
| [ranger](https://cran.r-project.org/package=ranger) |`m_ranger`| `rgr_` | num.trees, write.forest, replace, verbose, family |

`nthread` is the number of available threads (cores). XGBoost needs OpenMP installed on the system to parallel the processing. `use_covariate_transform` activates transforming covariates in order to achieve covariate balance. Users can pass custom function name in a list to be included in the processing. At each iteration, which is set by the users using `max_attempt`, the column that provides the worst covariate balance will be transformed.  

- Estimating GPS

```r
data_with_gps <- estimate_gps(Y,
                              w,
                              c,
                              pred_model = "sl",
                              gps_model = "parametric",
                              internal_use = FALSE,
                              params = list(xgb_nrounds = 50,
                                            xgb_max_depth = 6,
                                            xgb_eta = 0.3,
                                            xgb_min_child_weight = 1),
                              nthread = 1,                                
                              sl_lib = c("m_xgboost")
                              )

```

If `internal_use` is set to be TRUE, the program will return additional vectors to be used by the selected causal inference approach to generate a pseudo population. See `?estimate_gps` for more details. 

- Estimating Exposure Rate Function

```r
estimate_npmetric_erf<-function(matched_Y,
                                matched_w,
                                matched_counter = NULL,
                                bw_seq=seq(0.2,2,0.2),
                                w_vals,
                                nthread)
```

- Generating Synthetic Data

```r
syn_data <- generate_syn_data(sample_size=1000,
                              outcome_sd = 10,
                              gps_spec = 1,
                              cova_spec = 1)

```

## Contribution

For more information about reporting bugs and contribution, please read the contribution page from the package web page. 


## References

1. Wu, X., Mealli, F., Kioumourtzoglou, M.A., Dominici, F. and Braun, D., 2018. Matching on generalized propensity scores with continuous exposures. arXiv preprint arXiv:1812.06575. (https://arxiv.org/abs/1812.06575)
