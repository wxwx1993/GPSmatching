# CausalGPS


| Resource    |  Github Actions      |  Code Coverage  |
| ----------  | -------------------- |-----------------|
| Platforms   | Windows, macOS, Linux|  codecov        |
| R CMD check | [![R build status](https://github.com/NSAPH-Software/CausalGPS/workflows/R-CMD-check/badge.svg?branch=develop)](https://github.com/NSAPH-Software/CausalGPS/actions) | [![codecov](https://codecov.io/gh/NSAPH-Software/CausalGPS/branch/develop/graph/badge.svg?token=97PCUXRGXH)](https://app.codecov.io/gh/NSAPH-Software/CausalGPS/) |

Matching on generalized propensity scores with continuous exposures

## Summary

`CausalGPS` is an R package that implements matching on generalized propensity scores with continuous exposures. The package introduces a novel approach for estimating causal effects using observational data in settings with continuous exposures, and a new framework for GPS caliper matching that jointly matches on both the estimated GPS and exposure levels to fully adjust for confounding bias.

## Installation

- Installing from source

```r
library("devtools")
install_github("NSAPH-Software/CausalGPS")
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

**`Y`** A data.frame comprised of two columns: one contains the observed outcome variable, and the other is labeled as 'id'. The column for the outcome variable can be assigned any name as per your requirements.    
 **`w`** A data.frame comprised of two columns: one contains the observed exposure variable, and the other is labeled as 'id'. The column for the outcome variable can be assigned any name as per your requirements.   
 **`c`**  A data.frame of includes observed covariate variables. It should also consist of a column named 'id'.
 **`ci_appr`** The causal inference approach. Possible values are:   
   - "matching": Matching by GPS   
   - "weighting": Weighting by GPS   
 **`gps_density`** Model type which is used for estimating GPS value, including
 normal (default) and kernel.   
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
 **`exposure_trim_qtls`** A numerical vector of two. Represents the trim quantile
 level. Both numbers should be in the range of [0,1] and in increasing order
 (default: c(0.01,0.99)).   
 **`gps_trim_qtls`** A numerical vector of two. Represents the trim quantile
 level. Both numbers should be in the range of [0,1] and in increasing order
 (default: c(0.01,0.99)). 
 **`params`** Includes list of params that is used internally. Unrelated
  parameters will be ignored.   
 **`sl_lib`**: A vector of prediction algorithms. 
 **`nthread`** An integer value that represents the number of threads to be
 used by internal packages.   
 **`...`**  Additional arguments passed to different models.

### Additional parameters   
#### Causal Inference Approach (`ci.appr`)   
 
 - if ci.appr = 'matching':   
   - *dist_measure*: Distance measuring function. Available options:   
     - l1: Manhattan distance matching   
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
   
- Generating Pseudo Population

```r
pseudo_pop <- generate_pseudo_pop(Y,
                                  w,
                                  c,
                                  ci_appr = "matching",
                                  gps_density = "normal",
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
                                  exposure_trim_qtls = c(0.01,0.99),
                                  max_attempt = 1,
                                  dist_measure = "l1",
                                  delta_n = 1,
                                  scale = 1)

```
`matching_fn` is Manhattan distance matching approach. For prediction model we use [SuperLearner](https://github.com/ecpolley/SuperLearner) package. SuperLearner supports different machine learning methods and packages. `params` is a list of hyperparameters that users can pass to the third party libraries in the SuperLearner package. All hyperparameters go into the params list.  The prefixes are used to distinguished parameters for different libraries. The following table shows the external package names, their equivalent name that should be used in `sl_lib`, the prefixes that should be used for their hyperparameters in the `params` list, and available hyperparameters. 

| Package name | `sl_lib` name | prefix| available hyperparameters |
|:------------:|:-------------:|:-----:|:-------------------------:|
| [XGBoost](https://xgboost.readthedocs.io/en/latest/index.html)| `m_xgboost` | `xgb_`|  nrounds, eta, max_depth, min_child_weight |
| [ranger](https://cran.r-project.org/package=ranger) |`m_ranger`| `rgr_` | num.trees, write.forest, replace, verbose, family |

`nthread` is the number of available threads (cores). XGBoost needs OpenMP installed on the system to parallel the processing. `use_covariate_transform` activates transforming covariates in order to achieve covariate balance. Users can pass custom function name in a list to be included in the processing. At each iteration, which is set by the users using `max_attempt`, the column that provides the worst covariate balance will be transformed.  

- Estimating GPS

```r
data_with_gps <- estimate_gps(w,
                              c,
                              gps_density = "normal",
                              params = list(xgb_nrounds = 50,
                                            xgb_max_depth = 6,
                                            xgb_eta = 0.3,
                                            xgb_min_child_weight = 1),
                              nthread = 1,                                
                              sl_lib = c("m_xgboost")
                              )

```

- Generating Pseudo Population with an available GPS object

```r
gps_obj <- estimate_gps(w,
                        c,
                        gps_density = "normal",
                        params = list(xgb_max_depth = c(3,4,5),
                                      xgb_nrounds=c(10,20,30,40,50,60)),
                        nthread = 1,
                        sl_lib = c("m_xgboost")
                        )

pseudo_pop <- generate_pseudo_pop(Y,
                                  w,
                                  c,
                                  ci_appr = "matching",
                                  gps_obj = gps_obj,
                                  use_cov_transform = TRUE,
                                  trim_quantiles = c(0.01,0.99),
                                  covar_bl_method = "absolute",
                                  covar_bl_trs = 0.1,
                                  covar_bl_trs_type = "mean",
                                  dist_measure = "l1",
                                  max_attempt = 1,
                                  delta_n = 1,
                                  scale = 0.5,
                                  nthread = 12)

```

- Estimating Exposure Rate Function

```r
estimate_npmetric_erf <- function(matched_Y,
                                  matched_w,
                                  matched_counter = NULL,
                                  bw_seq = seq(0.2,2,0.2),
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

## Code of Conduct

Please note that the CausalGPS project is released with a [Contributor Code of Conduct](https://www.contributor-covenant.org/version/2/1/code_of_conduct.html). By contributing to this project, you agree to abide by its terms.


## References

1. Wu, X., Mealli, F., Kioumourtzoglou, M.A., Dominici, F. and Braun, D., 2022. Matching on generalized propensity scores with continuous exposures. Journal of the American Statistical Association, pp.1-29.
