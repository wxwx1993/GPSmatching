
## CausalGPS 0.2.5 (2021-07-09)

### Added

### Changed

* User needs to activate the logger

### Fixed

* CRAN package URLs are in canonical forms.

### Removed

## CausalGPS 0.2.4 (2021-07-11)

### Added

* OpenMP for Rcpp code
* optimized_compile
* log_system_info()
* Frequently asked questions
* logo 

### Changed

* estimate_gps.Rmd
* estimate_semi_erf -> estimate_semipmetric_erf
* estimate_erf -> estimate_npmetric_erf
* estimate_hr -> estimate_pmetric_erf
* gen_pseudo_pop -> generate_pseudo_pop
* gen_syn_data -> generate_syn_data
* estimate_erf accepts counter as an input
* estimate_erf can use multiple cores
* generating_pseudo_population.Rmd
* estimate_erf function description
* estimate_hr function description
* estimate_semi_erf function description
* compute_risk function description and return value
* outcome_models.Rmd
* generate_synthetic_data.Rmd


### Fixed

* Rcpp parLapply worker processors arguments

### Removed

* running_appr


## CausalGPS 0.2.3 (2021-05-12)

### Fixed

*  Fixed documentations

## CausalGPS 0.2.2 (2021-05-12)

### Added

* estimate_semi_erf
* estimate_hr

### Changed

* Package name: GPSmatching --> CausalGPS 

### Fixed
### Removed


## GPSmatching 0.2.1 (2021-04-23)

### Added

*  User defined bin sequence in compiling speudo population
*  Non-parametric option for estimating gps
*  Adaptive approach to transform features in training sessions.
*  Cpp code for computing pair of w and gps.
*  `set_logger` function.
*  Customized wrapper for ranger package.
*  Extended plot function for gen_pseudo_pop object (plot.R)
*  Extended plot function for estimate_erf object (plot.R)
*  Extended print function for estimate_erf object (print.R)
*  test-estimate_erf.R
*  create_weighting.R
*  Steps for adding test data into 'sysdata.rda'.
*  `weighting` option as causal inference approach.  
*  absolute_weighted_corr_fun.R
*  Testing and running example guidelines for developers
*  Customized wrapper for xgboost package.
*  `param` as an argument to accept hyperparameters from users.


### Changed

* R dependency 2.7 --> 3.5
* mclapply --> parLapply
* estimate_erf output returns S3 object.
* test-Covariate_balance.R --> test-absolute_corr_fun.R
* covariate_balance.R --> absolute_corr_fun.R
* User needs to pass `m_xgboost` instead of `SL.xgboost` to  use XGBoost package for prediction purposes.

### Fixed

* mclapply memory issue (compute_closest_wgps.R).

### Removed


## GPSmatching 0.2.0 (2021-03-01)

### Added

* Covariate balance check for categorical data.
* Contribution guidelines
* Parallel flag in training models (`mcSuperLearner`)
* gen_syn_data function for generating synthetic data
* Unittest for gen_syn_data
* Function to compute residuals and unittest
* Function to impute NA values based on density and unittest
* Function to separate prediction model training (train_it)
* Function to separate min and max value estimation and unittest
* Function to find the closest data based on gps and w
* Wrapper function to generate pseudo population and test it for covariate balance (gen_pseudo_pop)
* Function to estimate only GPS value (estimate_gps)
* Helper function to take the input data + GPS values and return pseudo population based on selected causal inference approach. The output of this function may or may not satisfy the covariate balance test. (compile_pseudo_pop)
* check_args function to check availability of the required parameters.
* check_covar_balance function to check if the generated pseudo population statistically acceptable.
* create_matching function to generate pseudo population based on matching approach.

### Changed

* create_matching only generates matched dataset.
* Covariate_balance.R --> covariate_balance.R
* matching_smooth --> estimate_erf.R
* risk_fun --> compute_risk
* smooth_fun --> smooth_erf
* hatvals --> estimate_hat_vals
* kernel_fun --> generate_kernel
* GPSmatching-package.R --> gpsmatching_package.R
* GPSmatching_smooth.R --> gpsmatching_smooth.R

### Fixed

* None

### Removed

* GPSmatching.R functions are separated into smaller functions, and the file is removed.
