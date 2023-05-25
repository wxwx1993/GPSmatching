## CausalGPS 0.4.0 (2023-05-26)

### Changed

* Docker image supports R 4.2.3
* `generate_syn_data` supports `vectorized_y` to accelerate data generation.
* `matching_fun` --> `dist_measure`
* `matching_l1` --> `matching_fn`
* `estimate_semipmetric_erf` now takes the `gam` models optional arguments.
* `estimate_pmetric_erf` now takes the `gnm` models optional arguments.
* `trim_quantiles` --> `exposure_trim_qtls`
* `generate_pseudo_pop` function accepts `gps_obj` as an optional input.
* `internal_use` is not part of parameters for `estimate_gps` function. 
* `estimate_gps` function only returns `id`, `w`, and computed `gps` as part of dataset.
* Now the design and analysis phases are explicitly separated.
* `gps_model` --> `gps_density`. Now it takes, `normal` and `kernel` options instead of `parametric` and `non-parametric` options.


### Added

* `estimate_npmetric_erf` supports both `locpol` and `KernSmooth` approaches.
* There is `gps_trim_qtls` input parameter to trim data samples based on gps values.
* Now users can also collect the original data in the pseudo population object.

### Fixed

* A bug with swapping transformed covairates with original one.

## CausalGPS 0.3.1 (2023-05-15)

### Changed

* Some of unit tests have less accuracy to overcome the bug with `stats::density` function. 

## CausalGPS 0.3.0 (2023-02-15)

### Changed

* Unit tests support new `wCorr` release (#193).
* Only optimized compilation is supported. In the previous versions, this approach is known as `optimzied_compile == TRUE`.

### Added

* The `earth` package is part of suggested packages. 


## CausalGPS 0.2.9 (2022-12-16)

### Fixed

* fixed a bug based on covariate balance threshold (#178, @naeemkh). 
* `estimate_npmetric_erf` assigns user-defined log file.

### Changed
* The process now prints the progress message based on the selected thresholds.
* In `estimate_npmetric_erf`:
  * `matched_Y` --> `m_Y`
  * `matched_w` --> `m_w`
  * `matched_cw` --> `counter_weight`
* In `estimate_npmetric_erf` function, the `matched_cw` input is now mandatory. 
* Internal kernel smoothing now uses `locpol::locpol` function.
* The entire data set is trimmed based on trimming quantiles.
* `earth` and `ranger` are not installed automatically. They can be installed manually if needed.
* `sysdata.rda` is modified to reflect transition from `counter` and `ipw` to `counter_weight`
* `counter_weight` is used as a counter or weight, in `matching` or `weighting`
approaches. `counter` and `ipw` are dropped.
* `sl_lib` becomes a required argument.
* The package has been transferred into NSAPH-Software Github account.
* Summary function of `gpsm_pspop` S3 object returns details of the adjusting process. 


### Added
* Now `Kolmogorov-Smirnov(KS)` statistics are provided for the computed pseudo population.
* `effect size` for the generated pseudo population is computed and reported.
* Binary search approach is used when scale = 1.
* `pseodo_pop` also includes covariate column names.
* `compute_closest_wgps_helper_no_sc` is added to take care of the mostly used special case (scale = 1).

### Removed 

* Dropped importing `KernSmooth` and `tidyr` packages. 
* `pred_model` argument dropped. The package only predicts using SuperLearner.


## CausalGPS 0.2.8 (2022-06-22)

### Fixed
* Message for not implemented methods changed to reduce misunderstanding.
* Empty counter will raise error in estimating non-parametric response function. 

### Changed
* matching_l1 returns frequency table instead of entire vector. 
* Vectorized population compilation and used data.table for multi-thread assignment.
* Removed nested parallelism in compiling pseudo population, which results in close control on memory.
* estimate_npmetric_erf also returns optimal h and risk values.

### Added
* `estimate_gps` returns the optimal hyperparameters.
* `estimate_gps` returns S3 object. 
* Internal xgboost approach support `verbose` parameter.
* Pseudo-population object now report the parameters that are used for the best covariate balance.


## CausalGPS 0.2.7 (2022-02-04)

### Fixed
* Naming covariate balance scores.

### Changed
* Restarting adaptive approach to keep trying up to maximum attempt.

### Added
* Synthetic data (synthetic_us_2010)
* Check on not defined covariate balance (absolute_corr_fun, absolute_weighted_corr_fun)
* Covariate balance threshold type: mean, median, maximal. 
* Improved test coverage.
* Singularity definition file.

## CausalGPS 0.2.6 (2021-09-06)

### Added
* added the status of optimized compile to generate_pseudo_pop function output.
* compute_closest_wgps accepts the number of user-defined threads.

### Changed

* Vignette file names.
* The trim condition from > and < into >= and <=.
* Removed seed input from generate_syn_data function. In R package, setting seed value inside function is not recommended. Users can set the seed before using the function. 
* OpenMP uses user defined number of cores.

### Fixed

* Initial covariate balance for weighted approach. The counter column was not preallocated correctly.
* Counter value for compiling. The initial value was set to one, which, however, zero is the correct one. 
* Private variable issue with OpenMP.
* Fixed OpenMP option on macOS checks. 

### Removed

## CausalGPS 0.2.5 (2021-07-23)

### Changed

* User needs to activate the logger

### Fixed

* CRAN package URLs are in canonical forms.

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

## GPSmatching 0.2.1 (2021-04-23)

### Added

*  User defined bin sequence in compiling pseudo population.
*  Non-parametric option for estimating GPS.
*  Adaptive approach to transform features in training sessions.
*  Cpp code for computing pair of w and GPS.
*  `set_logger` function.
*  Customized wrapper for ranger package.
*  Extended plot function for gen_pseudo_pop object (plot.R).
*  Extended plot function for estimate_erf object (plot.R).
*  Extended print function for estimate_erf object (print.R).
*  test-estimate_erf.R.
*  create_weighting.R.
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
* Function to find the closest data based on GPS and w
* Wrapper function to generate pseudo population and test it for covariate balance (gen_pseudo_pop)
* Function to estimate only GPS value (estimate_gps)
* Helper function to take the input data + GPS values and return pseudo population based on selected causal inference approach. The output of this function may or may not satisfy the covariate balance test. (compile_pseudo_pop)
* check_args function to check availability of the required parameters.
* check_covar_balance function to check if the generated pseudo population statistically acceptable.
* create_matching function to generate pseudo population based on matching approach.
* acknowledgments to index file 

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

### Removed

* GPSmatching.R functions are separated into smaller functions, and the file is removed.

###
