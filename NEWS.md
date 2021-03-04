## GPSmatching (development version)

### Added
- Customized wrapper for xgboost package ().
- `param` as an argument to accept hyperparameters from users.

### Changed
- User needs to pass `m_xgboost` instead of `SL.xgboost` to  use XGBoost package for prediciton purposes.
### Fixed
### Removed



# [0.2.0] - 2021-03-01

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