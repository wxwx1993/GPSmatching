# GPSmatching (development version)

## Added

* GenSynData function for generating synthetic data
* Unittest for generate_synthetic_data
* Function to compute residuals and unittest
* Function to impute NA values based on density and unittest
* Function to separate prediction model training (TrainIt)
* Function to separate min and max value estimation and unittest
* Function to find the closest data based on gps and w
* Wrapper function to generate pseudo population and test it for covariate balance (GenPseudoPop)
* Function to estimate only GPS value (EstimateGPS)
* Helper function to take the input data + GPS values and return pseudo population based on selected causal inference approach. The output of this function may or may not satisfy the covariate balance test. (CompilePseudoPop)
* CheckArgs function to check availability of the required parameters.
* CheckCovarBalance function to check if the generated pseudo population statistically acceptable.
* CreateMatching function to generate pseudo population based on matching approach.

## Changed

* matching_l1 --> MatchingL1
* create_matching --> CreateMatching
* CreateMatching only generates matched dataset.
* absolute_corr_fun --> AbsoluteCorrFun
* Covariate_balance.R --> covariate_balance.R
* matching_smooth --> EstimateERF.R
* risk_fun --> ComputeRisk
* matched_Y --> matched.Y
* matched_w -->matched.w
* smooth_fun --> SmoothERF
* hatvals --> EstimateHatvals
* kernel_fun --> GenerateKernel
* w_fun --> WFun
* GPSmatching-package.R --> gpsmatching_package.R
* GPSmatching_smooth.R --> gpsmatching_smooth.R

## Fixed

* None

## Remove

* GPSmatching.R functions are separated, and the file is removed.