Submission (Dec 15, 2022)
		
Thank you for taking the time to review the CausalGPS 0.2.9 package.
In this version upgrade we:
- Dropped importing `KernSmooth` and `tidyr` packages.
- Dropped`pred_model` argument. The package only uses SuperLearner for prediction models.
- Added features to use a more optimized algorithm for a commonly used simplified case (scale = 1).
- Added effective sample size.
- Added Kolmogorov-Smirnov (KS) statistics for the generated pseudo-population (uses `Ecume` package).
- Made `sl_lib` a required argument. 
- Removed `earth` and `ranger` packages from mandatory imports.
- Standardized the trimming approach to be less confusing for the users.
- Modified internal kernel smoothing approach. 
- Renamed a couple of internal parameters for clarity and uniformity in the package. 
- Fixed a bug on the covariate balance threshold.

The package passes all tests and checks successfully.

		

Best regards,
Naeem Khoshnevis
Harvard University
Information Technology
