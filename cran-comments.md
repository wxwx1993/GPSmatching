## Resubmission (July 22, 2021 - current)

Thank you so much for taking the time and reviewing the CausalGPS 0.2.5 package.
I fixed all the raised comments. Here is the summary of changes:

* Reworded description section of the package:
  From: 
  An R package for implementing matching on generalized propensity scores with continuous exposures. We developed an innovative approach for estimating causal effects using observational data in settings with continuous exposures and introduce a new framework for GPS caliper matching.
  
  To:
  Provides a framework for estimating causal effects of a continuous exposure using observational data, by implementing matching and weighting on the generalized propensity score.

* Added \value tag to the following functions:
  - gen_wrap_sl_lib.Rd
  - get_logger.Rd
  - log_system_info.Rd
  - plot.gpsm_erf.Rd
  - plot.gpsm_pspop.Rd
  - print.gpsm_erf.Rd
  - print.gpsm_pspop.Rd
  - set_logger.Rd
  
* Added \argument tag to the following functions:
  - get_logger.Rd
  - log_system_info.Rd


## Resubmission (July 20, 2021)

Thank you so much for taking the time and reviewing the CausalGPS 0.2.5 package. I fixed all problems with URLs.

* Fixed arXive paper URL
https://arxiv.org/abs/1812.06575/ --> https://arxiv.org/abs/1812.06575

* Removed *inst* folder contents

Please let me know if I need to take any further action. 


## Resubmission (July 19, 2021)

Thank you so much for taking the time and reviewing the CausalGPS 0.2.5 package. I went through your comments and fixed the problems; here is the list of changes:

* The NOTEs are created based on misspelled words in the DESCRIPTION file. All of them are names, except "pre" in "pre-print". I changed it into "preprint".  

* Fixed the arXiv paper URL (http -> https)
 http://arxiv.org/abs/1812.06575 --> https://arxiv.org/abs/1812.06575/

* Converted invalid CRAN URLs into canonical form:
https://cran.r-project.org/web/packages/ranger/index.html --> https://cran.r-project.org/package=ranger
https://cran.r-project.org/web/packages/logger/index.html --> https://cran.r-project.org/package=logger

* Fixed "permanently moved" issue with Github actions badge.

https://github.com/FASRC/GPSmatching/actions  --> https://github.com/fasrc/CausalGPS/actions

* I double-checked the possibly invalid file URLs. They are valid; as a result, I leave them as-is. 

Please let me know if I need to take any further action. 

