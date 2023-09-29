Re-Submission (September 29, 2023)

Thank you for your feedback. We have reviewed the discussions on R-package-devel and acknowledge that data.table may utilize multiple threads at certain instances. To address this, we have incorporated additional code to constrain data.table to a single thread during CRAN tests. We hope that this modification resolves the issue.
 
Best regards, 
Naeem Khoshnevis 
Research Computing and Data Services 
Harvard University


Re-Submission (September 29, 2023)

Thank you for your feedback. We have adjusted the package to skip the computationally intensive tests during CRAN checks due to the issues observed in one of the operating systems. We would like to kindly request acceptance of the package with this note, as the noted concern is specific to a single OS.
 
Best regards, 
Naeem Khoshnevis 
Research Computing and Data Services 
Harvard University


Re-Submission (September 29, 2023)

Thank you for your feedback and observations. In response, and as part of this revised submission, we have instituted several modifications to address the raised concerns:
- We have eliminated an additional vignette that required extensive computation.
- We have relocated several examples to the `\donttest{}` block to avoid their execution during CRAN checks.
- Some of the more computationally demanding tests have been configured to skip execution during CRAN checks.

We are optimistic that these alterations will rectify the identified issues. Regrettably, we encounter limitations in verifying the resolutions directly, as attempts to reproduce the said issues both locally and during an `rhub` check were unsuccessful – all tests and checks passed without discrepancies. We appreciate your understanding and look forward to any further guidance or suggestions you might have.

Best regards, 
Naeem Khoshnevis 
Research Computing and Data Services 
Harvard University


Re-Submission (September 29, 2023)

Thank you for the notes. We have removed the computationally intensive vignette and also moved examples to the donttest block. We hope these changes resolve the issues. Unfortunately, we are unable to reproduce them locally or during an rhub check (as they pass successfully), so we could not verify the resolution directly.

Best regards, 
Naeem Khoshnevis 
Research Computing and Data Services 
Harvard University

Submission (September 29, 2023)

Thank you so much for taking the time and reviewing the CausalGPS 0.4.1 package. In this version upgrade we:

- Added more details to the plots.
- Removed unnecessary outcome input in the design stage. 
- Addressed raised notes from CRAN on v0.4.0.

The package tested locally and on rhub and passed all tests and checks successfully. 

Best regards, 
Naeem Khoshnevis 
Research Computing and Data Services 
Harvard University
