Re-submission (June 21, 2022)

Thank you so much for taking the time and reviewing the CausalGPS 0.2.8 package and providing feedback. 
I fixed the issues according to the following:

- Modified (https://sylabs.io/guides/3.0/user-guide/definition_files.html) into (https://docs.sylabs.io/guides/3.0/user-guide/definition_files.html) (2 instances.)
- Modified (https://sylabs.io/guides/3.7/user-guide/quick_start.html) into ( https://docs.sylabs.io/guides/3.7/user-guide/quick_start.html)
- Modified (\url{https://doi.org/10.1002/joc.3413}) to (\doi{10.1002/joc.3413})
- Fixed the linking issue in the readme file. 
- Added trailing slash to the URLs.
- Double-checked http -> https. 

Best regards, 
Naeem Khoshnevis 
FASRC - Harvard University



Submission (June 20, 2022)

Thank you so much for taking the time and reviewing the CausalGPS 0.2.8 package. In this version upgrade we:


- Added a new internal features estimate_gps function.
- Improved test coverage.
- Optimized internal handling of the pseudo population (by computing frequency table at each core) to reduce memory consumption.
- Vectorized population compilation and used data.table for multi-thread assignment.
- Fixed a bug with passing an empty counter vector. 


The package passes all tests and checks successfully. Here is a sample of the test environment:

Debian Linux, R-devel, clang, ISO-8859-15 locale (debian-clang-devel) (OK)
Fedora Linux, R-devel, clang, gfortran (fedora-clang-devel) (OK)
Windows Server 2022, R-devel, 64 bit (windows-x86_64-devel) (OK)
Ubuntu Linux 20.04.1 LTS, R-release, GCC (ubuntu-gcc-release) (OK, but the emails says preperror)
macOS 10.13.6 High Sierra, R-release, brew (macos-highsierra-release)(OK)

On Github actions checks, it raises a warning (Requires (indirectly) orphaned package: ‘gtools’). But as far as I check, the gtools package has not been orphaned. 


Best regards, 
Naeem Khoshnevis 
FASRC - Harvard University
