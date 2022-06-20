
Submission (June 20, 2022)

Thank you so much for taking the time and reviewing the CausalGPS 0.2.8 package. In this version upgrade we:


- Added a new internal features estimate_gps function.
- Improved test coverage.
- Optimized internal handling of pseudo population (by computing frequency table at each core) to reduce memory consumption.
- Vectorized population compilation and used data.table for multi-thread assignment.
- Fixed a bug with passing empty counter vector. 


The package passes all tests and checks successfully. Here is a sample of the test environment:

Debian Linux, R-devel, clang, ISO-8859-15 locale (debian-clang-devel) (OK)
Fedora Linux, R-devel, clang, gfortran (fedora-clang-devel) (OK)
Windows Server 2022, R-devel, 64 bit (windows-x86_64-devel) (OK)
Ubuntu Linux 20.04.1 LTS, R-release, GCC (ubuntu-gcc-release) (OK, but the emails says preperror)
macOS 10.13.6 High Sierra, R-release, brew (macos-highsierra-release)(OK)


Best regards, 
Naeem Khoshnevis 
FASRC - Harvard University
