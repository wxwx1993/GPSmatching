Resubmission (September 3, 2021 - current)

Thank you so much for taking the time and reviewing the CausalGPS 0.2.6 package.
The original CausalGPS package was accepted on July 23, 2021. However, later on I have received an email regarding some installing problems on macOS and Solaris systems.

I fixed all the raised comments. Here is the summary of changes:

* Added _OPENMP flag to check if omp.h is available or not.
* Fixed a bug that was failing tests on some systems. 
* Tested on most of r-hub configuration successfully. 
* removed redundant package call (glue)
* Renamed the vignettes file name, Capital initials + m-dash instead of underscore.

An important note:

The package passes all tests and checks successfully. Here is a sample of the test environment:

* Debian Linux, R-devel, clang, ISO-8859-15 locale (debian-clang-devel),
* Fedora Linux, R-devel, clang, gfortran (fedora-clang-devel),
* macOS 10.13.6 High Sierra, R-release, brew (macos-highsierra-release),
* Oracle Solaris 10, x86, 32 bit, R-release (solaris-x86-patched),
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (windows-x86_64-devel).

It also passes Ubuntu tests on my local VM. However, on R-hub, I get PREPERROR while testing on Ubuntu systems (although it passes all tests successfully on R-hub Ubuntu system). 
I have already mentioned the problem in the R-hub Gitter channel and waiting for their response.
My educated guess is that the problem is related to R-hub containers rather than the package.
If you find this an issue, please do not approve the package, and let's wait for R-hub's feedback. 

Best regards,
Naeem Khoshnevis
FASRC - Harvard University
