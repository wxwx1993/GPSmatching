# CausalGPS Functional Tests

Unit tests are addressed under the tests folder; however, time-consuming unit tests and unit tests based on numerous cores can be added here.

## How to contribute to these tests

You can use R or Rmarkdown formats in your tests. We ignore this folder in building the package, so there is no restriction from the packaging perspective. However, please follow the following notes.

- Start all testing file names with `ft_` to distinguish these files from other files. 
- Provide a brief explanation (as a comment in R and as text in Rmd files) about the test and what the user should expect. 
- Make sure the test runs without any error and use sufficient printing outs to communicate with the users. 
- If you expect an error during the test, put it inside the `testthat::expect_error` function.

## List of available tests

- 
