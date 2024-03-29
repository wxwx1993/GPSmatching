% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_wrap_sl_lib.R
\name{gen_wrap_sl_lib}
\alias{gen_wrap_sl_lib}
\title{Generate customized wrapper for SuperLearner libraries}
\usage{
gen_wrap_sl_lib(lib_name, params, nthread)
}
\arguments{
\item{lib_name}{The library name (e.g., \code{m_xgboost}).}

\item{params}{A list that includes key-values for different parameters. Only
relevant parameters will be extracted, others will be ignored.}

\item{nthread}{Number of threads available to be used by external libraries
(in case they can use it).}
}
\value{
Returns a list of TRUE and best used parameters, if the modified library for
the given library is implemented; otherwise, it returns a list of FALSE.
This function is also called for side effects.
}
\description{
The function generates customized wrapper in order to have access to the
external libraries hyperparameters.
}
\keyword{internal}
