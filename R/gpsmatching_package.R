#' @title
#' The 'GPSmatching' package.
#'
#' @description
#' An R package for implementing matching on generalized propensity scores with
#' continuous exposures.
#'
#' @details
#' We developed an innovative approach for estimating causal effects using
#' observational data in settings with continuous exposures, and introduce a new
#' framework for GPS caliper matching.
#'
#' @docType package
#' @name GPSmatching-package
#' @aliases GPSmatching
#' @author Naeem Khoshnevis
#' @author Xiao Wu
#' @author Danielle Braun
#' @import parallel
#' @import data.table
#' @import SuperLearner
#' @import xgboost
#' @import earth
#' @import ranger
#' @import gam
#' @import KernSmooth
#' @import polycor
#' @importFrom Rcpp sourceCpp
#' @useDynLib GPSmatching, .registration = TRUE
#'
#' @references
#' Wu, X., Mealli, F., Kioumourtzoglou, M.A., Dominici, F. and Braun, D., 2018.
#'  Matching on generalized propensity scores with continuous exposures.
#'  arXiv preprint arXiv:1812.06575.
#'
#' Kennedy, E.H., Ma, Z., McHugh, M.D. and Small, D.S., 2017. Non‐parametric
#'  methods for doubly robust estimation of continuous treatment effects.
#'  Journal of the Royal Statistical Society: Series B (Statistical Methodology),
#'  79(4), pp.1229-1245.
#'
NULL
