#' @title
#' Check covariate balance
#'
#' @description
#' Checks covariate balance based on absolute correlations for given data sets.
#'
#' @param w A vector of observed continuous exposure variable.
#' @param c A data frame or matrix of observed covariates variable.
#' @return
#' The function returns a list saved the measure related to covariate balance
#' \code{absolute_corr}: the absolute correlations for each pre-exposure
#'  covairates;
#' \code{mean_absolute_corr}: the average absolute correlations for all
#'  pre-exposure covairates.
#' @importFrom stats cor
#'
#' @keywords internal

absolute_corr_fun <- function(w,
                              c){

  # detect numeric columns
  col_n <- colnames(c)[unlist(lapply(c, is.numeric))]

  # detect factorial columns
  col_f <- colnames(c)[unlist(lapply(c, is.factor))]

  absolute_corr_n <- absolute_corr_f <- NULL

  if (length(col_n) > 0) {
  absolute_corr_n<- sapply(col_n,function(i){
              abs(cor(w,c[[i]],method = c("spearman")))})
  }

  if (length(col_f)) {
  absolute_corr_f<- sapply(col_f,function(i){
    abs(polycor::polyserial(w,c[[i]]))})
  }

  absolute_corr <- c(absolute_corr_f, absolute_corr_n)

  return(list(absolute_corr = absolute_corr,
              mean_absolute_corr = mean(absolute_corr)))
}
