#' Check covariate balance based on absolute correlations for given data sets.
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
#' @export

AbsoluteCorrFun <- function(w,
                              c){
  absolute.corr<- sapply(colnames(c),function(i){
              abs(cor(w,c[[i]],method = c("spearman")))})

  return(list(absolute.corr = absolute.corr,
              mean_absolute.corr = mean(absolute.corr)))
}
