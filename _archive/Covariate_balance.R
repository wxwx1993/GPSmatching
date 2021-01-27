#' Check covariate balance based on absolute correlations for given data sets.
#'
#' @param w a vector of observed continuous exposure variable.
#' @param c a data frame or matrix of observed covariates variable.
#' @return
#' The function returns a list saved the measure related to covariate balance
#' \code{absolute_corr}: the absolute correlations for each pre-exposure covairates;
#' \code{mean_absolute_corr}: the average absolute correlations for all pre-exposure covairates.
#' @export

absolute_corr_fun <- function(w,
                              c){
  absolute_corr<- sapply(colnames(c),function(i){
              abs(cor(w,c[[i]],method = c("spearman")))})

  return(list(absolute_corr = absolute_corr,
              mean_absolute_corr = mean(absolute_corr)))
}

