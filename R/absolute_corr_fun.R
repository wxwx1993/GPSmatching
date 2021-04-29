#' @title
#' Check covariate balance
#'
#' @description
#' Checks covariate balance based on absolute correlations for given data sets.
#'
#' @param w A vector of observed continuous exposure variable.
#' @param c A data frame or matrix of observed covariates variable.
#' @param  nthread Number of available threads to use.
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
                              c,
                              nthread){

  # w type should be numeric (polyserial requirments)
  if (!is.numeric(w)) {
     w <- unlist(w)
     if (!is.numeric(w)) {
       stop('w type should be numeric.')
    }
  }

  # convert c to datatable
  data.table::setDT(c)

  # detect numeric columns
  col_n <- colnames(c)[unlist(lapply(c, is.numeric))]

  # detect factorial columns
  col_f <- colnames(c)[unlist(lapply(c, is.factor))]

  absolute_corr_n <- absolute_corr_f <- NULL

  platform_os <- .Platform$OS.type

  if (length(col_n) > 0){
    cl <- parallel::makeCluster(nthread, type="PSOCK")
      absolute_corr_n<- parallel::parLapply(cl, c[,..col_n], function(c_data){
        abs(cor(w,c_data,method = c("spearman")))})
    parallel::stopCluster(cl)
  }

  if (length(col_f) > 0) {
    cl <- parallel::makeCluster(nthread, type="PSOCK")
      absolute_corr_f<- parallel::parLapply(cl, c[,..col_f],function(c_data){
        abs(polycor::polyserial(w,c_data))})
    parallel::stopCluster(cl)
  }

  absolute_corr <- c(unlist(absolute_corr_f), unlist(absolute_corr_n))

  return(list(absolute_corr = absolute_corr,
              mean_absolute_corr = mean(absolute_corr)))
}
