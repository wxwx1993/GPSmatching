#' @title
#' Check  Weighted Covariate Balance Using Absolute Approach
#'
#' @description
#' Checks covariate balance based on absolute weighted correlations for
#' given data sets.
#'
#' @param w A vector of observed continuous exposure variable.
#' @param vw A vector of weights.
#' @param c A data.table of observed covariates variable.
#' @return
#' The function returns a list saved the measure related to covariate balance
#' \code{absolute_corr}: the absolute correlations for each pre-exposure
#'  covairates;
#' \code{mean_absolute_corr}: the average absolute correlations for all
#'  pre-exposure covairates.
#'
#' @export
#'
#' @examples
#' set.seed(639)
#' n <- 100
#' mydata <- generate_syn_data(sample_size=100)
#' year <- sample(x=c("2001","2002","2003","2004","2005"),size = n,
#'  replace = TRUE)
#' region <- sample(x=c("North", "South", "East", "West"),size = n,
#'  replace = TRUE)
#' mydata$year <- as.factor(year)
#' mydata$region <- as.factor(region)
#' mydata$cf5 <- as.factor(mydata$cf5)
#' data.table::setDT(mydata)
#' cor_val <- absolute_weighted_corr_fun(mydata[,2],
#'                                       data.table::data.table(runif(n)),
#'                                       mydata[, 3:length(mydata)])
#' print(cor_val$mean_absolute_corr)
#'
absolute_weighted_corr_fun <- function(w,
                                       vw,
                                       c){

  if (class(w)[1] != "data.table") {stop("w should be a data.table.")}
  if (class(vw)[1] != "data.table") {stop("vw should be a data.table.")}
  if (class(c)[1] != "data.table") {stop("c should be a data.table.")}

  # detect numeric columns
  col_n <- colnames(c)[unlist(lapply(c, is.numeric))]

  # detect factorial columns
  col_f <- colnames(c)[unlist(lapply(c, is.factor))]

  absolute_corr_n <- absolute_corr_f <- NULL

  if (length(col_n) > 0) {
    absolute_corr_n <- sapply(col_n,function(i){
      abs(wCorr::weightedCorr(as.list(w)[[colnames(w)[1]]],
                              c[[i]],
                              weights = as.list(vw)[[colnames(vw)[1]]],
                              method = c("spearman")))})
    absolute_corr_n <- unlist(absolute_corr_n)
    names(absolute_corr_n) <- col_n
  }

  if (length(col_f) > 0) {
    internal_fun<- function(i){
                          abs(wCorr::weightedCorr(as.list(w)[[colnames(w)[1]]],
                          c[[i]],
                          weights = as.list(vw)[[colnames(vw)[1]]],
                          method = c("Polyserial")))}

    absolute_corr_f <- c()
    for (item in col_f){
      if (length(unique(c[[item]])) == 1 ){
        absolute_corr_f <- c(absolute_corr_f, NA)
      } else {
        absolute_corr_f <- c(absolute_corr_f, internal_fun(item))
      }
    }
    names(absolute_corr_f) <- col_f
  }

  absolute_corr <- c(absolute_corr_n, absolute_corr_f)

  logger::log_trace(paste0("absolute_corr value: {paste(names(absolute_corr), ",
                           "absolute_corr, collapse = ', ', sep = ' : ')}"))

  if (sum(is.na(absolute_corr)) > 0) {
    warning(paste(
      "The following features generated missing values: ",
      names(absolute_corr)[is.na(absolute_corr)],
      "\nIn computing mean covariate balance, they will be ignored."))
  }


  # compute mean value
  mean_val <- mean(absolute_corr, na.rm = TRUE)

  # compute median value
  median_val <- median(absolute_corr, na.rm = TRUE)

  # Maximal value
  max_val <- max(absolute_corr, na.rm = TRUE)

  return(list(absolute_corr = absolute_corr,
              mean_absolute_corr = mean_val,
              median_absolute_corr = median_val,
              maximal_absolute_corr = max_val))

}
