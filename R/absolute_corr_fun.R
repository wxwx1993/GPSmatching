#' @title
#' Check covariate balance using absolute approach
#'
#' @description
#' Checks covariate balance based on absolute correlations for given data sets.
#'
#' @param w A vector of observed continuous exposure variable.
#' @param c A data table of observed covariates variable.
#' @return
#' The function returns a list including:
#' - \code{absolute_corr}: the absolute correlations for each pre-exposure
#'  covariates;
#' - \code{mean_absolute_corr}: the average absolute correlations for all
#'  pre-exposure covariates.
#'
#' @importFrom stats median
#' @export
#' @examples
#' set.seed(291)
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
#' cor_val <- absolute_corr_fun(mydata[,2], mydata[, 3:length(mydata)])
#' print(cor_val$mean_absolute_corr)
#'
absolute_corr_fun <- function(w, c) {

  if (class(w)[1] != "data.table") {stop("w should be a data.table.")}
  if (class(c)[1] != "data.table") {stop("c should be a data.table.")}

  # detect numeric columns
  col_n <- colnames(c)[unlist(lapply(c, is.numeric))]

  # detect factor columns
  col_f <- colnames(c)[unlist(lapply(c, is.factor))]

  absolute_corr_n <- absolute_corr_f <- NULL

  if (length(col_n) > 0) {
      absolute_corr_n <- lapply(col_n, function(i){
        abs(stats::cor(w,c[[i]], method = c("spearman")))})
      absolute_corr_n <- unlist(absolute_corr_n)
      names(absolute_corr_n) <- col_n
  }

  if (length(col_f) > 0) {
      w_numeric <- as.list(w[, 1])[[colnames(w[, 1])[1]]]
      absolute_corr_f <- lapply(col_f, function(i){
        abs(polycor::polyserial(w_numeric, c[[i]]))})
      absolute_corr_f <- unlist(absolute_corr_f)
      names(absolute_corr_f) <- col_f
  }

  absolute_corr <- c(absolute_corr_n, absolute_corr_f)
  logger::log_trace(paste0("absolute_corr value: {paste(names(absolute_corr), ",
                    "absolute_corr, collapse = ', ', sep = ' : ')}"))

  if (sum(is.na(absolute_corr)) > 0){
    warning(paste(
      "The following features generated missing values: ",
      names(absolute_corr)[is.na(absolute_corr)],
      "\n In computing mean covariate balance, they will be ignored."))
  }

  # compute mean value
  mean_val <- mean(absolute_corr, na.rm = TRUE)

  # compute median value
  median_val <- stats::median(absolute_corr, na.rm = TRUE)

  # Maximal value
  max_val <- max(absolute_corr, na.rm = TRUE)

  return(list(absolute_corr = absolute_corr,
              mean_absolute_corr = mean_val,
              median_absolute_corr = median_val,
              maximal_absolute_corr = max_val))
}
