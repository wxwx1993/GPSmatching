#' @title
#' Trim a gps object based on provided trimming quantiles
#'
#' @description
#' Trims a gps object based on provided trimming quantiles.
#'
#' @param gps_obj A gps S3 object.
#' @param trim_quantiles A vector of trimming quantiles with two min and max
#' values.
#'
#' @return
#' A trimmed gps_obj
#'
#' @keywords internal
#'
trim_gps <- function(gps_obj, trim_quantiles){

  if ((trim_quantiles[1] < 0 || trim_quantiles[1] > 1) ||
      (trim_quantiles[2] < 0 || trim_quantiles[2] > 1) ||
      (trim_quantiles[1] > trim_quantiles[2])) {
    stop(paste("trim_quntiles should be in the [0,1] range,",
               " and the first quantile should be less than the second one."))
  }

  if (!inherits(gps_obj, "cgps_gps")){
    stop("Provided gps_obj is not an standard gps object.")
  }

  df1 <- gps_obj$dataset

  # get trim quantiles and trim data
  q1 <- stats::quantile(df1[["gps"]], trim_quantiles[1])
  q2 <- stats::quantile(df1[["gps"]], trim_quantiles[2])

  logger::log_debug("{trim_quantiles[1]*100}% quantile for trim: {q1}")
  logger::log_debug("{trim_quantiles[2]*100}% for trim: {q2}")
  df1 <- df1[df1[["gps"]] <= q2  & df1[["gps"]] >= q1, ]

  gps_obj$dataset <- df1

  return(gps_obj)

}
