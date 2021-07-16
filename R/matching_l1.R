#' @title
#' Match observations
#'
#' @description
#' Matching function using L1 distance on single exposure level w
#'
#' @param dataset a completed observational data frame or matrix containing
#'  (Y, w, gps, counter, row_index, c).
#' @param e_gps_pred a vector of predicted gps values obtained by Machine
#' learning methods.
#' @param e_gps_std_pred a vector of predicted std of gps obtained by
#'  Machine learning methods.
#' @param w the targeted single exposure levels.
#' @param w_resid the standardized residuals for w.
#' @param w_mx a vector with length 2, includes min(w), max(w).
#' @param gps_mx a vector with length 2, includes min(gps), max(gps)
#' @param scale a specified scale parameter to control the relative weight
#' that is attributed to
#' the distance measures of the exposure versus the GPS estimates
#'  (Default is 0.5).
#' @param delta_n a specified caliper parameter on the exposure (Default is 1).
#' @param nthread Number of available cores.
#' @param optimized_compile An option to activate optimized compilation.
#' @param gps_model Model type which is used for estimating GPS value, including
#' parametric (default) and non-parametric.
#' @return
#' \code{dp}: The function returns a data.table saved the matched points on
#'  by single exposure
#' level w by the proposed GPS matching approaches.
#'
#' @keywords internal
#'
matching_l1 <- function(w,
                        dataset,
                        e_gps_pred,
                        e_gps_std_pred,
                        w_resid,
                        gps_mx,
                        w_mx,
                        gps_model = "parametric",
                        delta_n=1,
                        scale=0.5,
                        nthread=1,
                        optimized_compile)
{

  if (length(w)!=1){
    stop("w should be a vector of size 1.")
  }

  logger::log_debug("Started matching on single w value (w = {w}) ...")
  st_ml_t <- proc.time()

  if (gps_model == "parametric"){
    p_w <- stats::dnorm(w, mean = e_gps_pred, sd=e_gps_std_pred)
  } else if (gps_model == "non-parametric") {
    w_new <- compute_resid(w, e_gps_pred, e_gps_std_pred)
    p_w <- compute_density(w_resid, w_new)
  } else {
    stop(paste("Invalid gps model: ", gps_model,
               ". Valide options: parametric, non-parametric"))
  }

  w_min <- w_mx[1]
  w_max <- w_mx[2]
  gps_min <- gps_mx[1]
  gps_max <- gps_mx[2]

  # handles check note.
  gps <- NULL

  dataset <- transform(dataset,
                       std_w = (w - w_min) / (w_max - w_min),
                       std_gps = (gps - gps_min) / (gps_max - gps_min))

  std_w <- (w - w_min) / (w_max - w_min)
  std_p_w <- (p_w - gps_min) / (gps_max - gps_min)

  dataset_subset <- dataset[abs(dataset[["w"]] - w) <= (delta_n/2), ]

  if (nrow(dataset_subset) < 1){
    logger:: log_warn(paste("There is no data to match with ", w, "in ", delta_n/2,
                  " radius."))
    return(list())
  }
  wm <- compute_closest_wgps(dataset_subset[["std_gps"]],
                             std_p_w,
                             dataset_subset[["std_w"]],
                             std_w,
                             scale)

  dp <- dataset_subset[wm,]

  dp["std_w"] <- NULL
  dp["std_gps"] <- NULL

  e_ml_t <- proc.time()
  logger::log_debug("Finished matching on single w value (w = {w}), ",
                    " Wall clock time: {(e_ml_t - st_ml_t)[[3]]} seconds.")

  if (!optimized_compile){
    return(dp)
  } else {
    return(dp["row_index"])
  }
}
