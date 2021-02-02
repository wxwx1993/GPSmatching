#' Matching function using L1 distance on single exposure level w
#'
#' @param dataset a completed observational data frame or matrix containing (Y, w, c).
#' @param e_gps_pred a vector of predicted gps values obtained by Machine learning methods.
#' @param e_gps_std_pred a vector of predicted std of gps obtained by Machine learning methods.
#' @param w the targeted single exposure levels.
#' @param w_resid the standardized residuals for w.
#' @param w_mx a vector with length 2, includes min(w), max(w).
#' @param gps_mx a vector with length 2, includes min(gps), max(gps)
#' @param scale a specified scale parameter to control the relative weight that is attributed to
#' the distance measures of the exposure versus the GPS estimates (Default is 0.5).
#' @param delta_n a specified caliper parameter on the exposure (Default is 1).
#' @return
#' \code{dp}: The function returns a data.table saved the matched points on by single exposure
#' level w by the proposed GPS matching approaches.
#' @export

matching_l1 <- function(w,
                        dataset,
                        e_gps_pred,
                        e_gps_std_pred,
                        w_resid,
                        w_mx,
                        gps_mx,
                        delta_n=1,
                        scale=0.5)
{
  w_new <- compute_resid(w, e_gps_pred, e_gps_std_pred)
  p.w <- compute_density(w_resid, w_new)

  w.min <- w_mx[1]
  w.max <- w_mx[2]
  gps.min <- gps_mx[1]
  gps.max <- gps_mx[2]

  # handles check note.
  gps <- NULL

  dataset <- transform(dataset,
                       std.w = (w - w.min) / (w.max - w.min),
                       std.gps = (gps - gps.min) / (gps.max - gps.min))

  std.w <- (w - w.min) / (w.max - w.min)
  std.p.w <- (p.w - gps.min) / (gps.max - gps.min)

  dataset.subset <- dataset[abs(dataset[["w"]] - w) <= (delta_n/2), ]

  wm <- compute_closest_wgps(dataset.subset[["std.gps"]],
                             std.p.w,
                             dataset.subset[["std.w"]],
                             std.w,
                             scale)

  dp <- dataset.subset[wm,]
  return(dp)
  gc()
}
