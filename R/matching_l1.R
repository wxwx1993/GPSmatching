#' Matching function using L1 distance on single exposure level w
#'
#' @param dataset a completed observational data frame or matrix containing (Y, w, c).
#' @param e_gps_pred a vector of predicted gps values obtained by Machine learning methods.
#' @param e_gps_std_pred a vector of predicted std of gps obtained by Machine learning methods.
#' @param w the targeted single exposure levels.
#' @param w_resid the standardized residuals for w.
#' @param scale a specified scale parameter to control the relative weight that is attributed to the distance measures of the exposure versus the GPS estimates (Default is 0.5).
#' @param delta_n a specified caliper parameter on the exposure (Default is 1).
#' @return
#' \code{dp}: The function returns a data.table saved the matched points on by single exposure level w by the proposed GPS matching approaches.
#' @export

matching_l1 <- function(dataset,
                        e_gps_pred,
                        e_gps_std_pred,
                        w,
                        w_resid,
                        delta_n = 1,
                        scale = 0.5)
{
  w_new <- (w - e_gps_pred) / e_gps_std_pred
  p.w <- approx(density(w_resid, na.rm = TRUE)$x,
                density(w_resid, na.rm = TRUE)$y,
                xout = w_new,
                rule = 2)$y

  w.min <- min(dataset[["w"]], na.rm = T)
  w.max <- max(dataset[["w"]], na.rm = T)
  gps.min <- min(dataset[["gps"]], na.rm = T)
  gps.max <- max(dataset[["gps"]], na.rm = T)
  ##
  dataset <- transform(dataset,
                       std.w = (w - w.min) / (w.max - w.min),
                       std.gps = (gps - gps.min) / (gps.max - gps.min))
  std.w <- (w - w.min) / (w.max - w.min)
  std.p.w <- (p.w - gps.min) / (gps.max - gps.min)
  ##
  dataset.subset <- dataset[abs(dataset[["w"]] - w) <= (delta_n / 2), ]
  ##
  wm <- apply(abs(outer(dataset.subset[["std.gps"]], std.p.w, `-`)) * scale,
              2,
              function(x) which.min(abs(dataset.subset[["std.w"]] - std.w) * (1 - scale) + x)
  )
  dp <- dataset.subset[wm, ]
  return(dp)
  gc()
}
