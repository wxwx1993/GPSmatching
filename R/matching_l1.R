# Matching function using L1 distance on single exposure level w

matching_l1 <- function(dataset,
                        e_gps_pred,
                        e_gps_std_pred,
                        w,
                        delta_n=1,
                        w_resid,
                        scale)
{
  w_new <- (w-e_gps_pred)/e_gps_std_pred
  p.w <- approx(density(w_resid,na.rm = TRUE)$x,density(w_resid,na.rm = TRUE)$y,xout=w_new,rule=2)$y

  w.min <- min(dataset[["w"]],na.rm=T)
  w.max <- max(dataset[["w"]],na.rm=T)
  gps.min <- min(dataset[["gps"]],na.rm=T)
  gps.max <- max(dataset[["gps"]],na.rm=T)
  ##
  dataset <- transform(dataset,
                       std.w = (w - w.min) / (w.max - w.min),
                       std.gps = (gps - gps.min) / (gps.max - gps.min))
  std.w <- (w - w.min) / (w.max - w.min)
  std.p.w <- (p.w - gps.min) / (gps.max - gps.min)
  ##
  dataset.subset <- dataset[abs(dataset[["w"]] - w) <= (delta_n/2), ]
  ##
  wm <- apply(abs(outer(dataset.subset[["std.gps"]], std.p.w, `-`)) * scale,
              2,
              function(x) which.min(abs(dataset.subset[["std.w"]] - std.w) * (1 - scale) + x)
  )
  dp <- dataset.subset[wm,]
  return(dp)
  gc()
}
