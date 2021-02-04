#' Kernel function
#'
#' @param t standardized vector (z-score)
#'
#' @return
#' probability distribution
#'
#' @keywords internal
#'
#' @importFrom stats dnorm
kern_fun <- function(t){
  dnorm(t)
}

#' TODO: Function title
#'
#' @param bw bandwidth value
#' @param matched_w a vector of continuous exposure variable in matched set.
#' @param w.vals a vector of values that you want to calculate the values of the ERF at.
#'
#' @return
#' return value (TODO)
#' @keywords internal
#'
w_fun <- function(bw,matched_w,w.vals){
  w.avals <- NULL
  for (w.val in w.vals){
    w.std <- (matched_w-w.val)/bw
    kern.std <- kern_fun(w.std)/bw
    tmp_mean <- mean(w.std^2*kern.std)
    w.avals <- c(w.avals, tmp_mean*(kern_fun(0)/bw) /
                   (mean(kern.std)*tmp_mean-mean(w.std*kern.std)^2))
  }
  return(w.avals/length(matched_w))
}

#' TODO: Function title
#'
#' @param bw bandwidth value
#' @param matched_w a vector of continuous exposure variable in matched set.
#' @param w.vals vector of values that you want to calculate the values of the ERF at.
#'
#' @return
#' TODO: return value
#' @keywords internal
#'
hatvals <- function(bw,matched_w,w.vals){
  approx(w.vals,w_fun(bw,matched_w,w.vals),xout=matched_w,rule=2)$y
}


#' Smoothing exposure response fuction based on bandwidth
#'
#' @param matched_Y a vector of outcome variable in matched set.
#' @param bw bandwidth value
#' @param matched_w a vector of continuous exposure variable in matched set.
#'
#' @return
#' TODO: return value
#' @keywords internal
#'
smooth_fun <- function(matched_Y,bw,matched_w){
  tmp_val <- approx(locpoly(matched_w,matched_Y,bandwidth=bw, gridsize=1000),xout=matched_w,rule=2)$y
  return(tmp_val)
}


#' Compute risk value
#'
#' @param h bandwidth value
#' @param matched_Y a vector of outcome variable in matched set.
#' @param matched_w a vector of continuous exposure variable in matched set.
#' @param w.vals a vector of values that you want to calculate the values of the ERF at.
#'
#' @return
#' TODO: return value
#' @keywords internal
#'
risk_fun <- function(h, matched_Y,matched_w,w.vals){
  hats <- hatvals(h,matched_w,w.vals)
  tmp_mean <- mean( ((matched_Y - smooth_fun(matched_Y,bw=h,matched_w = matched_w))/(1-hats))^2)
  return(tmp_mean)
}


#' Estimate smoothed exposure-response function (ERF).
#'
#' @param matched_Y a vector of outcome variable in matched set.
#' @param matched_w a vector of continuous exposure variable in matched set.
#' @param bw.seq a vector of bandwidth values (Default is seq(0.2,2,0.2)).
#' @param w.vals a vector of values that you want to calculate the values of the ERF at.
#'
#' @return
#' \code{erf}: The function returns a vector saved the output values of exposure-response function (ERF) given input \code{w.vals}.
#'
#' @export
#'
matching_smooth<-function(matched_Y,
                          matched_w,
                          bw.seq=seq(0.2,2,0.2),
                          w.vals){
  risk.val <- sapply(bw.seq, risk_fun, matched_Y = matched_Y, matched_w = matched_w, w.vals = w.vals)
  h.opt <- bw.seq[which.min(risk.val)]
  erf <- approx(locpoly(matched_w, matched_Y, bandwidth=h.opt), xout=w.vals)$y
  return(erf)
}
