#' Estimate smoothed exposure-response function (ERF).
#'
#' @param matched_Y a vector of outcome variable in matched set.
#' @param matched_w a vector of continuous exposure variable in matched set.
#' @param bw.seq a vector of bandwidth values (Default is seq(0.2,2,0.2)).
#' @param w.vals a vector of exposure levels that ERF curves was evaluated.
#' @return
#' \code{erf}: The function returns a vector saved the output values of exposure-response function (ERF) given input \code{w.vals}.
#' @export

# Fit non-parametric kernel smoothing on matched set
matching_smooth<-function(matched_Y,
                          matched_w,
                          bw.seq=seq(0.2,2,0.2),
                          w.vals){
  ## The specified Gaussian kernel
  kern_fun <- function(t){ dnorm(t) }
  w_fun <- function(bw){
    w.avals <- NULL
    for (w.val in w.vals){
    w.std <- (matched_w-w.val)/bw
    kern.std <- kern_fun(w.std)/bw
    w.avals <- c(w.avals, mean(w.std^2*kern.std)*(kern_fun(0)/bw) /
                   (mean(kern.std)*mean(w.std^2*kern.std)-mean(w.std*kern.std)^2))
      }
    return(w.avals/length(matched_w))
    }
  hatvals <- function(bw){approx(w.vals,w_fun(bw),xout=matched_w,rule=2)$y}
  smooth_fun <- function(out,bw){
    approx(locpoly(matched_w,out,bandwidth=bw, gridsize=1000),xout=matched_w,rule=2)$y
    }
  ##
  risk_fun <- function(h){
    hats <- hatvals(h); mean( ((matched_Y - smooth_fun(matched_Y,bw=h))/(1-hats))^2)
    }
  risk.val <- sapply(bw.seq, risk_fun)
  h.opt <- bw.seq[which.min(risk.val)]

  erf <- approx(locpoly(matched_w, matched_Y, bandwidth=h.opt), xout=w.vals)$y
  return(erf)
}
