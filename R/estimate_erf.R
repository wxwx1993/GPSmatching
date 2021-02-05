#' @title
#' Estimate smoothed exposure-response function (ERF).
#'
#' @description
#' TODO
#'
#' @param matched.Y a vector of outcome variable in matched set.
#' @param matched.w a vector of continuous exposure variable in matched set.
#' @param bw.seq a vector of bandwidth values (Default is seq(0.2,2,0.2)).
#' @param w.vals a vector of values that you want to calculate the values of
#'  the ERF at.
#'
#' @return
#' \code{erf}: The function returns a vector saved the output values of
#'  exposure-response function (ERF) given input \code{w.vals}.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' erf <- EstimateERF(pseuodo.pop$Y, pseuodo.pop$w,
#'                    w.vals = seq(0.1,10, by=0.5))
#'}
EstimateERF<-function(matched.Y,
                      matched.w,
                      bw.seq=seq(0.2,2,0.2),
                      w.vals){
  risk.val <- sapply(bw.seq, ComputeRisk, matched.Y = matched.Y,
                     matched.w = matched.w, w.vals = w.vals)
  h.opt <- bw.seq[which.min(risk.val)]
  erf <- approx(locpoly(matched.w, matched.Y, bandwidth=h.opt), xout=w.vals)$y
  return(erf)
}
