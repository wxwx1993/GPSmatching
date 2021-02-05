#' @title
#' Estimate GPS values
#'
#' @description
#' Estimates GPS value for each observation.
#'
#'
#' @param Y A vector of observed outcome variable.
#' @param w A vector of observed continuous exposure variable.
#' @param c A data frame or matrix of observed covariates variable.
#' @param pred.model The selected prediction model.
#' @param ...  Additional arguments passed to the model.
#'
#' @return
#' The function returns a list of 6 objects according to the following order:
#'   - Original data set + GPS values (Y, w, GPS, c)
#'   - e_gps_pred
#'   - e_gps_std_pred
#'   - w_resid
#'   - gps_mx (min and max of gps)
#'   - w_mx (min and max of w).
#'
#' @export
#'
EstimateGPS <- function(Y,
                        w,
                        c,
                        pred.model,
                        ...){

  e.gps <- TrainIt(Y = w, X = c, pred.model, ...)
  e.gps.pred <- e.gps$SL.predict
  e.gps.std <- TrainIt(Y = abs(w-e.gps.pred), X = c, pred.model, ...)
  e.gps.std.pred <- e.gps.std$SL.predict
  w.resid <- ComputeResid(w,e.gps.pred,e.gps.std.pred)
  gps <- ComputeDensity(w.resid, w.resid)
  w.mx <- ComputeMinMax(w)
  gps.mx <- ComputeMinMax(gps)
  dataset <- cbind(Y,w,gps,c)

  return(list(dataset, e.gps.pred, e.gps.std.pred, w.resid, gps.mx, w.mx))
}
