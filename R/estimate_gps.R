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
#' @param internal.use If TRUE will return helper vectors as well. Otherwise,
#'  will return original data + GPS value.
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
#' If \code{internal.use} is set to be FALSE, only originla data set + GPS will
#' be returend.
#'
#' @export
#'
#' @examples
#'
#' m.d <- GenSynData(sample.size = 100)
#' data.with.gps <- EstimateGPS(m.d$Y,
#'                              m.d$treat,
#'                              m.d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
#'                              pred.model = "sl",
#'                              internal.use = FALSE,
#'                              sl.lib = c("SL.xgboost","SL.earth","SL.gam",
#'                                        "SL.ranger")
#'                              )
#'
EstimateGPS <- function(Y,
                        w,
                        c,
                        pred.model,
                        internal.use = TRUE,
                        ...){


  # Check passed arguments
  CheckArgsEGPS(pred.model, ...)

  e.gps <- TrainIt(target = w, input = c, pred.model, ...)
  e.gps.pred <- e.gps$SL.predict
  e.gps.std <- TrainIt(target = abs(w-e.gps.pred), input = c, pred.model, ...)
  e.gps.std.pred <- e.gps.std$SL.predict
  w.resid <- ComputeResid(w,e.gps.pred,e.gps.std.pred)
  gps <- ComputeDensity(w.resid, w.resid)
  w.mx <- ComputeMinMax(w)
  gps.mx <- ComputeMinMax(gps)
  dataset <- cbind(Y,w,gps,c)

  if (internal.use){
    return(list(dataset, e.gps.pred, e.gps.std.pred, w.resid, gps.mx, w.mx))
  } else {
    return(dataset)
  }
}
