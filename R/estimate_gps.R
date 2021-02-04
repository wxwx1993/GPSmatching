#' @title
#' Estimate GPS values
#'
#' @description
#' Estimates GPS value for each observation.
#'
#'
#' @param y A vector of observed outcome variable (Y).
#' @param w A vector of observed continuous exposure variable.
#' @param c A data frame or matrix of observed covariates variable.
#' @param pred.model The selected prediction model.
#' @param ...  Additional arguments passed to the model.
#'
#' @return
#' \code{matched_set}: The function returns a data.table saved the constructed
#'  matched set by the proposed GPS matching approaches.
#'
#' @export

# Create matched set using GPS matching approaches
EstimateGPS <- function(y,
                        w,
                        c,
                        pred.model,
                        ...){

  e_gps <- TrainIt(Y = w, X = c, pred.model, ...)
  e_gps_pred <- e_gps$SL.predict
  e_gps_std <- TrainIt(Y = abs(w-e_gps_pred), X = c, pred.model, ...)
  e_gps_std_pred <- e_gps_std$SL.predict
  w_resid <- compute_resid(w,e_gps_pred,e_gps_std_pred)
  gps <- compute_density(w_resid, w_resid)
  w_mx <- compute_minmax(w)
  gps_mx <- compute_minmax(gps)
  dataset <- cbind(y,w,gps,c)


  return(list(dataset, e_gps_pred, e_gps_std_pred, w_resid, gps_mx, w_mx))
}
