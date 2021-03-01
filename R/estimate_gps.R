#' @title
#' Estimate GPS values
#'
#' @description
#' Estimates GPS value for each observation.
#'
#'
#' @param Y A vector of observed outcome variable.
#' @param w A vector of observed continuous exposure variable.
#' @param c A matrix or dataframe of observed covariates variable.
#' @param pred_model The selected prediction model.
#' @param running_appr The running approach.
#' @param internal_use If TRUE will return helper vectors as well. Otherwise,
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
#' If \code{internal.use} is set to be FALSE, only original data set + GPS will
#' be returned.
#'
#' @export
#'
#' @examples
#' m_d <- gen_syn_data(sample_size = 100)
#' data_with_gps <- estimate_gps(m_d$Y,
#'                               m_d$treat,
#'                               m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
#'                               pred_model = "sl",
#'                               running_appr = "base",
#'                               internal_use = FALSE,
#'                               sl_lib = c("SL.xgboost","SL.earth","SL.gam",
#'                                        "SL.ranger")
#'                              )
#'
estimate_gps <- function(Y,
                         w,
                         c,
                         pred_model,
                         running_appr,
                         internal_use = TRUE,
                         ...){


  # Check passed arguments
  check_args_estimate_gps(pred_model, running_appr, ...)

  e_gps <- train_it(target = w, input = c, pred_model, running_appr, ...)
  e_gps_pred <- e_gps$SL.predict
  e_gps_std <- train_it(target = abs(w-e_gps_pred), input = c, pred_model,
                       running_appr, ...)
  e_gps_std_pred <- e_gps_std$SL.predict
  w_resid <- compute_resid(w,e_gps_pred,e_gps_std_pred)
  gps <- compute_density(w_resid, w_resid)
  w_mx <- compute_min_max(w)
  gps_mx <- compute_min_max(gps)
  dataset <- cbind(Y,w,gps,c)

  if (internal_use){
    return(list(dataset, e_gps_pred, e_gps_std_pred, w_resid, gps_mx, w_mx))
  } else {
    return(dataset)
  }
}
