#' @title
#' Create matched set
#'
#' @description
#' GPS Matching function to create matched set
#'
#'
#' @param Y a vector of observed outcome variable.
#' @param w a vector of observed continuous exposure variable.
#' @param c a data frame or matrix of observed covariates variable.
#' @param matching_fun a specifed matching function (Default is "matching_l1"
#'  (Manhattan distance matching)).
#' @param scale a specified scale parameter to control the relative weight that
#'  is attributed to the distance measures of the exposure versus the GPS
#'  estimates (Default is 0.5).
#' @param delta_n a specified caliper parameter on the exposure (Default is 1).
#' @param model a prediction model
#' @param ...  Additional arguments passed to the model.
#' @details
#'
#' ## Implemented models
#' * sl: SuperLearner \cr
#'   Uses SuperLearner package to train a prediction model.The required
#'   parameters:
#'   - *sl.lib*: a set of methods used for estimating GPS (e.g., ("SL.xgboost",
#'   "SL.earth","SL.gam","SL.ranger"))
#'
#' @return
#' \code{matched_set}: The function returns a data.table saved the constructed
#'  matched set by the proposed GPS matching approaches.
#'
#' @name create_matching
#' @export


# Create matched set using GPS matching approaches
create_matching <- function(Y,
                            w,
                            c,
                            matching_fun=matching_l1,
                            scale=0.5,
                            delta_n=1,
                            model,
                            ...){


  # ## Check for model arguments
  # if (model == 'sl'){
  #   required_args <- c('sl.lib')
  # } else if (model == 'ni') {
  #   required_args = c()
  # }
  #
  # dot_args <- list(...)
  #
  # for (arg in required_args){
  #   if (!is.element(arg,names(dot_args))){
  #     stop(paste('At least one argument is not provided for model ',
  #                model, '. Missing argument: ', arg, '.'))
  #   }
  # }


  ## GPS function estimation
  # e_gps <- train_it(Y = w, X = c, model = 'sl', ...)
  # e_gps_pred <- e_gps$SL.predict
  # e_gps_std <- train_it(Y = abs(w-e_gps_pred), X = c, model = 'sl', ...)
  # e_gps_std_pred <- e_gps_std$SL.predict
  # w_resid <- compute_resid(w,e_gps_pred,e_gps_std_pred)
  #
  # gps <- compute_density(w_resid, w_resid)
  # w_mx <- compute_minmax(w)
  # gps_mx <- compute_minmax(gps)
  #
  # dataset <- cbind(Y,w,c,gps)

  out_val <- EstimateGPS(Y, w, c, model, ...)

  bin.num<-seq(min(dataset$w)+delta_n/2, max(dataset$w), by = delta_n)

  matched_set <-  lapply(bin.num,
                        matching_fun,
                        dataset=dataset,
                        e_gps_pred = e_gps_pred,
                        e_gps_std_pred = e_gps_std_pred,
                        w_resid=w_resid,
                        w_mx = w_mx,
                        gps_mx = gps_mx,
                        delta_n=delta_n,
                        scale=scale)

  return(data.table(Reduce(rbind,matched_set)))
}
