#' @title
#' Estimate smoothed exposure-response function (ERF) for pseudo population
#'
#' @description
#' Estimate smoothed exposure-response function (ERF) for matched and weighted
#' data set using non-parametric models.
#'
#' @param m_Y A vector of outcome variable in the matched set.
#' @param m_w A vector of continuous exposure variable in the matched set.
#' @param counter_weight A vector of counter or weight variable in the matched
#' set.
#' @param bw_seq A vector of bandwidth values.
#' @param w_vals A vector of values that you want to calculate the values of
#'  the ERF at.
#' @param nthread The number of available cores.
#' @param kernel_appr Internal kernel approach. Available options are `locpol`
#' and `kernsmooth`.
#'
#' @details
#' Estimate Functions Using Local Polynomial kernel regression.
#'
#' @return
#' The function returns a gpsm_erf object. The object includes the following
#' attributes:
#'
#' - params
#'  - m_Y
#'  - m_w
#'  - bw_seq
#'  - w_vals
#' - erf
#' - fcall
#'
#' @export
#'
#' @examples
#'
#' set.seed(697)
#' m_d <- generate_syn_data(sample_size = 200)
#' pseudo_pop <- generate_pseudo_pop(m_d[, c("id", "Y")],
#'                                   m_d[, c("id", "w")],
#'                                   m_d[, c("id", "cf1","cf2","cf3",
#'                                         "cf4","cf5","cf6")],
#'                                   ci_appr = "matching",
#'                                   pred_model = "sl",
#'                                   sl_lib = c("m_xgboost"),
#'                                   params = list(xgb_nrounds=c(10,20,30),
#'                                    xgb_eta=c(0.1,0.2,0.3)),
#'                                   nthread = 1,
#'                                   covar_bl_method = "absolute",
#'                                   covar_bl_trs = 0.1,
#'                                   covar_bl_trs_type="mean",
#'                                   max_attempt = 1,
#'                                   dist_measure = "l1",
#'                                   delta_n = 1,
#'                                   scale = 0.5)
#'
#' erf_obj <- estimate_npmetric_erf(pseudo_pop$pseudo_pop$Y,
#'                                  pseudo_pop$pseudo_pop$w,
#'                                  pseudo_pop$pseudo_pop$counter_weight,
#'                                  bw_seq=seq(0.2,2,0.2),
#'                                  w_vals = seq(2,20,0.5),
#'                                  nthread = 1)
#'
estimate_npmetric_erf<-function(m_Y,
                                m_w,
                                counter_weight,
                                bw_seq,
                                w_vals,
                                nthread,
                                kernel_appr = "locpol") {

  # function call
  fcall <- match.call()

  if (length(m_Y) != length(m_w)) {
    stop("Length of output and treatment should be equal!")
  }

  if (!is.double(m_Y) || !is.double(m_w)) {
    stop("Output and treatment vectors should be double vectors.")
  }

  if (!(kernel_appr %in% c("locpol", "kernsmooth"))){
    stop(paste("Acceptible kernel_appr: `locpol` and `kernsmooth`",
               "The provided value: ", kernel_appr))
  }

  if (sum(counter_weight == 0) == length(counter_weight)) {
      counter_weight <- counter_weight + 1
      logger::log_debug("Giving equal weight for all samples.")
  }

  if (is.null(get_options("logger_file_path"))) {
    logger_file_path <- "CausalGPS.log"
  } else {
    logger_file_path <- get_options("logger_file_path")
  }

  cl <- parallel::makeCluster(nthread, type="PSOCK",
                              outfile=logger_file_path)

  parallel::clusterExport(cl = cl,
                          varlist = c("estimate_hat_vals", "w_fun",
                                      "generate_kernel", "smooth_erf",
                                      "kernel_appr"
                                      ),
                          envir = environment())

  risk_val_1 <-  parallel::parLapply(cl,
                                     bw_seq,
                                     compute_risk,
                                     matched_Y = m_Y,
                                     matched_w = m_w,
                                     matched_cw = counter_weight,
                                     w_vals = w_vals,
                                     x_eval = NULL,
                                     kernel_appr = kernel_appr)

  parallel::stopCluster(cl)

  risk_val <- do.call(rbind, risk_val_1)[, 1]

  h_opt <- bw_seq[which.min(risk_val)]

  logger::log_info("The band width with the minimum risk value: {h_opt}.")

  if (kernel_appr == "locpol"){
    erf <- smooth_erf_locpol(matched_Y = m_Y,
                             matched_w = m_w,
                             matched_cw = counter_weight,
                             x_eval = w_vals,
                             bw = h_opt)
  } else if (kernel_appr == "kernsmooth"){
    erf <- smooth_erf_kernsmooth(matched_Y = m_Y,
                                 matched_w = m_w,
                                 matched_cw = counter_weight,
                                 x_eval = w_vals,
                                 bw = h_opt)
  } else {
    stop(paste("`kernel_appr`: ", kernel_appr, " is not supported."))
  }

  if (sum(is.na(erf)) > 0){
    logger::log_debug("erf has {sum(is.na(erf))} missing values.")
  }

  result <- list()
  class(result) <- "gpsm_erf"
  result$params$m_Y <- m_Y
  result$params$m_w <- m_w
  result$params$bw_seq <- bw_seq
  result$params$w_vals <- w_vals
  result$risk_val <- risk_val
  result$h_opt <- h_opt
  result$erf <- erf
  result$fcall <- fcall

  return(result)
}
