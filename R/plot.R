#' @title
#' A helper function for gpsm_erf object
#'
#' @description
#' A helper function to plot gpsm_erf object using ggplot2 package.
#'
#' @param object A gpsm_erf object.
#' @param ... Additional arguments passed to customize the plot.
#'
#' @return
#' Returns a ggplot object.
#'
#' @export
#'
#' @keywords internal
#' @importFrom ggplot2 autoplot
#' @importFrom rlang .data
#'
autoplot.gpsm_erf <- function(object, ...) {

  gg_labs <- gg_title <- NULL

  ## collect additional arguments
  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names) {
    assign(i, unlist(dot_args[i], use.names = FALSE))
  }

  if (is.null(gg_labs)) {
    gg_labs <- c("Exposure Level", "Outcome Rate")
  }

  if (is.null(gg_title)) {
    gg_title <- "Exposure Response Curve"
  }

  # extract data
  tmp_data <- data.frame(w.vals = object$params$w_vals,
                         erf = object$erf)

  g <- ggplot2::ggplot(tmp_data) +
       ggplot2::theme_bw() +
       ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
       ggplot2::geom_line(ggplot2::aes(.data$w.vals, .data$erf),
                          color = "red", size = 1)


  g <- g + ggplot2::labs(x = gg_labs[1], y = gg_labs[2])
  g <- g + ggplot2::ggtitle(gg_title)

  return(g)
}

#' @title
#' Extend generic plot functions for gpsm_erf class
#'
#' @description
#' A wrapper function to extend generic plot functions for gpsm_erf class.
#'
#' @param x  A gpsm_erf object.
#' @param ... Additional arguments passed to customize the plot.
#'
#' @return
#' Returns a ggplot2 object, invisibly. This function is called for side effects.
#'
#' @export
#'
plot.gpsm_erf <- function(x, ...) {
  g <- ggplot2::autoplot(x, ...)
  print(g)
  invisible(g)
}


#' @title
#' A helper function for gpsm_pspop object
#'
#' @description
#' A helper function to plot gpsm_pspop object using ggplot2 package.
#'
#' @param object A gpsm_pspop object.
#' @param ... Additional arguments passed to customize the plot.
#'
#' @return
#' Returns a ggplot object.
#'
#' @export
#'
#' @keywords internal
#' @importFrom ggplot2 autoplot
#' @importFrom rlang .data
#'
autoplot.gpsm_pspop <- function(object, ...){

  gg_labs <- gg_title <- include_details <- NULL

  if (object$params$ci_appr == "matching"){
    default_gg_labs <- list(x = "Absolute Correlation", y = "Covariates")
  } else if (object$params$ci_appr == "weighting"){
    default_gg_labs <- list(x = "Absolute Weighted Correlation", y="Covariates")
  }

  default_gg_title <- "Covariate Balance Test"

  ## collect additional arguments
  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i,unlist(dot_args[i], use.names = FALSE))
  }

  # create a data.frame from two data.
  balance <- data.frame(original = object$original_corr_results$absolute_corr,
                        adjusted = object$adjusted_corr_results$absolute_corr)

  # Convert row names in to attribute.
  balance$covar_label <- row.names(balance)

  # sort data.frame based on original data correlation values
  balance <- balance[order(balance$original), ]
  covar_label <- balance$covar_label
  row.names(balance) <- NULL

  n_cov <- length(balance$original)
  m_balance <- melt(as.data.table(balance), measure.vars = c("original",
                                                             "adjusted"))
  m_balance$covariates <- rep(seq(1, n_cov, 1), 2)

  g <- ggplot2::ggplot(data = m_balance,
                       ggplot2::aes(x=.data$value,
                                    y=.data$covariates,
                                    color=.data$variable,
                                    shape=.data$variable)) + # Add shape aesthetic
    ggplot2::geom_point() +
    ggplot2::geom_path() +
    ggplot2::scale_color_manual(values = c("original" = "blue", "adjusted" = "orange")) + # Define colors manually
    ggplot2::scale_shape_manual(values=c(16, 15)) +  # Differentiate shapes: 16 is a solid circle, 24 is a triangle
    ggplot2::scale_y_discrete(limit = factor(1:n_cov),labels = covar_label) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::geom_vline(xintercept = object$params$covar_bl_trs) +
    ggplot2::geom_vline(xintercept = object$adjusted_corr_results$mean_absolute_corr,
                        linetype="dotdash") +
    ggplot2::labs(x = default_gg_labs$x, y = default_gg_labs$y) +
    ggplot2::ggtitle(default_gg_title)

  if (!is.null(gg_labs)) {
    g <- g + ggplot2::labs(x=gg_labs[1], y=gg_labs[2])
  }

  if (!is.null(gg_title)) {
    g <- g + ggplot2::ggtitle(gg_title)
  }

  if (!is.null(include_details)){
    if (!is.logical(include_details)){
      stop(paste("include_details should be logical value.",
                 "Current: ", include_details))
      }
  } else {
    include_details <- FALSE
  }


  # legend position
  if (include_details) {
    g <- g + ggplot2::theme(legend.position = c(0.8, 0.1))
  }

  # Object details
  if (include_details){
    object_sum <- "Object Summary "
    object_sum <- paste0(object_sum, "\n", "--------------")
    object_sum <- paste0(object_sum, "\n", "Approach: ",
                         object$params$ci_appr)
    if (object$params$ci_appr == "matching"){
      object_sum <- paste0(object_sum, "\n", " scale: ",
                           round(object$params$scale, 3))
      object_sum <- paste0(object_sum, "\n", " delta: ",
                           round(object$params$delta, 3))
      object_sum <- paste0(object_sum, "\n", " dist measure: ",
                           object$params$dist_measure)
    }
    object_sum <- paste0(object_sum, "\n", "exposure trim qtls: ",
                         round(object$exposure_trim_qtls[1],3), ", ",
                         round(object$exposure_trim_qtls[2],3), ")")
    object_sum <- paste0(object_sum, "\n", "gps trim qtls: ",
                         "(",
                         round(object$gps_trim_qtls[1],3), ", ",
                         round(object$gps_trim_qtls[2],3), ")")
    object_sum <- paste0(object_sum, "\n", "# sample (trimmed/org): ",
                         nrow(object$pseudo_pop), "/", object$original_data_size)
    object_sum <- paste0(object_sum, "\n", "--------------")
    object_sum <- paste0(object_sum, "\n", "Covar balance method: ",
                         object$params$covar_bl_method)
    object_sum <- paste0(object_sum, "\n", " Threshhold type: ",
                         object$params$covar_bl_trs_type)
    object_sum <- paste0(object_sum, "\n", " Threshhold value: ",
                         round(object$params$covar_bl_trs, 3))
    object_sum <- paste0(object_sum, "\n", " Passed covariate test: ",
                         object$passed_covar_test)
    object_sum <- paste0(object_sum, "\n", " Maximal abs. cov.: ",
                         round(object$adjusted_corr_results$maximal_absolute_corr, 3))
    object_sum <- paste0(object_sum, "\n", " Median abs. cov.: ",
                         round(object$adjusted_corr_results$median_absolute_corr, 3))
    object_sum <- paste0(object_sum, "\n", " Mean abs. cov.: ",
                         round(object$adjusted_corr_results$mean_absolute_corr, 3))
    object_sum <- paste0(object_sum, "\n", " Number of attemtps: ",
                         object$counter, "/",
                         object$params$max_attempt)
    object_sum <- paste0(object_sum, "\n", " Use cov transform: ",
                         object$use_cov_transform)
    object_sum <- paste0(object_sum, "\n", "--------------")
    object_sum <- paste0(object_sum, "\n", "Kolmogorov Smirnov Test")
    object_sum <- paste0(object_sum, "\n", " ess: ", round(object$ess,3))
    object_sum <- paste0(object_sum, "\n", " ess (min recomennded):", round(object$ess_recommended,3))
    n_gps_param <- length(object$best_gps_used_params)

    if (n_gps_param > 0) {
      gps_params <- object$best_gps_used_params
      object_sum <- paste0(object_sum, "\n", "--------------")
      object_sum <- paste0(object_sum, "\n", "SL params")
      for (item in seq(1,n_gps_param)){
        object_sum <- paste0(object_sum, "\n  ", names(gps_params[item]),": ",
                             round(gps_params[[item]],5))
      }
    }

    text_grob <- cowplot::ggdraw() + cowplot::draw_label(object_sum,
                                                         fontface = 'plain',
                                                         hjust = 0,
                                                         vjust = 1,
                                                         x = 0.01,
                                                         y = 0.95,
                                                         size = 7,
                                                         lineheight = 1.2)
  }

  if (include_details){
    g <- cowplot::plot_grid(
      g, text_grob,
      ncol = 2,
      rel_widths = c(2, 1)
    )
  }

  return(g)
}

#' @title
#' Extend generic plot functions for gpsm_erf class
#'
#' @description
#' A wrapper function to extend generic plot functions for gpsm_erf class.
#'
#' @param x  A gpsm_erf object.
#' @param ... Additional arguments passed to customize the plot.
#' @details
#' ## Additional parameters
#' - *include_details*: If set to TRUE, the plot will include run details (Default = FALSE).
#'
#' @return
#' Returns a ggplot2 object, invisibly. This function is called for side effects.
#'
#' @export
#'
plot.gpsm_pspop <- function(x, ...) {
  g <- ggplot2::autoplot(x, ...)
  print(g)
  invisible(g)
}
