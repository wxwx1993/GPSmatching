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
autoplot.gpsm_erf <- function(object, ...){

  gg_labs <- gg_title <- NULL

  ## collect additional arguments
  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i,unlist(dot_args[i],use.names = FALSE))
  }

  if (is.null(gg_labs)){
    gg_labs <- c("Exposure Level", "Outcome Rate")
  }

  if (is.null(gg_title)){
    gg_title <- "Exposure Response Curve"
  }

  # extract data
  tmp_data <- data.frame(w.vals = object$params$w_vals,
                         erf = object$erf)

  g <- ggplot2::ggplot(tmp_data) +
       ggplot2::theme_bw() +
       ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
       ggplot2::geom_line(ggplot2::aes(.data$w.vals, .data$erf),
                          color="red", size=1)


  g <- g + ggplot2::labs(x=gg_labs[1], y=gg_labs[2])
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
plot.gpsm_erf <- function(x, ...){
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

  gg_labs <- gg_title <- NULL

  if (object$params$ci_appr == "matching"){
    default_gg_labs <- list(x="Absolute Correlation", y="Covariates")
  } else if (object$params$ci_appr == "weighting"){
    default_gg_labs <- list(x="Absolute Weighted Correlation", y="Covariates")
  }

  default_gg_title <- "Covariate Balance Test"

  ## collect additional arguments
  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i,unlist(dot_args[i],use.names = FALSE))
  }

  # create a data.frame from two data.
  balance <- data.frame(original = object$original_corr_results$absolute_corr,
                        adjusted = object$adjusted_corr_results$absolute_corr)

  # Convert row names in to attribute.
  balance$covar_label <- row.names(balance)

  # sort data.frame based on original data correlation values
  balance <- balance[order(balance$original),]
  covar_label <- balance$covar_label
  row.names(balance) <- NULL

  n_cov <- length(balance$original)
  m_balance <- melt(as.data.table(balance), measure.vars = c("original","adjusted"))
  m_balance$covariates <- rep(seq(1,n_cov,1),2)

  g <- ggplot2::ggplot(data = m_balance,
                       ggplot2::aes(x=.data$value,
                                    y=.data$covariates, color=.data$variable)) +
    ggplot2::geom_point() +
    ggplot2::geom_path() +
    ggplot2::scale_y_discrete(limit=factor(1:n_cov),labels = covar_label) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::geom_vline(xintercept = object$params$covar_bl_trs) +
    ggplot2::geom_vline(xintercept = object$adjusted_corr_results$mean_absolute_corr,
                        linetype="dotdash") +
    ggplot2::labs(x = default_gg_labs$x, y = default_gg_labs$y) +
    ggplot2::ggtitle(default_gg_title)

  if (!is.null(gg_labs)){
    g <- g + ggplot2::labs(x=gg_labs[1], y=gg_labs[2])
  }

  if (!is.null(gg_title)){
    g <- g + ggplot2::ggtitle(gg_title)
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
#'
#' @return
#' Returns a ggplot2 object, invisibly. This function is called for side effects.
#'
#' @export
#'
plot.gpsm_pspop <- function(x, ...){
  g <- ggplot2::autoplot(x, ...)
  print(g)
  invisible(g)
}
