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
#'
autoplot.gpsm_erf <- function(object, ...){

  gg_labs <- NULL
  gg_title <- "Exposure Rate Function"

  ## collect additional arguments
  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i,unlist(dot_args[i],use.names = FALSE))
  }

  # extract data
  tmp_data <- data.frame(w.vals = object$params$w_vals,
                         erf = object$erf)

  g <- ggplot2::ggplot(tmp_data) +
       ggplot2::theme_bw() +
       ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
       ggplot2::geom_line(ggplot2::aes(.data$w.vals,.data$erf),
                          color="red", size=1)

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
#' @export
#'
plot.gpsm_erf <- function(x, ...){
  g <- ggplot2::autoplot(x, ...)
  print(g)
  invisible(g)
}
