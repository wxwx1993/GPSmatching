#' @title
#' Estimate semi exposure-response function (semi-ERF).
#'
#' @description
#' TODO
#'
#' @param Y a vector of outcome variable in matched set.
#' @param w a vector of continuous exposure variable in matched set, which is
#' used for smoothing.
#' @param z data.frame of predictors
#' @param family a description of the error distribution (see ?gam)
#'
#' @return
#' returns an object of class Gam
#'
#' @export
#'
estimate_semi_erf<-function(formula, family){

#
#     if (!is.data.frame(z)){
#       stop("Predictors should be data.frame.")
#     }
#
#     col_n <- col_f <- NULL
#
#     # detect numeric columns
#     col_n <- colnames(z)[unlist(lapply(z, is.numeric))]
#
#     # detect factorial columns
#     col_f <- colnames(z)[unlist(lapply(z, is.factor))]
#
#     if (is.null(col_f)){
#       stop("At least one of the predictors (z) should be categorical.")
#     }
#
#     if (length(col_n)>0){
#       warning(paste("The following numerical predictors were ignored:",
#                     paste(col_n, collapse = ", ")))
#     }
#
#     fmla <- as.formula(paste("Y ~ ", paste(c('s(w)',col_f), collapse= "+")))

    gam_model <- gam::gam(formula = formula, family = family)

  return(gam_model)
}
