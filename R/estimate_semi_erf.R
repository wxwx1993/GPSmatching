#' @title
#' Estimate semi exposure-response function (semi-ERF).
#'
#' @description
#' TODO
#'
#' @param formula a vector of outcome variable in matched set.
#' @param family a description of the error distribution (see ?gam)
#' @param data dataset that formula is build upon
#'
#' @return
#' returns an object of class Gam
#'
#' @export
#'
estimate_semi_erf<-function(formula, family, data){


    # if (!is.data.frame(z)){
    #   stop("Predictors should be data.frame.")
    # }
    #
    # col_n <- col_f <- NULL
    #
    # # detect numeric columns
    # col_n <- colnames(z)[unlist(lapply(z, is.numeric))]
    #
    # # detect factorial columns
    # col_f <- colnames(z)[unlist(lapply(z, is.factor))]
    #
    # if (is.null(col_f)){
    #   stop("At least one of the predictors (z) should be categorical.")
    # }
    #
    # if (length(col_n)>0){
    #   warning(paste("The following numerical predictors were ignored:",
    #                 paste(col_n, collapse = ", ")))
    # }
    #
    # for (item in col_f){
    #   assign(item, data[item])
    # }
    #
    # fmla <- as.formula(paste("Y ~ ", paste(c('s(w)',col_f), collapse= "+")))
    #
    # gam_model <- gam::gam(fmla, family = family)

    gam_model <- gam::gam(formula = formula, family = family, data = data)

  return(gam_model)
}
