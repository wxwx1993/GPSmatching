#' @title
#' Create pseudo population using weighting casual inference approach
#'
#' @description
#' Generates pseudo population based on weighting casual inference method.
#'
#' @param dataset The study data set.
#' @param ... Additional parameters.
#'
#' @return
#' Returns a data table which includes the following columns:
#'  - Y
#'  - w
#'  - gps
#'  - counter
#'  - row_index
#'  - ipw
#'  - covariates
#'
#' @keywords internal
#'
create_weighting <- function(dataset, ...){

  # data set content: Y, w, gps, counter, row_index, c

  if (sum(!is.element(c("Y","w","gps","counter","row_index"),
                      colnames(dataset)))> 0){
    stop("Dataset does not include all required columns.")
  }

  Nm <- compute_density(dataset[["w"]], dataset[["w"]])
  ipw <- Nm / (dataset[["gps"]])
  return(data.table::data.table(cbind(dataset[,c("Y","w","gps",
                                                 "counter","row_index")],
                          ipw, dataset[,6:length(dataset)])))
}
