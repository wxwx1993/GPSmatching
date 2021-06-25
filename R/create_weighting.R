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
#'  - covaraites
#' @export
#'
create_weighting <- function(dataset, ...){
  # dataset content: Y, w, gps, counter, row_index, c

  Nm <- stats::dnorm(dataset[["w"]],
              mean = mean(dataset[["w"]], na.rm=TRUE),
              sd = stats::sd(dataset[["w"]], na.rm = TRUE))

  ipw <- Nm / (dataset[["gps"]])

  return(data.table(cbind(dataset[,c("Y","w","gps")],
                          ipw, dataset[,6:length(dataset)])))
}
