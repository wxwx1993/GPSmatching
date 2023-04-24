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
create_weighting <- function(dataset, exposure_col_name, ...){

  if (sum(!is.element(c(exposure_col_name, "gps", "counter_weight", "id"),
                      colnames(dataset))) > 0) {
    stop("Dataset does not include all required columns.")
  }

  Nm <- compute_density(dataset[[exposure_col_name]],
                        dataset[[exposure_col_name]])
  ipw <- Nm / (dataset[["gps"]])
  dataset$counter_weight <- ipw
  return(data.table::data.table(dataset))
}
