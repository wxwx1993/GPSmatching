#' @title
#' Create pseudo population using matching casual inference approach
#'
#' @description
#' Generates pseudo population based on matching casual inference method.
#'
#' @param dataset A list with 6 elements. Including An original dataset as well
#'  as helper vectors from estimating GPS. See [CompilePseudoPop()] for more
#'  details.
#' @param ...  Additional arguments passed to the function.
#'
#' @return
#' Returns data.table of matched set.
#' @export
#'
CreateMatching <- function(dataset, ...){

  # dataset content: dataset, e_gps_pred, e_gps_std_pred, w_resid, gps_mx, w_mx

  # Passing packaging check() ----------------------------
  delta.n <- NULL
  # ------------------------------------------------------

  dot.args <- list(...)
  arg.names <- names(dot.args)

  for (i in arg.names){
    assign(i,unlist(dot.args[i],use.names = FALSE))
  }

  matching.fun <- get(matching.fun)

  gps.mx <- dataset[[5]]
  w.mx <- dataset[[6]]

  bin.num<-seq(w.mx[1]+delta.n/2, w.mx[2], by = delta.n)

  matched.set <-  lapply(bin.num,
                         matching.fun,
                         dataset=dataset[[1]],
                         e_gps_pred = dataset[[2]],
                         e_gps_std_pred = dataset[[3]],
                         w_resid=dataset[[4]],
                         gps_mx = gps.mx,
                         w_mx = w.mx,
                         delta.n = delta.n,
                         scale = scale)

  return(data.table(Reduce(rbind,matched.set)))
}
