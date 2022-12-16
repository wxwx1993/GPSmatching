#' @title
#' Create pseudo population using matching casual inference approach
#'
#' @description
#' Generates pseudo population based on matching casual inference method.
#'
#' @param dataset A list with 6 elements. Including An original dataset as well
#'  as helper vectors from estimating GPS. See [compile_pseudo_pop()] for more
#'  details.
#' @param bin_seq Sequence of w (treatment) to generate pseudo population. If
#' NULL is passed the default value will be used, which is
#' `seq(min(w)+delta_n/2,max(w), by=delta_n)`.
#' @param gps_model Model type which is used for estimating GPS value, including
#' parametric (default) and non-parametric.
#' @param nthread Number of available cores.
#' @param optimized_compile Option to activate optimized compilation.
#' @param ...  Additional arguments passed to the function.
#'
#' @return
#' Returns data.table of matched set.
#'
#' @keywords internal
#'
create_matching <- function(dataset, bin_seq = NULL, gps_model = "parametric",
                            nthread = 1, optimized_compile, ...){

  # dataset content: dataset, e_gps_pred, e_gps_std_pred, w_resid, gps_mx, w_mx

  # Passing packaging check() ----------------------------
  delta_n <- NULL
  # ------------------------------------------------------

  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i,unlist(dot_args[i],use.names = FALSE))
  }

  matching_fun <- get(matching_fun)

  gps_mx <- dataset$gps_mx
  w_mx <- dataset$w_mx

  if (is.null(bin_seq)){

    bin_num<-seq(w_mx[1]+delta_n/2, w_mx[2], by = delta_n)
    used_bin <- "Default"

  } else {

    bin_num <- bin_seq
    used_bin <- "User defined"

  }

    logger::log_debug(used_bin, "bin seq is used. Min: {min(bin_num)} ",
                        "Max: {max(bin_num)}, count: {length(bin_num)}.")

    logger::log_debug("Started generating matched set ...")
    st_t_m <- proc.time()

    matched_set <-  lapply(bin_num,
                           matching_fun,
                           dataset=dataset$dataset,
                           e_gps_pred = dataset$e_gps_pred,
                           e_gps_std_pred = dataset$e_gps_std_pred,
                           w_resid=dataset$w_resid,
                           gps_mx = gps_mx,
                           w_mx = w_mx,
                           gps_model = gps_model,
                           delta_n = delta_n,
                           scale = scale,
                           nthread = nthread,
                           optimized_compile = optimized_compile)

  e_t_m <- proc.time()
  logger::log_debug("Finished generating matched set (Wall clock time:  ",
                    " {(e_t_m - st_t_m)[[3]]} seconds).")

  if (!optimized_compile){
    return(data.table(do.call(rbind,matched_set)))
  } else {

    logger::log_debug("Started working on compiling  ... ")

    s_comp_p <- proc.time()

    cp_original_data <- dataset[[1]]

    # create initial freq_table
    logger::log_debug("Started working on merging the frequency table  ... ")
    s_bindlist <- proc.time()
    N <- N.x <- N.y <- row_index <- NULL
    freq_table <- data.table(row_index=numeric(), N=integer())
    for (i in seq(1, length(matched_set))){

      if (length(matched_set[[i]]) == 0){
        # bins that does not have any match.
        next
      }
      freq_table <- merge(freq_table, matched_set[[i]],
                          by="row_index",
                          all=TRUE)
      row.names(freq_table) <- NULL
      freq_table[is.na(freq_table)] <- 0
      freq_table[, N:= N.x + N.y]
      freq_table[, N.x:= NULL]
      freq_table[, N.y:= NULL]
    }
    e_bindlist <- proc.time()
    logger::log_debug(paste0("Finished binding the frequency table ",
                             "(Wall clock time:  ",
                             (e_bindlist - s_bindlist)[[3]]," seconds)."))

    if (nrow(freq_table) != 0){
      index_of_data <- freq_table[["row_index"]]
      added_count <- freq_table[["N"]]
      counter_tmp <- numeric(nrow(cp_original_data))
      counter_tmp[index_of_data] <- added_count
      cp_original_data$counter_weight <- counter_tmp
    }

    e_comp_p <- proc.time()

    logger::log_debug("Finished compiling (vectorized) (Wall clock time:  ",
                      " {(e_comp_p - s_comp_p)[[3]]} seconds).")

    return(data.table(cp_original_data))
  }
}
