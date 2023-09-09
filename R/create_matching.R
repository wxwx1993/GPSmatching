#' @title
#' Create pseudo population using matching casual inference approach
#'
#' @description
#' Generates pseudo population based on matching casual inference method.
#'
#' @param data_obj A list of elements. Including An original dataset as well
#'  as helper vectors from estimating GPS. See [compile_pseudo_pop()] for more
#'  details.
#' @param bin_seq Sequence of w (treatment) to generate pseudo population. If
#' NULL is passed the default value will be used, which is
#' `seq(min(w)+delta_n/2,max(w), by=delta_n)`.
#' @param gps_density Model type which is used for estimating GPS value, including
#' `normal` (default) and `kernel`.
#' @param nthread Number of available cores.
#' @param ...  Additional arguments passed to the function.
#'
#' @return
#' Returns data.table of matched set.
#'
#' @keywords internal
#'
create_matching <- function(data_obj, exposure_col_name, bin_seq = NULL,
                            gps_density = "normal",
                            nthread = 1, ...) {

  # Passing packaging check() ----------------------------
  delta_n <- NULL
  counter_weight <- NULL
  i.counter_weight <- NULL
  dist_measure <- NULL
  # ------------------------------------------------------

  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names) {
    assign(i, unlist(dot_args[i], use.names = FALSE))
  }

  #matching_fun <- get(matching_fun)

  gps_mx <- data_obj$gps_mx
  w_mx <- data_obj$w_mx


    if (is.null(bin_seq)) {

    bin_num<-seq(w_mx[1] + delta_n / 2, w_mx[2], by = delta_n)
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
                           matching_fn,
                           dataset=data_obj$dataset,
                           exposure_col_name = exposure_col_name,
                           e_gps_pred = data_obj$dataset$e_gps_pred,
                           e_gps_std_pred = data_obj$dataset$e_gps_std_pred,
                           w_resid=data_obj$dataset$w_resid,
                           gps_mx = gps_mx,
                           w_mx = w_mx,
                           dist_measure = dist_measure,
                           gps_density = gps_density,
                           delta_n = delta_n,
                           scale = scale,
                           nthread = nthread)

  e_t_m <- proc.time()
  logger::log_debug("Finished generating matched set (Wall clock time:  ",
                    " {(e_t_m - st_t_m)[[3]]} seconds).")

    logger::log_debug("Started working on compiling  ... ")

    s_comp_p <- proc.time()

    cp_original_data <- data_obj$dataset

    # create initial freq_table
    logger::log_debug("Started working on merging the frequency table  ... ")
    s_bindlist <- proc.time()
    N <- N.x <- N.y <- id <- NULL
    freq_table <- data.table(id=numeric(), N=integer())
    for (i in seq(1, length(matched_set))){

      if (length(matched_set[[i]]) == 0){
        # bins that does not have any match.
        next
      }
      freq_table <- merge(freq_table, matched_set[[i]],
                          by = "id",
                          all = TRUE)
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

    cp_original_data["counter_weight"] <- rep(0, nrow(cp_original_data))

    if (nrow(freq_table) != 0) {
      c_w <- cp_original_data[, c("id", "counter_weight")]
      data.table::setDT(c_w)
      merged_dt <- merge(c_w, freq_table, by="id", all.x = TRUE)
      merged_dt[is.na(N), N := 0]
      merged_dt[, counter_weight := counter_weight + N]
      c_w[merged_dt, counter_weight := i.counter_weight, on = "id"]
      data.table::setDF(c_w)
      cp_original_data$counter_weight <- NULL
      cp_original_data <- merge(cp_original_data, c_w, by = "id")
    }

    e_comp_p <- proc.time()

    logger::log_debug("Finished compiling (vectorized) (Wall clock time:  ",
                      " {(e_comp_p - s_comp_p)[[3]]} seconds).")

    return(data.table(cp_original_data))
}
