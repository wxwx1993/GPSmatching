#' @title
#' Log system information
#'
#' @description
#' Logs system related information into the log file.
#'
#' @return
#' NULL
#'
#'@keywords internal
log_system_info <- function(){

  sys_info <- Sys.info()

  logger::log_info("System name: {sys_info[1]}, ",
                   "OS type: {.Platform$OS.type}, ",
                   "machine architecture: {sys_info[5]}, ",
                   "user: {sys_info[7]}, ",
                   "{R.version$version.string}, ",
                   "detected cores: {parallel::detectCores()[[1]]}")

}
