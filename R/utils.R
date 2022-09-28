#' @title
#' Log system information
#'
#' @description
#' Logs system related information into the log file.
#'
#' @return
#' No return value. This function is called for side effects.
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

# Keeping logger options
my_options <- new.env(parent = emptyenv())

get_options <- function(k, v){
  my_options[[k]]
}

set_options <- function(k, v){
  my_options[[k]] <- v
}

list_options <- function(){
  names(my_options)
}
