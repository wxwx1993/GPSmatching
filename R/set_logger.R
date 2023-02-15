#' @title
#' Set Logger Settings
#'
#' @description
#' Updates logger settings, including log level and location of the file.
#'
#' @param logger_file_path A path (including file name) to log the messages.
#' (Default: CausalGPS.log)
#' @param logger_level The log level. Available levels include:
#'   - TRACE
#'   - DEBUG
#'   - INFO (Default)
#'   - SUCCESS
#'   - WARN
#'   - ERROR
#'   - FATAL
#'
#' @export
#'
#' @return
#' No return value. This function is called for side effects.
#'
#' @examples
#'
#' set_logger("Debug")
#'
set_logger <- function(logger_file_path = "CausalGPS.log",
                       logger_level = "INFO") {

  available_levels <- c("TRACE", "DEBUG", "INFO", "SUCCESS", "WARN",
                        "ERROR", "FATAL")

  if (!is.element(logger_level, available_levels)) {
    stop(paste("logger_level: ", logger_level, " is not valid."))
  }

  #assign("logger_file_path", logger_file_path, envir = log_env)

 logger::log_appender(appender = logger::appender_file(logger_file_path),
                       index = 1)

  set_options("logger_file_path", logger_file_path)
  set_options("logger_level", logger_level)

  #create_matching$logger_file_path <- logger_file_path

  if (!is.null(logger_level)) {
    if (is.element(logger_level, available_levels)) {

      logger::log_threshold(logger_level)

    } else {
      stop(paste("Logger level is not valid. Available levels: ",
                 paste(available_levels, collapse = " ")))
    }
  } else {
    logger::log_threshold(logger::INFO, index = 1)
  }
}

#' @title
#' Get Logger Settings
#'
#' @description
#' Returns current logger settings.
#'
#'
#' @return
#' Returns a list that includes **logger_file_path** and **logger_level**.
#'
#' @export
#'
#' @examples
#'
#' set_logger("mylogger.log", "INFO")
#' log_meta <- get_logger()
#'
get_logger <- function(){

  return(list(logger_file_path = get_options("logger_file_path"),
              logger_level = get_options("logger_level")))
}


