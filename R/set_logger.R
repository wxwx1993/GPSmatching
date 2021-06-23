#' @title
#' Set logger settings
#'
#' @description
#' Updates logger settings, including log level and location of the file.
#'
#' @param logger_file_path A path (including file name) to log the messages.
#' @param logger_level The log level. Available levels include:
#'   - TRACE
#'   - DEBUG
#'   - INFO
#'   - SUCESS
#'   - WARN
#'   - ERROR
#'   - FATAL
#'
#' @export
#'
#' @examples
#'
#' set_logger("Debug")
#'
set_logger <- function(logger_file_path=NULL, logger_level=NULL){

  available_levels <- c("TRACE", "DEBUG", "INFO", "SUCCESS", "WARN",
                        "ERROR", "FATAL")

  if (!is.null(logger_level)){
    if (is.element(logger_level, available_levels)){

      logger::log_threshold(logger_level)

    } else {
      stop(paste("Logger level is not valid. Available levels: ", paste(available_levels, collapse = " ")))
    }
  }

  if (!is.null(logger_file_path)){
    logger::log_appender(appender = logger::appender_file(logger_file_path),
                         index = 2)
  }
}
