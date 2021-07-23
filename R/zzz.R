
# .onLoad <- function(libname, pkgname){
#
#   flogger <- logger::layout_glue_generator(format =
#                                            paste('{time} {node} {pid} ',
#                                                  '{namespace} {fn} ',
#                                                  '{level}:  {msg}',
#                                                  sep = ""))
#   logger::log_appender(appender = logger::appender_file("CausalGPS.log"),
#                        index = 1)
#   logger::log_threshold(logger::INFO,index = 1)
#   logger::log_layout(flogger, index = 1)
#
# }

.onLoad <- function(libname, pkgname){

  flogger <- logger::layout_glue_generator(format =
                                             paste('{time} {node} {pid} ',
                                                   '{namespace} {fn} ',
                                                   '{level}:  {msg}',
                                                   sep = ""))

  logger::log_layout(flogger, index = 1)

}
