.onLoad <- function(libname, pkgname){

  flogger <- logger::layout_glue_generator(format =
                                             paste('{time} {node} {pid} ',
                                                   '{namespace} {fn} ',
                                                   '{level}:  {msg}',
                                                   sep = ""))

  logger::log_layout(flogger, index = 1)
  # RNGkind(kind = "L'Ecuyer-CMRG")

}
