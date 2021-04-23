create_weighting <- function(dataset, ...){
  # dataset content: Y, w, gps, c

  Nm <- stats::dnorm(dataset[["w"]],
              mean = mean(dataset[["w"]], na.rm=TRUE),
              sd = stats::sd(dataset[["w"]], na.rm = TRUE))

  ipw <- Nm / (dataset[["gps"]])
  return(cbind(dataset[,c("Y","w","gps")],ipw,dataset[,4:length(dataset)]))
}