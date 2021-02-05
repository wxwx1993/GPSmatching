#' @title
#' Generate synthetic data for GPSmatching package
#'
#' @description
#' Generates synthetic data based on ... (TODO)
#'
#' @param sample.size sample size description (TODO)
#' @param seed seed description (TODO)
#' @param outcome.sd outcpme_sd description (TODO)
#' @param gps.spec gps_spec description (TODO)
#' @param cova.spec cova_spec description (TODO)
#'
#' @return
#' \code{synthetic_data}: The function returns a data.frame saved the
#'  constructed synthetic data.
#' @export
#'
#' @importFrom stats approx density  rnorm rt  runif
#'
#' @examples
#' s_data <- GenSynData(sample.size=100, seed = 403,
#'                                   outcome.sd = 10, gps.spec = 1,
#'                                   cova.spec = 1)
#'
GenSynData <- function(sample.size=1000, seed = 300, outcome.sd = 10,
                       gps.spec = 1, cova.spec = 1) {

  if (sample.size < 0 || !is.numeric(sample.size)){
    stop("'sample_size' should be a positive ineteger numer.")
   }

  #TODO: Check other input arguments.

  #options(digits=4) # only print 4 sig digits
  set.seed(seed)
  size <- sample.size

  #pre-treatment variables (confounders)
  cf  <- MASS::mvrnorm(n = size,
                       mu = c(0,0,0,0),
                       Sigma = matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),
                       ncol=4))

  cf5 <- sample(c((-2):2), size, replace = TRUE)
  cf6 <- runif(size, min=-3, max=3)

  if (gps.spec == 1) {

    treat <- ((- 0.8 + 0.1 * cf[ ,1] + 0.1 * cf[ ,2] - 0.1 * cf[ ,3]
               + 0.2 * cf[ ,4] + 0.1 * cf5 + 0.1 * cf6) * 9
               + 17  + rnorm(size,sd=5))

  } else if (gps.spec == 2) {

    treat <- ((- 0.8 + 0.1 * cf[ ,1] + 0.1 * cf[ ,2] - 0.1 * cf[ ,3]
              + 0.2 * cf[ ,4] + 0.1 * cf5 + 0.1 * cf6) * 15 + 22 + rt(size,2))

    treat[which(treat < (-5))] <- (-5)
    treat[which(treat > (25))] <- (25)

  } else if (gps.spec == 3) {

    treat <- ((- 0.8 + 0.1 * cf[ , 1] + 0.1 * cf[ , 2]- 0.1 *cf[ ,3] + 0.2 * cf [ , 4]
               + 0.1 * cf5 + 0.1 * cf6) * 9
               + 1.5 * cf[ , 3] ^ 2 + rnorm(size, mean = 0, 5) + 15)

  } else if (gps.spec == 4) {

    treat <- (49 * exp((-0.8 + 0.1 * cf[ ,1] + 0.1 * cf[ , 2] - 0.1 * cf[ , 3]
            + 0.2 * cf[ , 4] + 0.1 * cf5 + 0.1 * cf6))
            / (1 + exp((-0.8 + 0.1 * cf[,1] + 0.1 * cf[ , 2] - 0.1 * cf[ , 3]
            + 0.2 * cf[ , 4] + 0.1 * cf5 + 0.1 * cf6))) - 6 + rnorm(size, sd=5))

  } else if (gps.spec == 5) {

    treat <- (42 / (1 + exp((-0.8 + 0.1 * cf[ , 1] + 0.1 * cf[ , 2]- 0.1 * cf[ , 3]
           + 0.2 * cf[,4] + 0.1 * cf5 + 0.1 * cf6))) - 18 + rnorm(size,sd=5))

  } else if (gps.spec == 6) {

    treat <- (log(abs(-0.8 + 0.1 * cf[ , 1] + 0.1 * cf[ , 2] - 0.1 * cf[ , 3]
             + 0.2 * cf[ , 4] + 0.1 * cf5 + 0.1 * cf6)) * 7 + 13 + rnorm(size,sd=4))

  } else if (gps.spec == 7) {

    treat <- ((-0.8 + 0.1 * cf[,1] + 0.1 * cf[,2] - 0.1 * cf[,3] + 0.2 * cf[,4]
             + 0.1 * cf5 + 0.1 * cf6) * 15 + 22 + rt(size,2)) #+ rcauchy(size)
  }

  #produce outcome Y
  Y <- as.numeric()

  for (i in 1:size) {
    Y[i] <- ((-(1 + (sum(c(0.2, 0.2, 0.3, -0.1) * cf[i, ]))
                  * 10 - 2 * cf5[i] - 2*cf6[i] + (treat[i]-20)
                  * (0.1 + 0.1 * cf[i,4] + 0.1 * cf5[i]
                  + 0.1 * cf[i,3] ^ 2 - 0.13 ^ 2 * (treat[i] - 20) ^2)))
                  + stats::rnorm(1, mean=0, sd=outcome.sd))
  }
  if (cova.spec == 1) {

    cf = cf

  } else if (cova.spec == 2) {

    cf[,1] <- exp(cf[ ,1] / 2)
    cf[,2] <- (cf[ ,2] / (1 + exp(cf[ ,1]))) + 10
    cf[,3] <- (cf[ ,1] * cf[ ,3]/25 + 0.6) ^ 3
    cf[,4] <- (cf[ ,2] + cf[ ,4] + 20) ^ 2

  }

  simulated.data<-data.frame(cbind(Y,treat,cf, cf5, cf6))
  colnames(simulated.data)[3:8]<-c("cf1","cf2","cf3","cf4","cf5","cf6")
  return(simulated.data)
}
