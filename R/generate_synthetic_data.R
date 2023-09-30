#' @title
#' Generate synthetic data for the CausalGPS package
#'
#' @description
#' Generates synthetic data set based on different GPS models and covariates.
#'
#' @param sample_size A positive integer number that represents a number of data
#' samples.
#' @param outcome_sd A positive double number that represents standard deviation
#' used to generate the outcome in the synthetic data set.
#' @param gps_spec A numerical integer values ranging from 1 to 7. The
#' complexity and form of the relationship between covariates and treatment
#' variables are determined by the `gps_spec`. Below, you will find a concise
#' definition for each of these values:
#'   - *gps_spec: 1*: The treatment is generated using a normal distributionMay 24, 2023
#'   (`stats::rnorm`) and a linear function of covariates (cf1 to cf6).
#'   - *gps_spec: 2*: The treatment is generated using a Student's
#'   t-distribution (`stats::rt`) and a linear function of covariates, but is
#'   also truncated to be within a specific range (-5 to 25).
#'   - *gps_spec: 3*: The treatment includes a quadratic term for the third
#'   covariate.
#'   - *gps_spec: 4*: The treatment is calculated using an exponential
#'   function within a fraction, creating logistic-like model.
#'   - *gps_spec: 5*: The treatment also uses logistic-like model but with
#'   different parameters.
#'   - *gps_spec: 6*: The treatment is calculated using the natural logarithm
#'   of the absolute value of a linear combination of the covariates.
#'   - *gps_spec: 7*: The treatment is generated similarly to `gps_spec = 2`,
#'   but without truncation.

#' @param cova_spec A numerical value (1 or 2) to modify the covariates. It
#' determines how the covariates in the synthetic data set are transformed.
#' If `cova_spec` equals 2, the function applies non-linear transformation to
#' the covariates, which can add complexity to the relationships between
#' covariates and outcomes in the synthetic data. See the code for more details.
#'
#' @param vectorized_y A Boolean value indicates how Y internally is generated.
#' (Default = `FALSE`). This parameter is introduced for backward compatibility.
#' vectorized_y = `TRUE` performs better.
#'
#'
#' @return
#' \code{synthetic_data}: The function returns a data.frame saved the
#'  constructed synthetic data.
#'
#' @export
#'
#' @examples
#'
#' set.seed(298)
#' s_data <- generate_syn_data(sample_size = 100,
#'                             outcome_sd = 10,
#'                             gps_spec = 1,
#'                             cova_spec = 1)
#'
generate_syn_data <- function(sample_size = 1000,
                              outcome_sd = 10,
                              gps_spec = 1,
                              cova_spec = 1,
                              vectorized_y = FALSE) {

  if (sample_size < 0 || !is.numeric(sample_size)) {
    stop(paste("'sample_size' should be a positive ineteger number.",
               " You entered: ", sample_size))
  }

  if (outcome_sd < 0){
    stop(paste("'outcome_sd' should be a positive double number.",
               " You entered:", outcome_sd))
  }

  if(!gps_spec %in% 1:7){
    stop(paste("gps_spec: ", gps_spec, ", is not a valid value."))
  }

  if(!cova_spec %in% 1:2){
    stop(paste("cova_spec: ", cova_spec, ", is not a valid value."))
  }

  size <- sample_size

  #pre-treatment variables (confounders)
  cf  <- MASS::mvrnorm(n = size,
                       mu = c(0,0,0,0),
                       Sigma = matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),
                       ncol=4))

  cf5 <- sample(c((-2):2), size, replace = TRUE)
  cf6 <- stats::runif(size, min=-3, max=3)

  tmp_treat <- (- 0.8 + 0.1 * cf[, 1]
                      + 0.1 * cf[, 2]
                      - 0.1 * cf[, 3]
                      + 0.2 * cf[, 4]
                      + 0.1 * cf5
                      + 0.1 * cf6)

  if (gps_spec == 1) {

    treat <- (tmp_treat * 9 + 17  + stats::rnorm(size,sd=5))

  } else if (gps_spec == 2) {

    treat <- (tmp_treat * 15 + 22 + stats::rt(size,2))
    treat[which(treat < (-5))] <- (-5)
    treat[which(treat > (25))] <- (25)

  } else if (gps_spec == 3) {

    treat <- (tmp_treat * 9 + 1.5 * cf[ , 3] ^ 2 +
              stats::rnorm(size, mean = 0, 5) + 15)

  } else if (gps_spec == 4) {

    treat <- (49 * exp(tmp_treat)
            / (1 + exp(tmp_treat)) - 6 + stats::rnorm(size, sd=5))
  } else if (gps_spec == 5) {

    treat <- (42 / (1 + exp(tmp_treat)) - 18 + stats::rnorm(size,sd=5))

  } else if (gps_spec == 6) {

    treat <- (log(abs(tmp_treat)) * 7 + 13 + stats::rnorm(size,sd=4))

  } else if (gps_spec == 7) {

    treat <- (tmp_treat * 15 + 22 + stats::rt(size,2))
  } else {

    stop(paste("gps_spec: ", gps_spec, ", is not a valid value."))
  }


  if (vectorized_y){
    # Pre-calculate some parts of the formula
    cf_sum <- rowSums(cf * c(0.2, 0.2, 0.3, -0.1))
    cf_part <- 0.1 + 0.1 * cf[,4] + 0.1 * cf5 + 0.1 * cf[,3] ^ 2

    # Pre-generate a vector of random numbers
    rand_norm <- stats::rnorm(size, mean = 0, sd = outcome_sd)

    # Calculate Y
    Y <- -((1 + cf_sum * 10 - 2 * cf5 - 2 * cf6 + (treat - 20) * cf_part
            - 0.13 ^ 2 * (treat - 20) ^ 2)) +
      rand_norm
  } else {
    # TODO: This part is kept for backward compatibility.
    Y <- as.numeric()

    # non-vectorized Y
    for (i in 1:size) {
      Y[i] <- ((-(1 + (sum(c(0.2, 0.2, 0.3, -0.1) * cf[i, ]))
                  * 10 - 2 * cf5[i] - 2 * cf6[i] + (treat[i] - 20)
                  * (0.1 + 0.1 * cf[i,4] + 0.1 * cf5[i]
                     + 0.1 * cf[i,3] ^ 2 - 0.13 ^ 2 * (treat[i] - 20) ^ 2)))
               + stats::rnorm(1, mean = 0, sd = outcome_sd))
    }
  }

  if (cova_spec == 1) {

    cf <- cf

  } else if (cova_spec == 2) {

    cf[,1] <- exp(cf[ ,1] / 2)
    cf[,2] <- (cf[ ,2] / (1 + exp(cf[ ,1]))) + 10
    cf[,3] <- (cf[ ,1] * cf[ ,3]/25 + 0.6) ^ 3
    cf[,4] <- (cf[ ,2] + cf[ ,4] + 20) ^ 2

  } else {
    stop(paste("cova_spec: ", cova_spec, ", is not a valid value."))
  }
  w <- treat
  simulated_data <- data.frame(cbind(Y, w, cf, cf5, cf6))
  colnames(simulated_data)[3:8]<-c("cf1", "cf2", "cf3", "cf4", "cf5", "cf6")
  simulated_data$id <- seq_along(1:nrow(simulated_data))
  return(simulated_data)
}


