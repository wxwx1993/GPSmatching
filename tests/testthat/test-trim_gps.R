test_that("trim_gps works as expected.", {

  skip_on_cran()
  set.seed(422)
  n <- 500
  data.table::setDTthreads(1)
  mydata <- generate_syn_data(sample_size = n)
  year <- sample(x=c("2001", "2002", "2003", "2004", "2005"), size = n,
                 replace = TRUE)
  region <- sample(x=c("North", "South", "East", "West"),size = n,
                   replace = TRUE)
  mydata$year <- as.factor(year)
  mydata$region <- as.factor(region)
  mydata$cf5 <- as.factor(mydata$cf5)

  gps_object <- estimate_gps(mydata[, c("id", "w")],
                             mydata[, c("id", "cf1", "cf2", "cf3",
                                        "cf4", "cf5", "cf6")],
                             params = list(xgb_max_depth = c(3,4,5),
                                           xgb_rounds = c(10,20,30,40)),
                             gps_density = "normal",
                             nthread = 1,
                             sl_lib = c("m_xgboost"))


  trim_quntiles <- c(0.05, 0.8)

  trimmed_gps_object <- trim_gps(gps_obj = gps_object,
                                 trim_quantiles = trim_quntiles)

  expect_equal(nrow(gps_object$dataset), 500)
  expect_equal(nrow(trimmed_gps_object$dataset), 375)
})
