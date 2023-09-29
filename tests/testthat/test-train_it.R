test_that("train_it works as expected.", {

  skip_on_cran()
  data.table::setDTthreads(1)
  x <- seq(0,1,0.001)
  y <- x^2 - x^3 + 4
  pred_model_b <- train_it(y, x,
                           sl_lib=c("SL.xgboost"))

  mse_b <- sum((y-pred_model_b$SL.predict)^2)/length(y)
  expect_lt(mse_b, 0.00001)
})


test_that("train_it works as expected (with earth).", {

  skip_if_not_installed("earth")
  skip_on_cran()
  data.table::setDTthreads(1)

  x <- seq(0,1,0.001)
  y <- x^2 - x^3 + 4
  pred_model_b <- train_it(y, x,
                           sl_lib=c("SL.xgboost","SL.earth","SL.gam"))

  mse_b <- sum((y-pred_model_b$SL.predict)^2)/length(y)
  expect_lt(mse_b, 0.00001)
})

