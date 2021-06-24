test_that("train_it works as expected.", {

  x <- seq(0,1,0.001)
  y <- x^2 - x^3 + 4
  pred_model_b <- train_it(y, x, pred_model = "sl",
                         sl_lib=c("SL.xgboost","SL.earth","SL.gam"))

  mse_b <- sum((y-pred_model_b$SL.predict)^2)/length(y)
  expect_lt(mse_b, 0.00001)
})
